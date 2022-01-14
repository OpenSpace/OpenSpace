#include "sgctedit/filesupport.h"

FileSupport::FileSupport(QVBoxLayout* parentLayout, std::vector<QRect>& monitorList,
                         std::vector<Display*>& displays, Orientation* orientation,
                         std::vector<sgct::config::Window>& windowList,
                         sgct::config::Cluster& cluster, std::function<void(bool)> cb)
    : _monitors(monitorList)
    , _displayWidgets(displays)
    , _orientationWidget(orientation)
    , _cluster(cluster)
    , _windowList(windowList)
    , _finishedCallback(cb)
{
    QVBoxLayout* layoutFullVertical = new QVBoxLayout();
    QHBoxLayout* layoutFilename = new QHBoxLayout();
    _lineFilename = new QLineEdit();
    _lineFilename->setFixedWidth(190);
    QLabel* labelFilename = new QLabel();
    labelFilename->setText("Filename: ");
    layoutFilename->addStretch(1);
    layoutFilename->addWidget(labelFilename);
    layoutFilename->addWidget(_lineFilename);
    layoutFilename->addStretch(1);

    _layoutButtonBox = new QHBoxLayout;
    _saveButton = new QPushButton("Save");
    _saveButton->setToolTip("Save global orientation changes");
    _layoutButtonBox->addStretch(1);
    _layoutButtonBox->addWidget(_saveButton);

    _cancelButton = new QPushButton("Cancel");
    _cancelButton->setToolTip("Cancel global orientation changes");
    _layoutButtonBox->addWidget(_cancelButton);
    layoutFullVertical->addLayout(layoutFilename);
    layoutFullVertical->addLayout(_layoutButtonBox);
    parentLayout->addLayout(layoutFullVertical);
    connect(_saveButton, SIGNAL(released()), this, SLOT(save()));
    connect(_cancelButton, SIGNAL(released()), this, SLOT(cancel()));
    connect(_lineFilename, SIGNAL(textEdited(const QString&)), this,
        SLOT(filenameEdited(const QString&)));
    _saveButton->setEnabled(false);
    _cluster.masterAddress = "127.0.0.1";
}

void FileSupport::saveCluster() {
    if (_orientationWidget) {
        sgct::config::Scene initScene;
        initScene.orientation = _orientationWidget->orientationValue();
        _cluster.scene = std::move(initScene);
        _cluster.firmSync = _orientationWidget->vsyncValue();
    }
}

bool FileSupport::isWindowFullscreen(sgct::ivec2 mDims, sgct::ivec2 wDims) {
    return ((mDims.x == wDims.x) && (mDims.y == wDims.y));
}

int FileSupport::findGuiWindow() {
    unsigned int windowIndex = 0;
    for (unsigned int m = 0; m < _displayWidgets.size(); ++m) {
        if (_displayWidgets[m]) {
            for (unsigned int w = 0; w < _displayWidgets[m]->nWindows(); ++w) {
                if (_displayWidgets[m]->windowControls()[w]->isGuiWindow()) {
                    return windowIndex;
                }
                windowIndex++;
            }
        }
    }
    return -1;
}

void FileSupport::saveWindows() {
    int webGuiWindowIndex = findGuiWindow();
    bool isOneOfWindowsSetAsWebGui = (webGuiWindowIndex >= 0);

    unsigned int windowIndex = 0;
    for (unsigned int m = 0; m < _displayWidgets.size(); ++m) {
        if (_displayWidgets[m]) {
            for (unsigned int w = 0; w < _displayWidgets[m]->nWindows(); ++w) {
                _windowList.push_back(sgct::config::Window());
                _windowList.back().viewports.push_back(sgct::config::Viewport());
                _windowList.back().viewports.back().isTracked = true;
                _windowList.back().viewports.back().position = {0.0, 0.0};
                _windowList.back().viewports.back().size = {1.0, 1.0};
                int projectionIdx
                    = _displayWidgets[m]->windowControls()[w]->projectionSelectedIndex();
                bool isSpoutSelected
                    = _displayWidgets[m]->windowControls()[w]->isSpoutSelected();

                saveProjectionInformation(
                    isSpoutSelected,
                    projectionIdx,
                    _displayWidgets[m]->windowControls()[w],
                    _windowList.back().viewports.back()
                );
                _windowList.back().size
                    = _displayWidgets[m]->windowControls()[w]->windowSize();
                _windowList.back().pos
                    = _displayWidgets[m]->windowControls()[w]->windowPos();
                _windowList.back().isDecorated
                    = _displayWidgets[m]->windowControls()[w]->isDecorated();
                bool isFullScreen = isWindowFullscreen(
                    _displayWidgets[m]->monitorResolution(),
                    _displayWidgets[m]->windowControls()[w]->windowSize()
                );
                if (isFullScreen) {
                    _windowList.back().isFullScreen = true;
                }
                if (isOneOfWindowsSetAsWebGui) {
                    if (windowIndex == webGuiWindowIndex) {
                        _windowList.back().draw2D = true;
                        _windowList.back().draw3D = false;
                        _windowList.back().viewports.back().isTracked = false;
                        _windowList.back().tags.push_back("GUI");
                    }
                    else {
                        _windowList.back().draw2D = false;
                        _windowList.back().draw3D = true;
                    }
                }
                else {
                        _windowList.back().draw2D = true;
                        _windowList.back().draw3D = true;
                        _windowList.back().viewports.back().isTracked = true;
                }
                if (!_displayWidgets[m]->windowControls()[w]->windowName().empty()) {
                    _windowList.back().name
                        = _displayWidgets[m]->windowControls()[w]->windowName();
                }
                _windowList.back().id = windowIndex++;
            }
        }
    }
}

void FileSupport::saveProjectionInformation(bool isSpoutSelected, int projectionIndex,
                             WindowControl* winControl, sgct::config::Viewport& viewport)
{
    if (isSpoutSelected) {
        sgct::config::SpoutOutputProjection projection;
        switch(projectionIndex) {
            case WindowControl::ProjectionIndeces::Fisheye:
                projection.mapping
                    = sgct::config::SpoutOutputProjection::Mapping::Fisheye;
                break;

            case WindowControl::ProjectionIndeces::Equirectangular:
            default:
                projection.mapping
                    = sgct::config::SpoutOutputProjection::Mapping::Equirectangular;
                break;
        }
        projection.quality = winControl->qualitySelectedIndex();
        projection.mappingSpoutName = "OpenSpace";
        viewport.projection = std::move(projection);
    }
    else {
        switch(projectionIndex) {
            case WindowControl::ProjectionIndeces::Fisheye:
                {
                    sgct::config::FisheyeProjection projection;
                    projection.quality = winControl->qualitySelectedIndex();
                    viewport.projection = std::move(projection);
                }
                break;

            case WindowControl::ProjectionIndeces::Spherical_Mirror:
                {
                    sgct::config::SphericalMirrorProjection projection;
                    projection.quality = winControl->qualitySelectedIndex();
                    viewport.projection = std::move(projection);
                }
                break;

            case WindowControl::ProjectionIndeces::Cylindrical:
                {
                    sgct::config::CylindricalProjection projection;
                    projection.quality = winControl->qualitySelectedIndex();
                    projection.heightOffset = winControl->heightOffset();
                    viewport.projection = std::move(projection);
                }
                break;

            case WindowControl::ProjectionIndeces::Equirectangular:
                {
                    sgct::config::EquirectangularProjection projection;
                    projection.quality = winControl->qualitySelectedIndex();
                    viewport.projection = std::move(projection);
                }
                break;

            case WindowControl::ProjectionIndeces::Planar:
            default:
                {
                    sgct::config::PlanarProjection projection;
                    projection.fov.left = winControl->fov() / 2.0;
                    projection.fov.right = projection.fov.left;
                    viewport.projection = std::move(projection);
                }
                break;
        }
    }
}

void FileSupport::filenameEdited(const QString& newString) {
    if (newString.isEmpty()) {
        _saveButton->setEnabled(false);
    }
    else {
        _saveButton->setEnabled(true);
    }
}

std::string FileSupport::saveFilename() {
    return _lineFilename->text().toStdString();
}

void FileSupport::save() {
    saveCluster();
    saveWindows();
    _finishedCallback(true);
}

void FileSupport::cancel() {
    _finishedCallback(false);
}

FileSupport::~FileSupport()
{
    delete _saveButton;
    delete _cancelButton;
    delete _lineFilename;
    delete _layoutButtonBox;
}

