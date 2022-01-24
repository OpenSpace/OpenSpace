#include "sgctedit/sgctedit.h"

/*using namespace openspace;

namespace {

}*/

SgctEdit::SgctEdit(QWidget* parent, std::vector<sgct::config::Window>& windowList,
                   sgct::config::Cluster& cluster, QApplication& qtApp)
    : _cluster(cluster)
    , _windowList(windowList)
    , QDialog(parent)
{
    setWindowTitle("Display/Window Editor");
    systemMonitorConfiguration(qtApp);
    createWidgets();
}

void SgctEdit::systemMonitorConfiguration(QApplication& qtApp) {
    QList<QScreen*> screenList = qtApp.screens();
    if (screenList.length() == 0) {
        std::cerr << "Error: Qt reports no screens available." << std::endl;
        return;
    }

    for (size_t s = 0; s < std::min(screenList.length(), 2); ++s) {
        int actualWidth = std::max(screenList[s]->size().width(),
            screenList[s]->availableGeometry().width());
        int actualHeight = std::max(screenList[s]->size().height(),
            screenList[s]->availableGeometry().height());
        _monitorSizeList.push_back({
            screenList[s]->availableGeometry().x(),
            screenList[s]->availableGeometry().y(),
            actualWidth,
            actualHeight
        });
    }
    _nMaxWindows = (_monitorSizeList.size() == 1) ? 3 : 4;
}

void SgctEdit::createWidgets() {
    QVBoxLayout* layoutMainV = new QVBoxLayout(this);
    QHBoxLayout* layoutMainH = new QHBoxLayout;
    _orientationWidget = new Orientation();

    {
        _monBox = new MonitorBox(
            _monitorWidgetSize,
            _monitorSizeList,
            _nMaxWindows,
            _colorsForWindows
        );
        QHBoxLayout* layoutMonBox = new QHBoxLayout();
        layoutMonBox->addStretch(1);
        layoutMonBox->addWidget(_monBox);
        layoutMonBox->addStretch(1);
        layoutMainV->addLayout(layoutMonBox);

        _monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        _monBox->setFixedSize(_monitorWidgetSize.width(), _monitorWidgetSize.height());

        addDisplayLayout(_monBox, layoutMainH);
    }
    {
        layoutMainV->addLayout(layoutMainH);
        _orientationWidget->addButtonToLayout(layoutMainV);

        _fileSupportWidget = new FileSupport(
            layoutMainV,
            _monitorSizeList,
            _displayWidget,
            _orientationWidget,
            _windowList,
            _cluster,
            [this](bool accepted) {
                if (accepted) {
                    _saveSelected = true;
                    accept();
                }
                else {
                    reject();
                }
            }
        );
    }
}

void SgctEdit::addDisplayLayout(MonitorBox* monBox, QHBoxLayout* layout)
{
    _displayLayout = new QVBoxLayout();
    _displayWidget = new Display(
        monBox,
        _monitorSizeList,
        [this](unsigned int monIndex) {
            if (_monitorSizeList.size() > 1) {
                _displayWidget->uncheckWebGuiOptions();
            }
        },
        _nMaxWindows,
        _colorsForWindows
    );
    _displayFrame = new QFrame;
    _displayLayout->addWidget(_displayWidget);
    _displayFrame->setLayout(_displayLayout);
    _displayFrame->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    layout->addWidget(_displayFrame);
}

bool SgctEdit::wasSaved() const {
    return _saveSelected;
}

std::string SgctEdit::saveFilename() {
    return _fileSupportWidget->saveFilename();
}

SgctEdit::~SgctEdit() {
    delete _orientationWidget;
    delete _fileSupportWidget;
    delete _displayWidget;
    delete _displayLayout;
    delete _displayFrame;
}
