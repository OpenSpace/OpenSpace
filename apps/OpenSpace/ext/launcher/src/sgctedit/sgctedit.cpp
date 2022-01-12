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
}

void SgctEdit::createWidgets() {
    QVBoxLayout* layoutMainV = new QVBoxLayout(this);
    QHBoxLayout* layoutMainH = new QHBoxLayout;
    _orientationWidget = new Orientation();

    if (_monitorSizeList.size() > 1) {
        _monitorWidgetSize = QRect(0, 0, 600, 350);
        _showMonitorLabel = true;
    }

    {
        _monBox = new MonitorBox(
            _monitorWidgetSize,
            _monitorSizeList,
            _showMonitorLabel
        );
        QHBoxLayout* layoutMonBox = new QHBoxLayout();
        layoutMonBox->addStretch(1);
        layoutMonBox->addWidget(_monBox);
        layoutMonBox->addStretch(1);
        layoutMainV->addLayout(layoutMonBox);

        _monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        _monBox->setFixedSize(_monitorWidgetSize.width(), _monitorWidgetSize.height());

        addDisplayLayout(0, _monBox, layoutMainH);
        if (_monitorSizeList.size() > 1) {
            addDisplayLayout(1, _monBox, layoutMainH);
        }
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

void SgctEdit::addDisplayLayout(unsigned int column, MonitorBox* monBox,
                                                                      QHBoxLayout* layout)
{
    _displayLayout[column] = new QVBoxLayout();
    _displayWidget[column] = new Display(
        column,
        monBox,
        _monitorSizeList,
        (column == 0) ? 1 : 0,
        _showMonitorLabel,
        [this](unsigned int monIndex) {
            if (_monitorSizeList.size() > 1) {
                _displayWidget[(monIndex == 0) ? 1 : 0]->uncheckWebGuiOptions();
            }
        }
    );
    _displayFrame[column] = new QFrame;
    _displayLayout[column]->addWidget(_displayWidget[column]);
    _displayFrame[column]->setLayout(_displayLayout[column]);
    _displayFrame[column]->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    layout->addWidget(_displayFrame[column]);
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
    for (unsigned int i = 0; i <= _monitorSizeList.size(); ++i) {
        if (_displayWidget[i]) delete _displayWidget[i];
        if (_displayLayout[i]) delete _displayLayout[i];
        if (_displayFrame[i]) delete _displayFrame[i];
    }
}
