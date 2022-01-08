#include "sgctedit/sgctedit.h"

/*using namespace openspace;

namespace {

}*/

SgctEdit::SgctEdit(QWidget* parent, QApplication& qtApp)
    : QDialog(parent)
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

    //_monitorSizeList.clear();
    //_monitorSizeList.push_back({3440, 0, 1920, 1200});
    //_monitorSizeList.push_back({1080, 0, 1920, 1080});
}

void SgctEdit::createWidgets() {
//    QApplication app(argc, argv);
//    QMainWindow win(nullptr);

//    QFrame* monitorBorderFrame = nullptr;

    QVBoxLayout* layoutMainV = new QVBoxLayout(this);
    QHBoxLayout* layoutMainH = new QHBoxLayout;

    _orientationWidget = new Orientation();
//    QWidget* mainWindow = new QWidget();
//    mainWindow->setLayout(layoutMainV);
//    win.setCentralWidget(mainWindow);

    if (_monitorSizeList.size() > 1) {
        _monitorWidgetSize = QRect(0, 0, 600, 350);
        _showMonitorLabel = true;
    }

    {
        MonitorBox* monBox = new MonitorBox(
            _monitorWidgetSize,
            _monitorSizeList,
            _showMonitorLabel
        );
        QHBoxLayout* layoutMonBox = new QHBoxLayout();
        layoutMonBox->addStretch(1);
        //_layout->addWidget(_monBox);
        layoutMonBox->addWidget(monBox);
        layoutMonBox->addStretch(1);
        layoutMainV->addLayout(layoutMonBox);

        monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        monBox->setFixedSize(_monitorWidgetSize.width(), _monitorWidgetSize.height());

        addDisplayLayout(0, monBox, layoutMainH);
        if (_monitorSizeList.size() > 1) {
            addDisplayLayout(1, monBox, layoutMainH);
        }
    }
    //layoutMainH->setSizeConstraint(QLayout::SetFixedSize);
    layoutMainV->addLayout(layoutMainH);
    _orientationWidget->addButtonToLayout(layoutMainV);
    _fileSupportWidget = new FileSupport(layoutMainV);

//    win.setWindowTitle("Window Details");
//    win.show();
//    app.exec();
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

SgctEdit::~SgctEdit() {
    delete _orientationWidget;
    delete _fileSupportWidget;
/*    for (unsigned int i = 0; i <= 1; ++i) {
        if (displayWidget[i]) delete displayWidget[i];
        if (displayLayout[i]) delete displayLayout[i];
        if (displayFrame[i]) delete displayFrame[i];
    }
    if (monitorBorderFrame) {
        delete monitorBorderFrame;
    }
    delete monBox;
    delete layoutMonBox;
    delete layoutMainH;
    delete layoutMainV;
    delete mainWindow;*/
}
