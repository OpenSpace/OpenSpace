#include <QApplication>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QMainWindow>
#include <QComboBox>
#include <QPushButton>
#include <QScreen>
#include <QTextBrowser>
#include <QWidget>
#include <string>

#include "include/display.h"
#include "include/filesupport.h"
#include "include/monitorbox.h"
#include "include/windowcontrol.h"
#include "include/orientation.h"



int main(int argc, char *argv[ ])
{
    QApplication app(argc, argv);
    QMainWindow win(nullptr);

    //Temporary code for monitor detection
    QList<QScreen*> screenList = app.screens();
    std::vector<QRect> monitorSizeList;
    for (size_t s = 0; s < std::min(screenList.length(), 2); ++s) {
        int actualWidth = std::max(screenList[s]->size().width(),
            screenList[s]->availableGeometry().width());
        int actualHeight = std::max(screenList[s]->size().height(),
            screenList[s]->availableGeometry().height());
        monitorSizeList.push_back({
            screenList[s]->availableGeometry().x(),
            screenList[s]->availableGeometry().y(),
            actualWidth,
            actualHeight
        });
    }
/*for (QScreen* s : screenList) {
    std::cout << "Monitor ";
    std::cout << s->size().width() << "x" << s->size().height();
    std::cout << ", " << s->availableGeometry().width() << "x";
    std::cout << s->availableGeometry().height() << " offset [";
    std::cout << s->availableGeometry().x() << ",";
    std::cout << s->availableGeometry().y() << "]";
    std::cout << std::endl;
}
    //std::cout << "Primary";
    //QScreen* screen = app.primaryScreen();
    //std::cout << screen->size().width() << "x" << screen->size().height() << std::endl;
    //QRect screenGeometry = screen->geometry();
    //End code for monitor detection
*/
    QFrame* monitorBorderFrame = nullptr;
    Orientation* orientationWidget = nullptr;

    if (screenList.length() == 0) {
        std::cerr << "Error: Qt reports no screens available." << std::endl;
        return -1;
    }
    QVBoxLayout* layoutMainV = new QVBoxLayout();
    QHBoxLayout* layoutMainH = new QHBoxLayout();

    orientationWidget = new Orientation();
    QWidget* mainWindow = new QWidget();
    mainWindow->setLayout(layoutMainV);
    win.setCentralWidget(mainWindow);

//monitorSizeList.push_back({3440, 0, 1920, 1080});
for (QRect m : monitorSizeList) {
  std::cout << "Display " << m.x() << "," << m.y() << " and " << m.width() << "x" << m.height() << std::endl;
}
for (QScreen* s : screenList) {
  std::cout << "Screen  " << s->size().width() << "," << s->size().height() << std::endl;
}
    bool showMonitorLabel = (monitorSizeList.size() > 1);
    MonitorBox* monBox = new MonitorBox(
        {0, 0, 400, 340},
        monitorSizeList,
        showMonitorLabel
    );
    QHBoxLayout* layoutMonBox = new QHBoxLayout();
    layoutMonBox->addStretch(1);
    //_layout->addWidget(_monBox);
    layoutMonBox->addWidget(monBox);
    layoutMonBox->addStretch(1);
    layoutMainV->addLayout(layoutMonBox);

    monBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    monBox->setFixedSize(400, 340);

    std::vector<QVBoxLayout*> displayLayout = {nullptr, nullptr};
    std::vector<QFrame*> displayFrame = {nullptr, nullptr};
    std::vector<Display*> displayWidget = {nullptr, nullptr};

    displayLayout[0] = new QVBoxLayout();
    displayWidget[0] = new Display(0, monBox, monitorSizeList, 1, showMonitorLabel);
    displayFrame[0] = new QFrame;
    displayLayout[0]->addWidget(displayWidget[0]);
    displayFrame[0]->setLayout(displayLayout[0]);
    displayFrame[0]->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
    layoutMainH->addWidget(displayFrame[0]);

    if (monitorSizeList.size() > 1) {
        displayLayout[1] = new QVBoxLayout();
        displayWidget[1] = new Display(1, monBox, monitorSizeList, 0, showMonitorLabel);
        displayFrame[1] = new QFrame;
        displayLayout[1]->addWidget(displayWidget[1]);
        displayFrame[1]->setLayout(displayLayout[1]);
        displayFrame[1]->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);
        layoutMainH->addWidget(displayFrame[1]);
    }

    layoutMainV->addLayout(layoutMainH);
    orientationWidget->addButtonToLayout(layoutMainV);
    FileSupport* fileSupportWidget = new FileSupport(layoutMainV);

    win.setWindowTitle("Window Details");
    win.show();
    app.exec();

    delete orientationWidget;
    for (unsigned int i = 0; i <= 1; ++i) {
        if (displayWidget[i]) delete displayWidget[i];
        if (displayLayout[i]) delete displayLayout[i];
        if (displayFrame[i]) delete displayFrame[i];
    }
    if (monitorBorderFrame) {
        delete monitorBorderFrame;
    }
    delete monBox;
    delete layoutMainH;
    delete layoutMainV;
    delete mainWindow;
}
