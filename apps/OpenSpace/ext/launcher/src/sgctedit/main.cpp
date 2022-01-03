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
#include "include/monitorbox.h"
#include "include/windowcontrol.h"


int main(int argc, char *argv[ ])
{
    QApplication app(argc, argv);

    //Temporary code for monitor detection
    QList<QScreen*> screenList = app.screens();
    for (QScreen* s : screenList) {
        std::cout << "Monitor ";
        std::cout << s->size().width() << "x" << s->size().height();
        std::cout << std::endl;
    }
    QScreen* screen = app.primaryScreen();
    QRect screenGeometry = screen->geometry();
    //End code for monitor detection

    QMainWindow win(nullptr);
    Display* displayWidget = new Display();
    win.setCentralWidget(displayWidget);
    win.setWindowTitle("Test Qt14");
    win.show();
    app.exec();

    delete displayWidget;
}
