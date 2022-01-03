#include <QApplication>
#include <QLabel>
#include <QLayout>
#include <QWidget>
#include <QTextBrowser>
#include <QLineEdit>
#include <QComboBox>
#include <QPushButton>
#include <QMainWindow>
#include <string>

#include "include/display.h"
#include "include/monitorbox.h"
#include "include/windowcontrol.h"


int main(int argc, char *argv[ ])
{
    QApplication app(argc, argv);
    QMainWindow win(nullptr);
    Display* displayWidget = new Display();
    win.setCentralWidget(displayWidget);
    win.setWindowTitle("Test Qt14");
    win.show();
    app.exec();

    delete displayWidget;
}
