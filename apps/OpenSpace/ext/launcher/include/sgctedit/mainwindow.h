#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QLabel>
#include <QLayout>
#include <QMainWindow>
#include <QPushButton>
#include <QVBoxLayout>

#include <QTextBrowser>

#include <vector>
#include "monitorbox.h"

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private:
    QPushButton* button_;
    QTextBrowser* textBrowser_;
/*
    QBoxLayout* _layout;
    QBoxLayout* _monitorAreaLayout;
    std::vector<MonitorBox*> _monitorBoxes = {nullptr, nullptr};
    QPushButton* _addSecondMonitor;
*/
};
#endif // MAINWINDOW_H
