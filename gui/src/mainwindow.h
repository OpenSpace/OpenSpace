#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QTcpSocket>

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

private slots:
    void on__connectButton_clicked();
    void on__statsCheckBox_toggled(bool checked);
    void readTcpData();
    void on__graphCheckBox_toggled(bool checked);
    void on__renderComboBox_currentIndexChanged(const QString &arg1);

private:
    qint64 sendToSGCT(QByteArray data);

    Ui::MainWindow *ui;
    QTcpSocket* _sgctSocket;
};

#endif // MAINWINDOW_H
