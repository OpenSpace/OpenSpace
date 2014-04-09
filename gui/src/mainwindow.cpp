#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <iostream>

#include <QJsonArray>
#include <QJsonValue>
#include <QJsonObject>
#include <QJsonDocument>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    _sgctSocket(NULL) {
    ui->setupUi(this);
    ui->_hostEdit->setText("127.0.0.1");
    setWindowTitle("OpenSpace Qt GUI");
    ui->_statsCheckBox->setEnabled(false);
    ui->_graphCheckBox->setEnabled(false);
    ui->_renderComboBox->setEnabled(false);

//    TEST JSON
//    QJsonObject json, jsonObj2;
//    QJsonArray jsonArr, jsonArr2;
//    json["Value 1"] = (QString("Herp"));
//    json["Value 2"] = (QString("Derp"));
//    jsonArr.push_back(QString("val1"));
//    jsonArr.push_back(QString("val2"));
//    jsonArr.push_back(QString("val3"));
//    jsonArr.push_back(QString("val4"));
//    jsonArr.push_back(QString("val5"));
//    jsonArr2.push_back(QString("val21"));
//    jsonArr2.push_back(QString("val22"));
//    jsonArr2.push_back(QString("val23"));
//    jsonArr2.push_back(QString("val24"));
//    jsonArr2.push_back(QString("val25"));
//    jsonObj2["ArrayYo2"] = jsonArr2;
//    jsonArr.push_back(jsonObj2);
//    json["ArrayYo"] = jsonArr;
//    QJsonDocument doc;
//    doc.setObject(json);
//    std::cout << doc.toJson().data() << std::endl;
//    quick_exit(0);
}

MainWindow::~MainWindow() {
    delete ui;
}

void MainWindow::on__connectButton_clicked() {
    _sgctSocket = new QTcpSocket(this);
    connect( _sgctSocket, SIGNAL(readyRead()), SLOT(readTcpData()) );
    _sgctSocket->connectToHost(ui->_hostEdit->text(), 20500);
    if (_sgctSocket->waitForConnected()) {
        ui->_statsCheckBox->setEnabled(true);
        ui->_graphCheckBox->setEnabled(true);
        ui->_renderComboBox->setEnabled(true);
    } else {
        std::cout << "Connection failed" << std::endl;
        ui->_statsCheckBox->setEnabled(false);
        ui->_graphCheckBox->setEnabled(false);
        ui->_renderComboBox->setEnabled(false);
    }
}

void MainWindow::on__statsCheckBox_toggled(bool checked) {
    QJsonObject json;
    QJsonDocument doc;
    if (checked) {
        json["stats"] = 1;
    } else {
        json["stats"] = 0;
    }
    doc.setObject(json);
    std::cout << "Bytes sent: " <<  sendToSGCT(doc.toJson()) << " " << std::flush;
}

void MainWindow::on__graphCheckBox_toggled(bool checked) {
    QJsonObject json;
    QJsonDocument doc;
    if (checked) {
        json["graph"] = 1;
    } else {
        json["graph"] = 0;
    }
    doc.setObject(json);
    std::cout << "Bytes sent: " <<  sendToSGCT(doc.toJson()) << " " << std::flush;
}

void MainWindow::readTcpData() {
    QByteArray data = _sgctSocket->readAll();
    std::cout << data.data() << std::flush;
}

void MainWindow::on__renderComboBox_currentIndexChanged(const QString &arg1){
    QJsonObject json;
    QJsonDocument doc;
    if (arg1.compare(QString("VolumeRaycaster")) == 0) {
        json["renderer"] = QString("volumeraycaster");
    } else if (arg1.compare(QString("Flare")) == 0) {
        json["renderer"] = QString("flare");
    }    
    doc.setObject(json);
    std::cout << "Bytes sent: " <<  sendToSGCT(doc.toJson()) << " " << std::flush;
}

qint64 MainWindow::sendToSGCT(QByteArray data) {
    return _sgctSocket->write(data + "\r\n"); // "\r\n" seperates messages
}
