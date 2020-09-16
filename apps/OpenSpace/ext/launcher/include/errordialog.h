#ifndef ERRORDIALOG_H
#define ERRORDIALOG_H

#include <QDir>
#include <QDialog>
#include <QLineEdit>
#include "ui_errordialog.h"

namespace Ui {
class errordialog;
}

class errordialog : public QDialog
{
    Q_OBJECT

public slots:
   //void accept();

public:
    explicit errordialog(QString message, QWidget *parent = nullptr);
    ~errordialog();

private:
    Ui::errordialog *ui;
    QWidget* _parent;
};

#endif // ERRORDIALOG_H
