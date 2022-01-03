#ifndef FILE_SUPPORT_H
#define FILE_SUPPORT_H

#include <QFrame>
#include <QLabel>
#include <QLayout>
#include <QPushButton>
#include <QVector>
#include <QWidget>

#include <vector>
#include <iostream>


class FileSupport : public QWidget
{
    Q_OBJECT

public:
    explicit FileSupport(QVBoxLayout* parentLayout);
    ~FileSupport();

private slots:
    void cancel();

private:
    QHBoxLayout* _layoutButtonBox = nullptr;
    QPushButton* _saveButton = nullptr;
    QPushButton* _cancelButton = nullptr;
};

#endif // FILE_SUPPORT_H
