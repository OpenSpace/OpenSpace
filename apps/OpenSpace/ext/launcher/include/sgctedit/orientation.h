#ifndef ORIENTATION_H
#define ORIENTATION_H

#include <QCheckBox>
#include <QFrame>
#include <QIntValidator>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QPushButton>
#include <QVector>
#include <QWidget>

#include <vector>
#include <iostream>


class Orientation : public QWidget
{
    Q_OBJECT

public:
    explicit Orientation();
    ~Orientation();
    void addButtonToLayout(QVBoxLayout* parentLayout);

private slots:
    void orientationDialog();

private:
    QHBoxLayout* _layoutOrientationFull = nullptr;
    QVBoxLayout* _layoutOrientationControls = nullptr;
    QPushButton* _orientationButton = nullptr;
    QCheckBox* _checkBoxVsync = nullptr;
};

#endif // ORIENTATION_H
