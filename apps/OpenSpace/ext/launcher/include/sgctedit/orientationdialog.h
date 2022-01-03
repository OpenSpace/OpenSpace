#ifndef ORIENTATION_DIALOG_H
#define ORIENTATION_DIALOG_H

#include <QCheckBox>
#include <QDialog>
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


class OrientationDialog : public QDialog
{
Q_OBJECT

public:
    explicit OrientationDialog(QWidget* parent);
    ~OrientationDialog();

private slots:

private:
    QVBoxLayout* _layoutWindow = nullptr;
    QHBoxLayout* _layoutPitch = nullptr;
    QHBoxLayout* _layoutRoll = nullptr;
    QHBoxLayout* _layoutYaw = nullptr;
    QHBoxLayout* _layoutButtonBox = nullptr;

    QLabel* _labelPitch = nullptr;
    QLabel* _labelRoll = nullptr;
    QLabel* _labelYaw = nullptr;

    QLineEdit* _linePitch = nullptr;
    QLineEdit* _lineRoll = nullptr;
    QLineEdit* _lineYaw = nullptr;
    QDoubleValidator* _validatorPitch = nullptr;
    QDoubleValidator* _validatorRoll = nullptr;
    QDoubleValidator* _validatorYaw = nullptr;

    QPushButton* _buttonSave = nullptr;
    QPushButton* _buttonCancel = nullptr;
};

#endif // ORIENTATION_DIALOG_H
