#ifndef WINDOWCONTROL_H
#define WINDOWCONTROL_H

#include <QCheckBox>
#include <QIntValidator>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPoint>
#include <QVector>
#include <QWidget>

#include <vector>
#include <iostream>


class WindowControl : public QWidget
{
    Q_OBJECT

public:
    explicit WindowControl(unsigned int windowIndex, QRect& widgetDims, QRect& monitorDims,
        QWidget *parent = nullptr);
    ~WindowControl();
    void setDimensions(const QRectF& dimensions);
    void setWindowScaleFactor(float scaleFactor);
    void setWindowChangeCallback(std::function<void(unsigned int, const QRectF&)> cb);
    QRectF& dimensions();
    QLineEdit* lineEditSizeWidth();
    QLineEdit* lineEditSizeHeight();
    QLineEdit* lineEditSizeOffsetX();
    QLineEdit* lineEditSizeOffsetY();
    QCheckBox* checkBoxFullscreen();
    QCheckBox* checkBoxVsync();
    QCheckBox* checkBoxWebGui();
    QCheckBox* checkBoxSpoutOutput();

private slots:
    void onSizeXChanged(const QString& newText);
    void onSizeYChanged(const QString& newText);
    void onOffsetXChanged(const QString& newText);
    void onOffsetYChanged(const QString& newText);

private:
    void updateScaledWindowDimensions();
    std::function<void(unsigned int, const QRectF&)> _windowChangeCallback;
    QRectF defaultWindowSizes[2] = {
        {50.0, 50.0, 800.0, 600.0},
        {900.0, 400.0, 640.0, 480.0}
    };
    QLineEdit* _size_x = nullptr;
    QLineEdit* _size_y = nullptr;
    QLineEdit* _offset_x = nullptr;
    QLineEdit* _offset_y = nullptr;

    int _index = 0;
    QRect _monitorResolution;
    QRectF _monitorDimsScaled;
    QRectF _windowDims;
    QRectF _windowDimsScaled;
    float _monitorScaleFactor = 1.0;
    QCheckBox* _checkBoxFullscreen = nullptr;
    QCheckBox* _checkBoxVsync = nullptr;
    QCheckBox* _checkBoxWebGui = nullptr;
    QCheckBox* _checkBoxSpoutOutput = nullptr;

    float _marginFractionOfWidgetSize = 0.025;
};

#endif // WINDOWCONTROL_H
