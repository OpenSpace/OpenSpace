#ifndef WINDOWCONTROL_H
#define WINDOWCONTROL_H

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
    void setDimensions(const QRect& dimensions);
    void setWindowScaleFactor(float scaleFactor);
    void setWindowChangeCallback(std::function<void(unsigned int, const QRectF&)> cb);
    QRect* dimensions();
    QLineEdit* lineEditSizeWidth();
    QLineEdit* lineEditSizeHeight();
    QLineEdit* lineEditSizeOffsetX();
    QLineEdit* lineEditSizeOffsetY();

private slots:
    void onSizeXChanged(const QString& newText);
    void onSizeYChanged(const QString& newText);

private:
    void updateScaledWindowDimensions();
    std::function<void(unsigned int, const QRectF&)> _windowChangeCallback;

    QLineEdit* _size_x = nullptr;
    QLineEdit* _size_y = nullptr;
    QLineEdit* _offset_x = nullptr;
    QLineEdit* _offset_y = nullptr;

    QRect _monitorResolution;
    QRectF _monitorDimsScaled;
    QRect _windowDims;
    QRectF _windowDimsScaled;
    float _monitorScaleFactor = 1.0;
    int _index = 0;

    float _marginFractionOfWidgetSize = 0.025;
};

#endif // WINDOWCONTROL_H
