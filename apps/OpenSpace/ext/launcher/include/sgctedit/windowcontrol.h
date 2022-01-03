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


struct ConfigResolution
{
    ConfigResolution(float width, float height) {
        _width = width;
        _height = height;
    }
    float _width;
    float _height;

    ConfigResolution& operator=(ConfigResolution& rhs) {
        _width = rhs._width;
        _height = rhs._height;
        return *this;
    }
};

class WindowControl : public QWidget
{
    Q_OBJECT

public:
    explicit WindowControl(QLineEdit* size_text_x, QLineEdit* size_text_y,
        QWidget *parent = nullptr);
    ~WindowControl();
    void setMonitorResolution(ConfigResolution r);
    void setNumWindows(int nWindows);
    int numWindows();
    void setWindowSize(int index, ConfigResolution r, ConfigResolution offset);
    ConfigResolution windowSize(int index);

protected:
    void paintEvent(QPaintEvent *event) override;

private slots:
    void onSizeXChanged(const QString& newText);
    void onSizeYChanged(const QString& newText);

private:
    void redrawMonitor(ConfigResolution);
    void redrawWindow(ConfigResolution);

    QLineEdit* _size_x = nullptr;
    QLineEdit* _size_y = nullptr;

    ConfigResolution _monitorDimensions;

    std::vector<ConfigResolution> _windowResolutions;
    QRectF _monitorRect;
    std::vector<QRectF> _windowRect;
    float _monitorScaleFactor = 1.0;
    float _offset[2] = {10.0, 10.0};

    float _marginFractionOfWidgetSize = 0.025;
    ConfigResolution _monitorWidgetSize;
    int _nWindows = 1;
};

#endif // WINDOWCONTROL_H
