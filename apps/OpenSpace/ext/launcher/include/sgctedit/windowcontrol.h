#ifndef WINDOWCONTROL_H
#define WINDOWCONTROL_H

#include <QCheckBox>
#include <QComboBox>
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


class WindowControl : public QWidget
{
    Q_OBJECT

public:
    explicit WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
        QRect& widgetDims, QRect& monitorDims, QWidget *parent = nullptr);
    ~WindowControl();
    void setDimensions(const QRectF& dimensions);
    void setWindowScaleFactor(float scaleFactor);
    void setWindowChangeCallback(
        std::function<void(unsigned int, unsigned int, const QRectF&)> cb);
    void showWindowLabel(bool show);
    void cleanupLayouts();
    QVBoxLayout* initializeLayout(QWidget* parentWidget);
    QRectF& dimensions();
    QLineEdit* lineEditSizeWidth();
    QLineEdit* lineEditSizeHeight();
    QLineEdit* lineEditSizeOffsetX();
    QLineEdit* lineEditSizeOffsetY();
    QCheckBox* checkBoxWindowDecor();
    QCheckBox* checkBoxVsync();
    QCheckBox* checkBoxWebGui();
    QCheckBox* checkBoxSpoutOutput();

private slots:
    void onSizeXChanged(const QString& newText);
    void onSizeYChanged(const QString& newText);
    void onOffsetXChanged(const QString& newText);
    void onOffsetYChanged(const QString& newText);
    void onProjectionChanged(int newSelection);
    void onQualityChanged(int newSelection);
    void onFullscreenClicked();

private:
    void updateScaledWindowDimensions();
    std::function<void(unsigned int, unsigned int, const QRectF&)> _windowChangeCallback;
    QRectF defaultWindowSizes[2] = {
        {50.0, 50.0, 800.0, 600.0},
        {900.0, 400.0, 2540.0, 680.0}
    };
    QList<QString> _projectionTypes = {
        "Planar", "Fisheye", "Spherical Mirror", "Cylindrical", "Equirectangular"
    };
    QList<QString> _qualityTypes = {
        "Low (256)", "Medium (512)", "High (1K)", "1.5K (1536)", "2K (2048)",
        "4K (4096)", "8K (8192)", "16K (16384)", "32K (32768)", "64K (65536)"
    };
    int _lineEditWidthFixed = 50;
    float _marginFractionOfWidgetSize = 0.025;
    unsigned int _monIndex = 0;
    unsigned int _index = 0;
    unsigned int _maxWindowSizePixels = 10000;

    QVBoxLayout* _layoutWindowCtrl = nullptr;
    QVBoxLayout* _layoutFullWindow = nullptr;

    QHBoxLayout* _layoutWinNum = nullptr;
    QLabel* _labelWinNum = nullptr;

    QLineEdit* _size_x = nullptr;
    QLineEdit* _size_y = nullptr;
    QLineEdit* _offset_x = nullptr;
    QLineEdit* _offset_y = nullptr;

    QIntValidator* _validatorSize_x = nullptr;
    QIntValidator* _validatorSize_y = nullptr;
    QIntValidator* _validatorOffset_x = nullptr;
    QIntValidator* _validatorOffset_y = nullptr;

    QRect& _monitorResolution;
    QRectF _monitorDimsScaled;
    QRectF _windowDims;
    QRectF _windowDimsScaled;
    float _monitorScaleFactor = 1.0;
    QPushButton* _fullscreenButton = nullptr;
    QCheckBox* _checkBoxWindowDecor = nullptr;
    QCheckBox* _checkBoxVsync = nullptr;
    QCheckBox* _checkBoxWebGui = nullptr;
    QCheckBox* _checkBoxSpoutOutput = nullptr;

    QComboBox* _comboProjection = nullptr;
    QComboBox* _comboQuality = nullptr;
    QLabel* _labelFov = nullptr;
    QLineEdit* _lineFov = nullptr;
    QDoubleValidator* _validatorFov = nullptr;
    QLabel* _labelHeightOffset = nullptr;
    QLineEdit* _lineHeightOffset = nullptr;
    QDoubleValidator* _validatorHeightOffset = nullptr;

    QHBoxLayout* _layoutName = nullptr;
    QLabel* _labelName = nullptr;
    QLineEdit* _windowName = nullptr;
    QLabel* _labelSize = nullptr;
    QLabel* _labelDelim = nullptr;
    QGridLayout* _layoutSize = nullptr;
    QHBoxLayout* _layoutGridFrame= nullptr;
    QHBoxLayout* _layoutGridSizeValues = nullptr;
    QHBoxLayout* _layoutGridOffsetValues = nullptr;
    QLabel* _labelOffset = nullptr;
    QLabel* _labelComma = nullptr;
    QHBoxLayout* _layoutCheckboxesFull1 = nullptr;
    QVBoxLayout* _layoutCheckboxesFull2 = nullptr;
    QHBoxLayout* _layoutFullscreenButton = nullptr;
    QHBoxLayout* _layoutCBoxWindowDecor = nullptr;
    QHBoxLayout* _layoutCBoxVsync = nullptr;
    QHBoxLayout* _layoutCBoxWebGui = nullptr;
    QHBoxLayout* _layoutCBoxSpoutOutput = nullptr;
    QHBoxLayout* _layoutComboProjection = nullptr;
    QHBoxLayout* _layoutComboQuality = nullptr;
    QHBoxLayout* _layoutFov = nullptr;
    QWidget* _layoutFovWrapper = nullptr;
    QHBoxLayout* _layoutHeightOffset = nullptr;
    QWidget* _layoutHeightOffsetWrapper = nullptr;
};

#endif // WINDOWCONTROL_H
