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
#include <QStandardItemModel>
#include <QVector>
#include <QWidget>

#include <vector>
#include <iostream>
#include <sgct/config.h>


class WindowControl : public QWidget
{
    Q_OBJECT

public:
    explicit WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
        QRect& widgetDims, QRect& monitorDims, QWidget *parent = nullptr);
    ~WindowControl();
    void setDimensions(const QRectF& dimensions);
    void setWindowChangeCallback(std::function<void(int, int, const QRectF&)> cb);
    void setWebGuiChangeCallback(std::function<void(unsigned int, unsigned int)> cb);
    void showWindowLabel(bool show);
    void cleanupLayouts();
    QVBoxLayout* initializeLayout(QWidget* parentWidget);
    QRectF& dimensions();
    QLineEdit* lineEditSizeWidth();
    QLineEdit* lineEditSizeHeight();
    QLineEdit* lineEditSizeOffsetX();
    QLineEdit* lineEditSizeOffsetY();
    QCheckBox* checkBoxWindowDecor();
    QCheckBox* checkBoxWebGui();
    QCheckBox* checkBoxSpoutOutput();
    std::string windowName();
    sgct::ivec2 windowSize();
    sgct::ivec2 windowPos();
    bool isDecorated();
    bool isSpoutSelected();
    bool isGuiWindow();
    void enableGuiWindowSelection(bool enabled);
    void uncheckWebGuiOption();
    int projectionSelectedIndex();
    int qualitySelectedIndex();
    float fovH();
    float fovV();
    float heightOffset();
    enum ProjectionIndeces : unsigned int {
        Planar = 0,
        Fisheye,
        Spherical_Mirror,
        Cylindrical,
        Equirectangular
    };
    std::string ProjectionTypeNames[5] = {"Planar", "Fisheye", "Spherical Mirror",
        "Cylindrical", "Equirectangular"};
    std::string QualityTypeNames[10] = {"Low (256)", "Medium (512)", "High (1K)",
        "1.5K (1536)", "2K (2048)", "4K (4096)", "8K (8192)", "16K (16384)",
        "32K (32768)", "64K (65536)"};

private slots:
    void onSizeXChanged(const QString& newText);
    void onSizeYChanged(const QString& newText);
    void onOffsetXChanged(const QString& newText);
    void onOffsetYChanged(const QString& newText);
    void onProjectionChanged(int newSelection);
    void onFullscreenClicked();
    void onSpoutSelection(int selectionState);
    void onWebGuiSelection(int selectionState);

private:
    void enableSpoutProjectionOptions(QComboBox* comboBox, bool enable);
    template <typename T>
    void enableProjectionOption(T* comboModel, int selectionIndex, bool enable);
    void updateScaledWindowDimensions();
    std::function<void(int, int, const QRectF&)> _windowChangeCallback;
    std::function<void(unsigned int, unsigned int)> _windowGuiCheckCallback;
    QRectF defaultWindowSizes[2] = {
        {50.0, 50.0, 1280.0, 720.0},
        {900.0, 400.0, 800.0, 600.0}
    };
    QList<QString> _projectionTypes = {
        QString::fromStdString(ProjectionTypeNames[ProjectionIndeces::Planar]),
        QString::fromStdString(ProjectionTypeNames[ProjectionIndeces::Fisheye]),
        QString::fromStdString(ProjectionTypeNames[ProjectionIndeces::Spherical_Mirror]),
        QString::fromStdString(ProjectionTypeNames[ProjectionIndeces::Cylindrical]),
        QString::fromStdString(ProjectionTypeNames[ProjectionIndeces::Equirectangular]),
    };
    QList<QString> _qualityTypes = {
        QString::fromStdString(QualityTypeNames[0]),
        QString::fromStdString(QualityTypeNames[1]),
        QString::fromStdString(QualityTypeNames[2]),
        QString::fromStdString(QualityTypeNames[3]),
        QString::fromStdString(QualityTypeNames[4]),
        QString::fromStdString(QualityTypeNames[5]),
        QString::fromStdString(QualityTypeNames[6]),
        QString::fromStdString(QualityTypeNames[7]),
        QString::fromStdString(QualityTypeNames[8]),
        QString::fromStdString(QualityTypeNames[9]),
    };
    int _lineEditWidthFixed = 50;
    float _marginFractionOfWidgetSize = 0.025;
    unsigned int _monIndex = 0;
    unsigned int _index = 0;
    int _maxWindowSizePixels = 10000;

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
    QRectF _windowDims;
    QPushButton* _fullscreenButton = nullptr;
    QCheckBox* _checkBoxWindowDecor = nullptr;
    QCheckBox* _checkBoxWebGui = nullptr;
    QCheckBox* _checkBoxSpoutOutput = nullptr;

    QComboBox* _comboProjection = nullptr;
    QComboBox* _comboQuality = nullptr;
    QLabel* _labelFovH = nullptr;
    QLineEdit* _lineFovH = nullptr;
    QDoubleValidator* _validatorFovH = nullptr;
    QLabel* _labelFovV = nullptr;
    QLineEdit* _lineFovV = nullptr;
    QDoubleValidator* _validatorFovV = nullptr;
    QLabel* _labelHeightOffset = nullptr;
    QLineEdit* _lineHeightOffset = nullptr;
    QDoubleValidator* _validatorHeightOffset = nullptr;

    QHBoxLayout* _layoutName = nullptr;
    QLabel* _labelName = nullptr;
    QLineEdit* _windowName = nullptr;
    QLabel* _labelSize = nullptr;
    QLabel* _labelDelim = nullptr;
    QHBoxLayout* _layoutSize = nullptr;
    QWidget* _widgetSize = nullptr;
    QHBoxLayout* _layoutOffset = nullptr;
    QLabel* _labelOffset = nullptr;
    QLabel* _labelComma = nullptr;
    QHBoxLayout* _layoutCheckboxesFull1 = nullptr;
    QVBoxLayout* _layoutCheckboxesFull2 = nullptr;
    QVBoxLayout* _layoutProjectionGroup = nullptr;
    QFrame* _borderProjectionGroup = nullptr;
    QHBoxLayout* _layoutFullscreenButton = nullptr;
    QHBoxLayout* _layoutCBoxWindowDecor = nullptr;
    QHBoxLayout* _layoutCBoxWebGui = nullptr;
    QHBoxLayout* _layoutCBoxSpoutOutput = nullptr;
    QHBoxLayout* _layoutComboProjection = nullptr;
    QHBoxLayout* _layoutComboQuality = nullptr;
    QHBoxLayout* _layoutFovH = nullptr;
    QHBoxLayout* _layoutFovV = nullptr;
    QWidget* _layoutFovWrapperH = nullptr;
    QWidget* _layoutFovWrapperV = nullptr;
    QHBoxLayout* _layoutHeightOffset = nullptr;
    QWidget* _layoutHeightOffsetWrapper = nullptr;
};

#endif // WINDOWCONTROL_H
