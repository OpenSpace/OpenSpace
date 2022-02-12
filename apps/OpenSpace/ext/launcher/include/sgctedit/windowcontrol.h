/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/
#ifndef __OPENSPACE_UI_LAUNCHER___WINDOWCONTROL___H__
#define __OPENSPACE_UI_LAUNCHER___WINDOWCONTROL___H__

#include <QWidget>

#include <sgct/config.h>
#include <QCheckBox>
#include <QComboBox>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QPushButton>
#include <vector>

class WindowControl : public QWidget {
Q_OBJECT
public:
    WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
        std::vector<QRect>& monitorDims, const QColor& winColor,
        QWidget *parent);
    ~WindowControl();
    void setWindowChangeCallback(std::function<void(int, int, const QRectF&)> cb);
    void setWebGuiChangeCallback(std::function<void(unsigned int)> cb);
    void showWindowLabel(bool show);
    QVBoxLayout* initializeLayout();
    QRectF& dimensions();
    QCheckBox* checkBoxWindowDecor();
    QCheckBox* checkBoxWebGui();
    QCheckBox* checkBoxSpoutOutput();
    std::string windowName() const;
    sgct::ivec2 windowSize() const;
    sgct::ivec2 windowPos() const;
    bool isDecorated() const;
    bool isSpoutSelected() const;
    bool isGuiWindow() const;
    void uncheckWebGuiOption();
    int qualitySelectedValue() const;
    unsigned int monitorNum() const;
    float fovH() const;
    float fovV() const;
    float heightOffset() const;
    enum class ProjectionIndeces {
        Planar = 0,
        Fisheye,
        SphericalMirror,
        Cylindrical,
        Equirectangular
    };
    ProjectionIndeces projectionSelectedIndex() const;
    int QualityValues[10] = {256, 512, 1024, 1536, 2048, 4096, 8192, 16384,
        32768, 65536};

private slots:
    void onSizeXChanged(const QString& newText);
    void onSizeYChanged(const QString& newText);
    void onOffsetXChanged(const QString& newText);
    void onOffsetYChanged(const QString& newText);
    void onMonitorChanged(int newSelection);
    void onProjectionChanged(int newSelection);
    void onFullscreenClicked();
    void onSpoutSelection(int selectionState);
    void onWebGuiSelection(int selectionState);

private:
    void createWidgets(QWidget* parent);
    void updateScaledWindowDimensions();
    std::function<void(int, int, const QRectF&)> _windowChangeCallback;
    std::function<void(unsigned int)> _windowGuiCheckCallback;
    QRectF defaultWindowSizes[4] = {
        {50.0, 50.0, 1280.0, 720.0},
        {900.0, 250.0, 1280.0, 720.0},
        {1200.0, 340.0, 1280.0, 720.0},
        {50.0, 50.0, 1280.0, 720.0}
    };
    QList<QString> _monitorNames = { "Monitor 1", "Monitor 2" };
    int _lineEditWidthFixed = 50;
    float _marginFractionOfWidgetSize = 0.025;
    unsigned int _nMonitors = 1;
    unsigned int _monIndex = 0;
    unsigned int _index = 0;
    std::vector<QRect>& _monitorResolutions;
    int _maxWindowSizePixels = 10000;
    const QColor& _colorForWindow;
    QVBoxLayout* _layoutFullWindow = nullptr;
    QLabel* _labelWinNum = nullptr;
    QLineEdit* _sizeX = nullptr;
    QLineEdit* _sizeY = nullptr;
    QLineEdit* _offsetX = nullptr;
    QLineEdit* _offsetY = nullptr;
    QRectF _windowDims;
    QPushButton* _fullscreenButton = nullptr;
    QCheckBox* _checkBoxWindowDecor = nullptr;
    QCheckBox* _checkBoxWebGui = nullptr;
    QCheckBox* _checkBoxSpoutOutput = nullptr;
    QComboBox* _comboMonitorSelect = nullptr;
    QComboBox* _comboProjection = nullptr;
    QComboBox* _comboQuality = nullptr;
    QLabel* _labelQuality = nullptr;
    QLabel* _labelFovH = nullptr;
    QLineEdit* _lineFovH = nullptr;
    QLabel* _labelFovV = nullptr;
    QLineEdit* _lineFovV = nullptr;
    QLabel* _labelHeightOffset = nullptr;
    QLineEdit* _lineHeightOffset = nullptr;
    QLineEdit* _windowName = nullptr;
};

#endif // __OPENSPACE_UI_LAUNCHER___WINDOWCONTROL___H__
