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
    /**
     * Constructor for WindowControl class, which contains settings and configuration
     * for individual windows
     *
     * \param monitorIndex The zero-based index for monitor number that this window
     *                     resides in
     * \param windowIndex The zero-based window index
     * \param monitorDims Vector of monitor dimensions in QRect form
     * \param winColor A  QColor object for this window's unique color
    */
    WindowControl(unsigned int monitorIndex, unsigned int windowIndex,
        std::vector<QRect>& monitorDims, const QColor& winColor,
        QWidget *parent);
    ~WindowControl();
    /**
     * Sets callback function to be invoked when a window's setting changes
     *
     * \param cb Callback function that accepts the listed arg types, in order of
     *           monitorIndex, windowIndex, and windowDims (that were just changed)
     */
    void setWindowChangeCallback(std::function<void(int, int, const QRectF&)> cb);
    /**
     * Sets callback function to be invoked when a window gets its GUI checkbox selected
     *
     * \param cb Callback function that accepts the index of the window that has its
     *           WebGUI option selected
     */
    void setWebGuiChangeCallback(std::function<void(unsigned int)> cb);
    /**
     * Makes the window label at top of a window control column visible
     *
     * \param bool Shows the window label if true
     */
    void showWindowLabel(bool show);
    /**
     * Initializes the layout of a window controls column, returning the Qt layout object
     *
     * \return the QVBoxLayout object that contains the entire windows control column
     */
    QVBoxLayout* initializeLayout();
    /**
     * Returns the dimensions of the window
     *
     * \return the QRectF object that contains the windows dimensions
     */
    QRectF& dimensions();
    /**
     * Returns the title name of the window
     *
     * \return the std::string of the window name
     */
    std::string windowName() const;
    /**
     * Returns the user-entered window size width, height from the text line objects
     *
     * \return the user-entered window size in sgct::ivec2 object
     */
    sgct::ivec2 windowSize() const;
    /**
     * Returns the user-entered window position in x,y pixles from the text line objects
     *
     * \return the user-entered window position in sgct::ivec2 object
     */
    sgct::ivec2 windowPos() const;
    /**
     * Returns bool for if the window control checkbox is set to be decorated
     *
     * \return bool for if window decoration is enabled
     */
    bool isDecorated() const;
    /**
     * Returns bool for if the window control checkbox spout selection is enabled
     *
     * \return bool for if window has spout enabled
     */
    bool isSpoutSelected() const;
    /**
     * Returns bool for if the window control checkbox for WebGUI is enabled
     *
     * \return bool for if window has WebGUI enabled
     */
    bool isGuiWindow() const;
    /**
     * Function called in order to disable/uncheck the WebGUI checkbox option
     */
    void uncheckWebGuiOption();
    /**
     * Returns index number of the selected window quality value. This is an index into
     * the QualityValues array
     *
     * \return index int into the QualityValues array
     */
    int qualitySelectedValue() const;
    /**
     * Returns index number of the monitor that this window is assigned to
     *
     * \return int index of monitor
     */
    unsigned int monitorNum() const;
    /**
     * Returns the user-entered horizontal field-of-view (planar projection only)
     *
     * \return float value of horizontal FOV
     */
    float fovH() const;
    /**
     * Returns the user-entered vertical field-of-view (planar projection only)
     *
     * \return float value of vertical FOV
     */
    float fovV() const;
    /**
     * Returns the user-entered height offset (cylindrical projection only)
     *
     * \return float value of height offset
     */
    float heightOffset() const;
    enum class ProjectionIndeces {
        Planar = 0,
        Fisheye,
        SphericalMirror,
        Cylindrical,
        Equirectangular
    };
    /**
     * Returns the user-selected window projection type
     *
     * \return ProjectionIndeces enum of the projection type
     */
    ProjectionIndeces projectionSelectedIndex() const;
    /**
     * Resets all controls for this window to default settings
     */
    void resetToDefaults();

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
    void determineIdealWindowSize();
    void updateScaledWindowDimensions();
    std::function<void(int, int, const QRectF&)> _windowChangeCallback;
    std::function<void(unsigned int)> _windowGuiCheckCallback;
    QRectF defaultWindowSizes[4] = {
        {50.f, 50.f, 1280.f, 720.f},
        {900.f, 250.f, 1280.f, 720.f},
        {1200.f, 340.f, 1280.f, 720.f},
        {50.f, 50.f, 1280.f, 720.f}
    };
    QList<QString> _monitorNames = { "Primary", "Secondary" };
    int QualityValues[10] = { 256, 512, 1024, 1536, 2048, 4096, 8192, 16384,
    32768, 65536 };
    int _lineEditWidthFixedWinSize = 50;
    int _lineEditWidthFixedFov = 80;
    float _marginFractionOfWidgetSize = 0.025f;
    float _defaultFovH = 80.f;
    float _defaultFovV = 50.534f;
    float _defaultHeightOffset = 0.f;
    unsigned int _nMonitors = 1;
    unsigned int _monIndex = 0;
    unsigned int _monIndexDefault = 0;
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
