/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <QIcon>
#include <vector>

class QCheckBox;
class QComboBox;
class QDoubleSpinBox;
class QLabel;
class QLineEdit;
class QSpinBox;

class WindowControl final : public QWidget {
Q_OBJECT
public:
    /**
     * Constructor for WindowControl class, which contains settings and configuration for
     * individual windows.
     *
     * \param monitorIndex The zero-based index for monitor number that this window
     *        resides in
     * \param windowIndex The zero-based window index
     * \param monitorDims Vector of monitor dimensions in QRect form
     * \param parent The parent widget
     */
    WindowControl(int monitorIndex, int windowIndex,
        const std::vector<QRect>& monitorDims, QWidget* parent);

    /**
     * Makes the window label at top of a window control column visible.
     *
     * \param show Shows the window label if true
     */
    void showWindowLabel(bool show);

    /**
     * Resets all controls for this window to default settings.
     */
    void resetToDefaults();

    /**
     * Sets the window dimensions.
     *
     * \param newDims The x, y dimensions to set the window to
     */
    void setDimensions(int x, int y, int width, int height);

    /**
     * Sets the monitor selection combobox.
     *
     * \param monitorIndex The zero-based monitor index to set the combobox selection to
     */
    void setMonitorSelection(int monitorIndex);

    /**
     * Sets the window name in the text edit box.
     *
     * \param windowName The window title to set
     */
    void setWindowName(const std::string& windowName);

    /**
     * Sets the window's decoration status. If set to true, then the window has a border.
     * If false it is borderless.
     *
     * \param hasWindowDecoration boolean for if window has decoration (border)
     */
    void setDecorationState(bool hasWindowDecoration);

    /**
     * Sets whether the window is shared using spout.
     */
    void setSpoutOutputState(bool shouldSpoutOutput);

    void setRender2D(bool state);
    void setRender3D(bool state);

    /**
     * Generates window configuration (sgct::config::Window struct) based on the
     * GUI settings.
     *
     * \param window The sgct::config::Window struct that is passed into the function and
     *        modified with the generated window content
     */
    void generateWindowInformation(sgct::config::Window& window) const;

    /**
     * Sets the window's projection type to planar, with the accompanying parameters
     * for horizontal and vertical FOV.
     *
     * \param hfov The horizontal field of view angle in degrees
     * \param vfov The vertical field of view angle in degrees
     */
    void setProjectionPlanar(float hfov, float vfov);

    /**
     * Sets the window's projection type to fisheye, with the accompanying quality
     * setting.
     *
     * \param quality The value for number of vertical lines of resolution. This will be
     *        compared against the QualityValues array in order to set the correct
     *        combobox index
     * \tilt  The tilt of the fisheye in degrees
     */
    void setProjectionFisheye(int quality, float tilt, float fov);

    /**
     * Sets the window's projection type to spherical mirror, with the accompanying
     * quality setting.
     *
     * \param quality The value for number of vertical lines of resolution. This will be
     *        compared against the QualityValues array in order to set the correct
     *        combobox index
     */
    void setProjectionSphericalMirror(int quality);

    /**
     * Sets the window's projection type to cylindrical, with the accompanying quality
     * setting and height offset value.
     *
     * \param quality The value for number of vertical lines of resolution. This will be
     *        compared against the QualityValues array in order to set the correct
     *        combobox index
     * \param heightOffset The height offset to be applied
     */
    void setProjectionCylindrical(int quality, float heightOffset);

    /**
     * Sets the window's projection type to equirectangular, with the accompanying
     * quality setting.
     *
     * \param quality The value for number of vertical lines of resolution. This will be
     *        compared against the QualityValues array in order to set the correct
     *        combobox index
     */
    void setProjectionEquirectangular(int quality);

    /**
     * Sets the window's projection type to blitting the contents of another window.
     *
     * \param windowBlitId The id of the window from which to blit
     */
    void setProjectionBlit(int windowBlitId);

    /**
     * This function must be called by users of this class whenever the total number of
     * windows has changed.
     *
     * \param newWindowCount the number of windows after the change
     */
    void updateWindowCount(int newWindowCount);

signals:
    void windowChanged(int monitorIndex, int windowIndex, const QRectF& newDimensions);

private:
    QWidget* createPlanarWidget();
    QWidget* createFisheyeWidget();
    QWidget* createSphericalMirrorWidget();
    QWidget* createCylindricalWidget();
    QWidget* createEquirectangularWidget();
    QWidget* createBlitWidget();

    void onSizeXChanged(int newValue);
    void onSizeYChanged(int newValue);
    void onOffsetXChanged(int newValue);
    void onOffsetYChanged(int newValue);
    void onProjectionChanged(int newSelection) const;
    void onFullscreenClicked();
    void onAspectRatioLockClicked();
    void onFovLockClicked();

    void updatePlanarLockedFov();

    static constexpr float IdealAspectRatio = 16.f / 9.f;
    float _aspectRatioSize = IdealAspectRatio;

    const int _monitorIndexDefault = 0;
    int _windowIndex = 0;
    bool _aspectRatioLocked = false;
    bool _fovLocked = true;
    std::vector<QRect> _monitorResolutions;
    QRect _windowDimensions;

    QLabel* _windowNumber = nullptr;
    QLineEdit* _windowName = nullptr;
    QComboBox* _monitor = nullptr;
    QSpinBox* _sizeX = nullptr;
    QSpinBox* _sizeY = nullptr;
    QSpinBox* _offsetX = nullptr;
    QSpinBox* _offsetY = nullptr;
    QCheckBox* _windowDecoration = nullptr;
    QCheckBox* _spoutOutput = nullptr;
    QCheckBox* _render2D = nullptr;
    QCheckBox* _render3D = nullptr;
    QFrame* _projectionGroup = nullptr;
    QComboBox* _projectionType = nullptr;

    struct {
        QWidget* widget = nullptr;
        QDoubleSpinBox* fovH = nullptr;
        QDoubleSpinBox* fovV = nullptr;
        QPushButton* buttonLockFov = nullptr;
    } _planar;

    struct {
        QWidget* widget = nullptr;
        QComboBox* quality = nullptr;
        QDoubleSpinBox* tilt = nullptr;
        QDoubleSpinBox* fov = nullptr;
    } _fisheye;

    struct {
        QWidget* widget = nullptr;
        QComboBox* quality = nullptr;
    } _sphericalMirror;

    struct {
        QWidget* widget = nullptr;
        QComboBox* quality = nullptr;
        QDoubleSpinBox* heightOffset = nullptr;
    } _cylindrical;

    struct {
        QWidget* widget = nullptr;
        QComboBox* quality = nullptr;
    } _equirectangular;

    struct {
        QWidget* widget = nullptr;
        QComboBox* windowId = nullptr;
        QLabel* unavailable = nullptr;
    } _blit;

    const QIcon _lockIcon;
    const QIcon _unlockIcon;
};

#endif // __OPENSPACE_UI_LAUNCHER___WINDOWCONTROL___H__
