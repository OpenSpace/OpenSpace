/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
     * Constructor for WindowControl class, which contains settings and configuration
     * for individual windows
     *
     * \param monitorIndex The zero-based index for monitor number that this window
     *                     resides in
     * \param windowIndex The zero-based window index
     * \param monitorDims Vector of monitor dimensions in QRect form
     * \param winColor A QColor object for this window's unique color
     */
    WindowControl(int monitorIndex, int windowIndex,
        const std::vector<QRect>& monitorDims, const QColor& winColor, QWidget* parent);
    
    /**
     * Makes the window label at top of a window control column visible
     *
     * \param bool Shows the window label if true
     */
    void showWindowLabel(bool show);

    /**
     * Resets all controls for this window to default settings
     */
    void resetToDefaults();

    sgct::config::Window generateWindowInformation() const;

signals:
    void windowChanged(int monitorIndex, int windowIndex, const QRectF& newDimensions);

private:
    enum class ProjectionIndices {
        Planar = 0,
        Fisheye,
        SphericalMirror,
        Cylindrical,
        Equirectangular
    };

    void createWidgets(const QColor& windowColor);
    QWidget* createPlanarWidget();
    QWidget* createFisheyeWidget();
    QWidget* createSphericalMirrorWidget();
    QWidget* createCylindricalWidget();
    QWidget* createEquirectangularWidget();

    void onSizeXChanged(int newValue);
    void onSizeYChanged(int newValue);
    void onOffsetXChanged(int newValue);
    void onOffsetYChanged(int newValue);
    void onProjectionChanged(int newSelection);
    void onFullscreenClicked();
    void onAspectRatioLockClicked();
    void onFovLockClicked();

    sgct::config::Projections generateProjectionInformation() const;
    void updatePlanarLockedFov();

    static constexpr float IdealAspectRatio = 16.f / 9.f;
    float _aspectRatioSize = IdealAspectRatio;

    const int _monitorIndexDefault = 0;
    int _windowIndex = 0;
    bool _aspectRatioLocked = false;
    bool _fovLocked = true;
    std::vector<QRect> _monitorResolutions;
    QRectF _windowDimensions;
    
    QLabel* _windowNumber = nullptr;
    QLineEdit* _windowName = nullptr;
    QComboBox* _monitor = nullptr;
    QSpinBox* _sizeX = nullptr;
    QSpinBox* _sizeY = nullptr;
    QSpinBox* _offsetX = nullptr;
    QSpinBox* _offsetY = nullptr;
    QCheckBox* _windowDecoration = nullptr;
    QComboBox* _projectionType = nullptr;

    struct {
        QWidget* widget = nullptr;
        QDoubleSpinBox* fovH = nullptr;
        QDoubleSpinBox* fovV = nullptr;
    } _planar;

    struct {
        QWidget* widget = nullptr;
        QComboBox* quality = nullptr;
        QCheckBox* spoutOutput = nullptr;
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
        QCheckBox* spoutOutput = nullptr;
    } _equirectangular;

    const QIcon _lockIcon;
    const QIcon _unlockIcon;
};

#endif // __OPENSPACE_UI_LAUNCHER___WINDOWCONTROL___H__
