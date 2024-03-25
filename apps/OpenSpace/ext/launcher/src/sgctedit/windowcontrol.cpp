/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include "sgctedit/windowcontrol.h"

#include <ghoul/misc/assert.h>
#include <ghoul/fmt.h>
#include "sgctedit/displaywindowunion.h"
#include "sgctedit/monitorbox.h"
#include <QCheckBox>
#include <QComboBox>
#include <QGridLayout>
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QSpinBox>
#include <numbers>

namespace {
    std::array<std::string, 4> MonitorNames = {
        "Primary", "Secondary", "Tertiary", "Quaternary"
    };

    constexpr int nQualityTypes = 10;

    const QList<QString> QualityTypes = {
        "Low (256)", "Medium (512)", "High (1K)", "1.5K (1536)", "2K (2048)", "4K (4096)",
        "8K (8192)", "16K (16384)", "32K (32768)", "64K (65536)"
    };

    constexpr std::array<int, nQualityTypes> QualityValues = {
        256, 512, 1024, 1536, 2048, 4096, 8192, 16384, 32768, 65536
    };

    const QList<QString> ProjectionTypes = {
        "Planar Projection", "Fisheye", "Spherical Mirror Projection",
        "Cylindrical Projection", "Equirectangular Projection"
    };

    constexpr std::array<QRectF, 4> DefaultWindowSizes = {
        QRectF(50.f, 50.f, 1280.f, 720.f),
        QRectF(150.f, 150.f, 1280.f, 720.f),
        QRectF(50.f, 50.f, 1280.f, 720.f),
        QRectF(150.f, 150.f, 1280.f, 720.f)
    };

    constexpr int LineEditWidthFixedWindowSize = 64;
    constexpr float DefaultFovLongEdge = 80.f;
    constexpr float DefaultFovShortEdge = 50.534f;
    constexpr float DefaultHeightOffset = 0.f;
    constexpr int MaxWindowSizePixels = 10000;
    constexpr double FovEpsilon = 0.00001;

    QList<QString> monitorNames(const std::vector<QRect>& resolutions) {
        QList<QString> monitorNames;
        for (size_t i = 0; i < resolutions.size(); i++) {
            const std::string fullName = std::format(
                "{} ({}x{})",
                MonitorNames[i], resolutions[i].width(), resolutions[i].height()
            );
            monitorNames.push_back(QString::fromStdString(fullName));
        }
        return monitorNames;
    }
} // namespace

WindowControl::WindowControl(int monitorIndex, int windowIndex,
                             const std::vector<QRect>& monitorDims,
                             const QColor& winColor, bool resetToDefault, QWidget* parent)
    : QWidget(parent)
    , _monitorIndexDefault(monitorIndex)
    , _windowIndex(windowIndex)
    , _monitorResolutions(monitorDims)
    , _lockIcon(":/images/outline_locked.png")
    , _unlockIcon(":/images/outline_unlocked.png")
{
    createWidgets(winColor);
    if (resetToDefault) {
        resetToDefaults();
    }
}

void WindowControl::createWidgets(const QColor& windowColor) {
    //      Col 0      Col 1    Col 2     Col 3    Col 4    Col 5   Col 6   Col 7
    //  *----------*----------*-------*----------*-------*--------*-------*-------*
    //  |                                   Window {n}                            | R0
    //  | Name     * [oooooooooooooooooooooooooooooooooooooooooooooooooooooooooo] | R1
    //  | Monitor  * DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD> | R2
    //  | Size     * [xxxxxx] *    x  * [yyyyyy] *  px   * <lock> * < Set to      | R3
    //  | Offset   * [xxxxxx] *    ,  * [yyyyyy] *  px   *        *   Fullscreen> | R4
    //  | [] Window Decoration                                                    | R5
    //  | [] UI only in this window                                               | R6
    //  | ~~~~~~~~~~~~~~~~~~~~~~~~~Projection components~~~~~~~~~~~~~~~~~~~~~~~~~ | R7
    //  *----------*----------*-------*----------*-------*--------*-------*-------*

    QGridLayout* layout = new QGridLayout(this);
    const QMargins margins = layout->contentsMargins();
    layout->setContentsMargins(margins.left(), 0, margins.right(), 0);
    layout->setColumnStretch(6, 1);
    layout->setRowStretch(8, 1);
    
    _windowNumber = new QLabel("Window " + QString::number(_windowIndex + 1));
    _windowNumber->setStyleSheet(QString::fromStdString(std::format(
        "QLabel {{ color : #{:02x}{:02x}{:02x}; }}",
        windowColor.red(), windowColor.green(), windowColor.blue()
    )));
    layout->addWidget(_windowNumber, 0, 0, 1, 8, Qt::AlignCenter);
    {
        const QString tip = "The name for the window (displayed in title bar)";

        QLabel* labelName = new QLabel("Name");
        labelName->setToolTip(tip);
        layout->addWidget(labelName, 1, 0);

        _windowName = new QLineEdit;
        _windowName->setToolTip(tip);
        layout->addWidget(_windowName, 1, 1, 1, 7);
    }
    const QString tip = "The monitor where this window is located";

    _monitor = new QComboBox;
    _monitor->addItems(monitorNames(_monitorResolutions));
    _monitor->setCurrentIndex(_monitorIndexDefault);
    _monitor->setToolTip(tip);
    connect(
        _monitor, qOverload<int>(&QComboBox::currentIndexChanged),
        [this]() {
            emit windowChanged(
                _monitor->currentIndex(),
                _windowIndex,
                _windowDimensions
            );
        }
    );
    if (_monitorResolutions.size() > 1) {
        QLabel* labelLocation = new QLabel("Monitor");
        labelLocation->setToolTip(tip);
        layout->addWidget(labelLocation, 2, 0);

        layout->addWidget(_monitor, 2, 1, 1, 7);
    }
    {
        QLabel* size = new QLabel("Size");
        size->setToolTip("The window's width & height in pixels");
        size->setFixedWidth(55);
        layout->addWidget(size, 3, 0);

        _sizeX = new QSpinBox;
        _sizeX->setMinimum(0);
        _sizeX->setMaximum(MaxWindowSizePixels);
        _sizeX->setFixedWidth(LineEditWidthFixedWindowSize);
        _sizeX->setToolTip("The window's width (pixels)");
        layout->addWidget(_sizeX, 3, 1);
        connect(
            _sizeX, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WindowControl::onSizeXChanged
        );

        QLabel* delim = new QLabel("x");
        delim->setSizePolicy(QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum));
        layout->addWidget(delim, 3, 2);

        _sizeY = new QSpinBox;
        _sizeY->setMinimum(0);
        _sizeY->setMaximum(MaxWindowSizePixels);
        _sizeY->setFixedWidth(LineEditWidthFixedWindowSize);
        _sizeY->setToolTip("The window's height (pixels)");
        layout->addWidget(_sizeY, 3, 3, Qt::AlignLeft);
        connect(
            _sizeY, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WindowControl::onSizeYChanged
        );

        QLabel* unit = new QLabel("px");
        unit->setSizePolicy(QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum));
        layout->addWidget(unit, 3, 4, Qt::AlignLeft);

        QPushButton* lockAspectRatio = new QPushButton;
        lockAspectRatio->setIcon(_unlockIcon);
        lockAspectRatio->setFocusPolicy(Qt::NoFocus);
        lockAspectRatio->setToolTip("Locks/Unlocks the aspect ratio of the window size");
        layout->addWidget(lockAspectRatio, 3, 5, Qt::AlignLeft);
        connect(
            lockAspectRatio, &QPushButton::released,
            this, &WindowControl::onAspectRatioLockClicked
        );
        connect(
            lockAspectRatio, &QPushButton::released,
            [this, lockAspectRatio]() {
                lockAspectRatio->setIcon(_aspectRatioLocked ? _lockIcon : _unlockIcon);
            }
        );
    }
    {
        QLabel* offset = new QLabel("Offset");
        offset->setToolTip(
            "The x,y location of the window's upper left corner from monitor's "
            "upper-left corner origin (pixels)"
        );
        offset->setFixedWidth(55);
        layout->addWidget(offset, 4, 0);

        _offsetX = new QSpinBox;
        _offsetX->setMinimum(0);
        _offsetX->setMaximum(MaxWindowSizePixels);
        _offsetX->setToolTip(
            "The x location of the window's upper left corner from monitor's left side "
            "(pixels)"
        );
        _offsetX->setFixedWidth(LineEditWidthFixedWindowSize);
        layout->addWidget(_offsetX, 4, 1);
        connect(
            _offsetX, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WindowControl::onOffsetXChanged
        );

        QLabel* comma = new QLabel(",");
        comma->setSizePolicy(QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum));
        layout->addWidget(comma, 4, 2);

        _offsetY = new QSpinBox;
        _offsetY->setMinimum(0);
        _offsetY->setMaximum(MaxWindowSizePixels);
        _offsetY->setToolTip(
            "The y location of the window's upper left corner from monitor's top edge "
            "(pixels)"
        );
        _offsetY->setFixedWidth(LineEditWidthFixedWindowSize);
        layout->addWidget(_offsetY, 4, 3, Qt::AlignLeft);
        connect(
            _offsetY, QOverload<int>::of(&QSpinBox::valueChanged),
            this, &WindowControl::onOffsetYChanged
        );

        QLabel* unit = new QLabel("px");
        unit->setSizePolicy(QSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum));
        layout->addWidget(unit, 4, 4, Qt::AlignLeft);
    }
    {
        QBoxLayout* holderLayout = new QHBoxLayout;

        QPushButton* setFullscreen = new QPushButton("Set Window\nto Fullscreen");
        setFullscreen->setToolTip(
            "If enabled, the window will be created in an exclusive fullscreen mode. The "
            "size of this\nwindow will be set to the screen resolution, and the window "
            "decoration automatically disabled"
        );
        setFullscreen->setFocusPolicy(Qt::NoFocus);
        setFullscreen->setSizePolicy(
            QSizePolicy::MinimumExpanding,
            QSizePolicy::MinimumExpanding
        );
        holderLayout->addStretch();
        holderLayout->addWidget(setFullscreen);
        holderLayout->addStretch();
        layout->addLayout(holderLayout, 3, 6, 2, 2);
        connect(
            setFullscreen, &QPushButton::released,
            this, &WindowControl::onFullscreenClicked
        );

        _windowDecoration = new QCheckBox("Window Decoration");
        _windowDecoration->setChecked(true);
        _windowDecoration->setToolTip(
            "If disabled, the window will not have a border frame or title bar, and no\n "
            "controls for minimizing/maximizing, resizing, or closing the window"
        );
        layout->addWidget(_windowDecoration, 5, 0, 1, 8);
    }
    {
        QFrame* projectionGroup = new QFrame;
        projectionGroup->setVisible(true);
        projectionGroup->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);

        //
        // Projection combobox
        QBoxLayout* projectionLayout = new QVBoxLayout(projectionGroup);
        projectionLayout->setContentsMargins(0, 0, 0, 0);
        projectionLayout->setSpacing(0);
        _projectionLabel = new QLabel(
            "Projection information not shown while user interface is set to display "
            "on the first window only"
        );
        _projectionLabel->setWordWrap(true);
        _projectionLabel->setObjectName("notice");
        _projectionLabel->setVisible(false);
        _projectionLabel->setEnabled(false);
        projectionLayout->addWidget(_projectionLabel);

        _projectionType = new QComboBox;
        _projectionType->addItems({
            ProjectionTypes[0],
            ProjectionTypes[1],
            ProjectionTypes[2],
            ProjectionTypes[3],
            ProjectionTypes[4]
        });
        _projectionType->setToolTip("Select from the supported window projection types");
        _projectionType->setCurrentIndex(0);
        projectionLayout->addWidget(_projectionType);
        connect(
            _projectionType, qOverload<int>(&QComboBox::currentIndexChanged),
            this, &WindowControl::onProjectionChanged
        );

        _planar.widget = createPlanarWidget();
        projectionLayout->addWidget(_planar.widget);

        _fisheye.widget = createFisheyeWidget();
        projectionLayout->addWidget(_fisheye.widget);

        _sphericalMirror.widget = createSphericalMirrorWidget();
        projectionLayout->addWidget(_sphericalMirror.widget);

        _cylindrical.widget = createCylindricalWidget();
        projectionLayout->addWidget(_cylindrical.widget);

        _equirectangular.widget = createEquirectangularWidget();
        projectionLayout->addWidget(_equirectangular.widget);

        // We need to trigger this once to ensure that all of the defaults are correct
        onProjectionChanged(0);

        layout->addWidget(projectionGroup, 7, 0, 1, 8);
    }
}

QWidget* WindowControl::createPlanarWidget() {
    //    Column 0   Column 1   Column 2
    //  *----------*----------*----------*
    //  | { Informational text }         |  Row 0
    //  |   HFOV   * [oooooo] *   Lock   |  Row 1
    //  |   VFOV   * [oooooo] *  Button  |  Row 2
    //  *----------*----------*----------*

    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    _planar.labelInfo = new QLabel(
        "This projection type is the 'regular' projection with a horizontal and a "
        "vertical field of view, given in degrees. The wider the field of view, the "
        "more content is shown at the same time, but everything becomes smaller. Very "
        "large values will introduce distorions on the corners"
    );
    _planar.labelInfo->setObjectName("info");
    _planar.labelInfo->setWordWrap(true);
    layout->addWidget(_planar.labelInfo, 0, 0, 1, 3);

    _planar.labelFovH = new QLabel("Horizontal FOV");
    const QString hfovTip =
        "The total horizontal field of view of the viewport (degrees)";
    _planar.labelFovH->setToolTip(hfovTip);
    layout->addWidget(_planar.labelFovH, 1, 0);

    _planar.fovH = new QDoubleSpinBox;
    _planar.fovH->setMinimum(FovEpsilon);
    _planar.fovH->setMaximum(180.0 - FovEpsilon);
    _planar.fovH->setValue(DefaultFovLongEdge);
    _planar.fovH->setEnabled(false);
    _planar.fovH->setToolTip(hfovTip);
    _planar.fovH->setSizePolicy(
        QSizePolicy::MinimumExpanding,
        QSizePolicy::MinimumExpanding
    );
    layout->addWidget(_planar.fovH, 1, 1);

    _planar.labelFovV = new QLabel("Vertical FOV");
    const QString vfovTip = "The total vertical field of view of the viewport (degrees). "
        "Internally,\nthe values for 'up' & 'down' will each be half this value";
    _planar.labelFovV->setToolTip(vfovTip);
    layout->addWidget(_planar.labelFovV, 2, 0);

    _planar.fovV = new QDoubleSpinBox;
    _planar.fovV->setMinimum(FovEpsilon);
    _planar.fovV->setMaximum(180.0 - FovEpsilon);
    _planar.fovV->setValue(DefaultFovShortEdge);
    _planar.fovV->setEnabled(false);
    _planar.fovV->setToolTip(vfovTip);
    _planar.fovV->setSizePolicy(
        QSizePolicy::MinimumExpanding,
        QSizePolicy::MinimumExpanding
    );
    layout->addWidget(_planar.fovV, 2, 1);

    _planar.buttonLockFov = new QPushButton;
    _planar.buttonLockFov->setIcon(_lockIcon);
    _planar.buttonLockFov->setToolTip(
        "Locks and scales the Horizontal & Vertical field-of-view to the ideal settings "
        "based on the provided aspect ratio"
    );
    _planar.buttonLockFov->setFocusPolicy(Qt::NoFocus);
    layout->addWidget(_planar.buttonLockFov, 1, 2, 2, 1);
    connect(
        _planar.buttonLockFov,
        &QPushButton::released,
        this,
        &WindowControl::onFovLockClicked
    );

    return widget;
}

QWidget* WindowControl::createFisheyeWidget() {
    //    Column 0   Column 1
    //  *------------*-----------*
    //  | { Informational text } |  Row 0
    //  | Quality    * [DDDDD>]  |  Row 1
    //  | [] Spout Output        |  Row 2
    //  *------------*-----------*
    
    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    _fisheye.labelInfo = new QLabel(
        "This projection provides a rendering in a format that is suitable for "
        "planetariums and other immersive environments. A field-of-view of 180 degrees "
        "is presented as a circular image in the center of the screen. For this "
        "projection a square window is suggested, but not necessary"
    );
    _fisheye.labelInfo->setObjectName("info");
    _fisheye.labelInfo->setWordWrap(true);
    layout->addWidget(_fisheye.labelInfo, 0, 0, 1, 2);

    _fisheye.labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times";
    _fisheye.labelQuality->setToolTip(qualityTip);
    layout->addWidget(_fisheye.labelQuality, 1, 0);

    _fisheye.quality = new QComboBox;
    _fisheye.quality->addItems(QualityTypes);
    _fisheye.quality->setToolTip(qualityTip);
    _fisheye.quality->setCurrentIndex(2);
    layout->addWidget(_fisheye.quality, 1, 1);

    _fisheye.spoutOutput = new QCheckBox("Spout Output");
    _fisheye.spoutOutput->setToolTip(
        "This projection method provides the ability to share the reprojected image "
        "using the Spout library.\nThis library only supports the Windows operating "
        "system. Spout makes it possible to make the rendered\nimages available to other "
        "real-time applications on the same machine for further processing"
    );
    layout->addWidget(_fisheye.spoutOutput, 2, 0, 1, 2);

    return widget;
}

QWidget* WindowControl::createSphericalMirrorWidget() {
    //    Column 0   Column 1
    //  *------------*-----------*
    //  | { Informational text } |  Row 0
    //  | Quality    * [DDDDD>]  |  Row 1
    //  *------------*-----------*
    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    _sphericalMirror.labelInfo = new QLabel(
        "This projection is rendering a image suite for use with a spherical mirror "
        "projection as described by Paul Bourke (http://paulbourke.net/dome/mirrordome/) "
        "and which is a low-cost yet effective way to provide content for a sphericalal "
        "display surface using a regular projector"
    );
    _sphericalMirror.labelInfo->setObjectName("info");
    _sphericalMirror.labelInfo->setWordWrap(true);
    layout->addWidget(_sphericalMirror.labelInfo, 0, 0, 1, 2);

    _sphericalMirror.labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times";
    _sphericalMirror.labelQuality->setToolTip(qualityTip);
    layout->addWidget(_sphericalMirror.labelQuality, 1, 0);

    _sphericalMirror.quality = new QComboBox;
    _sphericalMirror.quality->addItems(QualityTypes);
    _sphericalMirror.quality->setToolTip(qualityTip);
    _sphericalMirror.quality->setCurrentIndex(2);
    layout->addWidget(_sphericalMirror.quality, 1, 1);

    return widget;
}

QWidget* WindowControl::createCylindricalWidget() {
    //    Column 0   Column 1
    //  *------------*-----------*
    //  | { Informational text } |  Row 0
    //  | Quality    * [DDDDD>]  |  Row 1
    //  | HOffset    * [oooooo]  |  Row 2
    //  *------------*-----------*
    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    _cylindrical.labelInfo = new QLabel(
        "This projection type provides a cylindrical rendering that covers 360 degrees "
        "around the camera, which can be useful in immersive environments that are not "
        "spherical, but where, for example, all walls of a room are covered with "
        "projectors"
    );
    _cylindrical.labelInfo->setObjectName("info");
    _cylindrical.labelInfo->setWordWrap(true);
    layout->addWidget(_cylindrical.labelInfo, 0, 0, 1, 2);

    _cylindrical.labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times";
    _cylindrical.labelQuality->setToolTip(qualityTip);
    layout->addWidget(_cylindrical.labelQuality, 1, 0);

    _cylindrical.quality = new QComboBox;
    _cylindrical.quality->addItems(QualityTypes);
    _cylindrical.quality->setToolTip(qualityTip);
    _cylindrical.quality->setCurrentIndex(2);
    layout->addWidget(_cylindrical.quality, 1, 1);

    _cylindrical.labelHeightOffset = new QLabel("Height Offset");
    const QString heightTip = "Offsets the height from which the cylindrical projection "
        "is generated.\nThis is, in general, only necessary if the user position is "
        "offset and\ncountering that offset is desired in order to continue producing\n"
        "a 'standard' cylindrical projection";
    _cylindrical.labelHeightOffset->setToolTip(heightTip);
    layout->addWidget(_cylindrical.labelHeightOffset, 2, 0);

    _cylindrical.heightOffset = new QDoubleSpinBox;
    _cylindrical.heightOffset->setMinimum(-1000000.0);
    _cylindrical.heightOffset->setMaximum(1000000.0);
    _cylindrical.heightOffset->setValue(DefaultHeightOffset);
    _cylindrical.heightOffset->setToolTip(heightTip);
    layout->addWidget(_cylindrical.heightOffset, 2, 1);


    return widget;
}

QWidget* WindowControl::createEquirectangularWidget() {
    //    Column 0   Column 1
    //  *------------*-----------*
    //  | { Informational text } |  Row 0
    //  | Quality    * [DDDDD>]  |  Row 1
    //  | [] Spout Output        |  Row 2
    //  *------------*-----------*
    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    _equirectangular.labelInfo = new QLabel(
        "This projection provides the rendering as an image in equirectangular "
        "projection, which is a common display type for 360 surround video. When "
        "uploading a video in equirectangular projection to YouTube, for example, it "
        "will use it as a 360 video"
    );
    _equirectangular.labelInfo->setObjectName("info");
    _equirectangular.labelInfo->setWordWrap(true);
    layout->addWidget(_equirectangular.labelInfo, 0, 0, 1, 2);

    _equirectangular.labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times";
    _equirectangular.labelQuality->setToolTip(qualityTip);
    layout->addWidget(_equirectangular.labelQuality, 1, 0);

    _equirectangular.quality = new QComboBox;
    _equirectangular.quality->addItems(QualityTypes);
    _equirectangular.quality->setToolTip(qualityTip);
    _equirectangular.quality->setCurrentIndex(2);
    layout->addWidget(_equirectangular.quality, 1, 1);

    _equirectangular.spoutOutput = new QCheckBox("Spout Output");
    _equirectangular.spoutOutput->setToolTip(
        "This projection method provides the ability to share the reprojected image "
        "using the Spout library.\nThis library only supports the Windows operating "
        "system. Spout makes it possible to make the rendered\nimages available to other "
        "real-time applications on the same machine for further processing"
    );
    layout->addWidget(_equirectangular.spoutOutput, 2, 0, 1, 2);

    return widget;
}

void WindowControl::resetToDefaults() {
    //
    // Determine ideal window sizes
    constexpr float IdealScaleVerticalLines = 2.f / 3.f;
    constexpr int PrimaryMonitorIdx = 0;

    _windowDimensions = DefaultWindowSizes[_windowIndex];
    _offsetX->setValue(_windowDimensions.x());
    _offsetY->setValue(_windowDimensions.y());
    const float newHeight =
        _monitorResolutions[PrimaryMonitorIdx].height() * IdealScaleVerticalLines;
    const float newWidth = newHeight * IdealAspectRatio;
    _windowDimensions.setHeight(newHeight);
    _windowDimensions.setWidth(newWidth);
    _sizeX->setValue(static_cast<int>(newWidth));
    _sizeY->setValue(static_cast<int>(newHeight));

    //
    // Reset widgets
    _windowName->clear();
    if (_monitorResolutions.size() > 1) {
        _monitor->setCurrentIndex(_monitorIndexDefault);
    }
    _windowDecoration->setChecked(true);
    _fisheye.spoutOutput->setChecked(false);
    _equirectangular.spoutOutput->setChecked(false);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Planar));
    _planar.fovV->setValue(DefaultFovLongEdge);
    _planar.fovV->setValue(DefaultFovShortEdge);
    _cylindrical.heightOffset->setValue(DefaultHeightOffset);
    _fisheye.quality->setCurrentIndex(2);
    _sphericalMirror.quality->setCurrentIndex(2);
    _cylindrical.quality->setCurrentIndex(2);
    _equirectangular.quality->setCurrentIndex(2);
    emit windowChanged(_monitorIndexDefault, _windowIndex, _windowDimensions);
}

void WindowControl::setDimensions(QRectF newDims) {
    _windowDimensions = newDims;
    _sizeX->setValue(_windowDimensions.width());
    _sizeY->setValue(_windowDimensions.height());
    _offsetX->setValue(_windowDimensions.x());
    _offsetY->setValue(_windowDimensions.y());
}

void WindowControl::setMonitorSelection(int monitorIndex) {
    _monitor->setCurrentIndex(monitorIndex);
}

void WindowControl::showWindowLabel(bool show) {
    _windowNumber->setVisible(show);
}

void WindowControl::setWindowName(const std::string& windowName) {
    _windowName->setText(QString::fromStdString(windowName));
}

void WindowControl::setDecorationState(bool hasWindowDecoration) {
    _windowDecoration->setChecked(hasWindowDecoration);
}

sgct::config::Projections WindowControl::generateProjectionInformation() const {
    const ProjectionIndices type =
        static_cast<WindowControl::ProjectionIndices>(_projectionType->currentIndex());

    const bool isSpoutFisheye =
        type == ProjectionIndices::Fisheye && _fisheye.spoutOutput->isChecked();
    const bool isSpoutEquirectangular =
        type == ProjectionIndices::Equirectangular &&
        _equirectangular.spoutOutput->isChecked();

    using namespace sgct::config;
    switch (type) {
        case ProjectionIndices::Fisheye:
            if (isSpoutFisheye) {
                SpoutOutputProjection projection;
                projection.mapping = SpoutOutputProjection::Mapping::Fisheye;
                projection.quality = QualityValues[_fisheye.quality->currentIndex()];
                projection.mappingSpoutName = "OpenSpace";
                return projection;
            }
            else {
                FisheyeProjection projection;
                projection.quality = QualityValues[_fisheye.quality->currentIndex()];
                projection.fov = 180.f;
                projection.tilt = 0.f;
                return projection;
            }
        case ProjectionIndices::SphericalMirror:
            {
                SphericalMirrorProjection projection;
                projection.quality =
                    QualityValues[_sphericalMirror.quality->currentIndex()];
                return projection;
            }
        case ProjectionIndices::Cylindrical:
            {
                CylindricalProjection projection;
                projection.quality = QualityValues[_cylindrical.quality->currentIndex()];
                projection.heightOffset = _cylindrical.heightOffset->text().toFloat();
                return projection;
            }
        case ProjectionIndices::Equirectangular:
            if (isSpoutEquirectangular) {
                SpoutOutputProjection projection;
                projection.mapping = SpoutOutputProjection::Mapping::Equirectangular;
                projection.quality =
                    QualityValues[_equirectangular.quality->currentIndex()];
                projection.mappingSpoutName = "OpenSpace";
                return projection;
            }
            else {
                EquirectangularProjection projection;
                projection.quality =
                    QualityValues[_equirectangular.quality->currentIndex()];
                return projection;
            }
        case ProjectionIndices::Planar:
            {
                double fovH = _planar.fovH->value();
                fovH = std::clamp(fovH, FovEpsilon, 180.0 - FovEpsilon);
                
                double fovV = _planar.fovV->value();
                fovV = std::clamp(fovV, FovEpsilon, 180.0 - FovEpsilon);

                // The negative values for left & down are due to SGCT's convention
                PlanarProjection projection;
                projection.fov.right = fovH / 2.0;
                projection.fov.left = -projection.fov.right;
                projection.fov.up = fovV / 2.0;
                projection.fov.down = -projection.fov.up;
                return projection;
            }
        default:
            throw ghoul::MissingCaseException();
    }
}

void WindowControl::generateWindowInformation(sgct::config::Window& window) const {
    window.size = { _sizeX->text().toInt(), _sizeY->text().toInt() };
    window.monitor = _monitor->currentIndex();
    const QRect resolution = _monitorResolutions[_monitor->currentIndex()];
    window.pos = sgct::ivec2(
        resolution.x() + _offsetX->text().toInt(),
        resolution.y() + _offsetY->text().toInt()
    );

    sgct::config::Viewport vp;
    vp.isTracked = true;
    vp.position = sgct::vec2(0.f, 0.f);
    vp.size = sgct::vec2(1.f, 1.f);
    vp.projection = generateProjectionInformation();
    window.viewports.clear();
    window.viewports.push_back(vp);
    
    window.isDecorated = _windowDecoration->isChecked();
    if (!_windowName->text().isEmpty()) {
        window.name = _windowName->text().toStdString();
    }
}

void WindowControl::setProjectionPlanar(float hfov, float vfov) {
    _planar.fovH->setValue(hfov);
    _planar.fovV->setValue(vfov);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Planar));
}

void WindowControl::setProjectionFisheye(int quality, bool spoutOutput) {
    setQualityComboBoxFromLinesResolution(quality, _fisheye.quality);
    _fisheye.spoutOutput->setChecked(spoutOutput);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Fisheye));
}

void WindowControl::setProjectionSphericalMirror(int quality) {
    setQualityComboBoxFromLinesResolution(quality, _sphericalMirror.quality);
    _projectionType->setCurrentIndex(
        static_cast<int>(ProjectionIndices::SphericalMirror)
    );
}

void WindowControl::setProjectionCylindrical(int quality, float heightOffset) {
    setQualityComboBoxFromLinesResolution(quality, _cylindrical.quality);
    _cylindrical.heightOffset->setValue(heightOffset);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Cylindrical));
}

void WindowControl::setProjectionEquirectangular(int quality, bool spoutOutput) {
    setQualityComboBoxFromLinesResolution(quality, _equirectangular.quality);
    _equirectangular.spoutOutput->setChecked(spoutOutput);
    _projectionType->setCurrentIndex(
        static_cast<int>(ProjectionIndices::Equirectangular)
    );
}

void WindowControl::setVisibilityOfProjectionGui(bool enable) {
    _projectionType->setVisible(enable);
    _planar.labelInfo->setVisible(enable);
    _planar.fovH->setVisible(enable);
    _planar.labelFovH->setVisible(enable);
    _planar.fovV->setVisible(enable);
    _planar.labelFovV->setVisible(enable);
    _planar.buttonLockFov->setVisible(enable);
    _fisheye.labelInfo->setVisible(enable);
    _fisheye.quality->setVisible(enable);
    _fisheye.labelQuality->setVisible(enable);
    _fisheye.spoutOutput->setVisible(enable);
    _sphericalMirror.labelInfo->setVisible(enable);
    _sphericalMirror.quality->setVisible(enable);
    _sphericalMirror.labelQuality->setVisible(enable);
    _cylindrical.labelInfo->setVisible(enable);
    _cylindrical.heightOffset->setVisible(enable);
    _cylindrical.labelHeightOffset->setVisible(enable);
    _cylindrical.quality->setVisible(enable);
    _cylindrical.labelQuality->setVisible(enable);
    _equirectangular.labelInfo->setVisible(enable);
    _equirectangular.quality->setVisible(enable);
    _equirectangular.labelQuality->setVisible(enable);
    _equirectangular.spoutOutput->setVisible(enable);

    _projectionLabel->setVisible(!enable);
}

void WindowControl::setQualityComboBoxFromLinesResolution(int lines, QComboBox* combo) {
    ghoul_assert(combo, "Invalid pointer");
    for (unsigned int v = 0; v < nQualityTypes; ++v) {
        if (lines == QualityValues[v]) {
            combo->setCurrentIndex(v);
            break;
        }
    }
}

void WindowControl::onSizeXChanged(int newValue) {
    _windowDimensions.setWidth(newValue);
    if (_aspectRatioLocked) {
        const int updatedHeight = _windowDimensions.width() / _aspectRatioSize;
        _sizeY->blockSignals(true);
        _sizeY->setValue(updatedHeight);
        _sizeY->blockSignals(false);
        _windowDimensions.setHeight(updatedHeight);
    }
    emit windowChanged(_monitor->currentIndex(), _windowIndex, _windowDimensions);
    if (_fovLocked) {
        updatePlanarLockedFov();
    }
}

void WindowControl::onSizeYChanged(int newValue) {
    _windowDimensions.setHeight(newValue);
    if (_aspectRatioLocked) {
        const int updatedWidth = _windowDimensions.height() * _aspectRatioSize;
        _sizeX->blockSignals(true);
        _sizeX->setValue(updatedWidth);
        _sizeX->blockSignals(false);
        _windowDimensions.setWidth(updatedWidth);
    }
    emit windowChanged(_monitor->currentIndex(), _windowIndex, _windowDimensions);
    if (_fovLocked) {
        updatePlanarLockedFov();
    }
}

void WindowControl::onOffsetXChanged(int newValue) {
    const float prevWidth = _windowDimensions.width();
    _windowDimensions.setX(newValue);
    _windowDimensions.setWidth(prevWidth);
    emit windowChanged(_monitor->currentIndex(), _windowIndex, _windowDimensions);
}

void WindowControl::onOffsetYChanged(int newValue) {
    const float prevHeight = _windowDimensions.height();
    _windowDimensions.setY(newValue);
    _windowDimensions.setHeight(prevHeight);
    emit windowChanged(_monitor->currentIndex(), _windowIndex, _windowDimensions);
}

void WindowControl::onFullscreenClicked() {
    const QRect resolution = _monitorResolutions[_monitor->currentIndex()];

    _offsetX->setValue(0);
    _offsetY->setValue(0);
    _sizeX->setValue(resolution.width());
    _sizeY->setValue(resolution.height());
    _windowDecoration->setChecked(false);
}

void WindowControl::onProjectionChanged(int newSelection) const {
    const ProjectionIndices selected = static_cast<ProjectionIndices>(newSelection);
    _planar.widget->setVisible(selected == ProjectionIndices::Planar);
    _fisheye.widget->setVisible(selected == ProjectionIndices::Fisheye);
    _sphericalMirror.widget->setVisible(selected == ProjectionIndices::SphericalMirror);
    _cylindrical.widget->setVisible(selected == ProjectionIndices::Cylindrical);
    _equirectangular.widget->setVisible(selected == ProjectionIndices::Equirectangular);
}

void WindowControl::onAspectRatioLockClicked() {
    _aspectRatioLocked = !_aspectRatioLocked;
    if (_aspectRatioLocked) {
        _aspectRatioSize = _windowDimensions.width() / _windowDimensions.height();
    }
}

void WindowControl::onFovLockClicked() {
    _fovLocked = !_fovLocked;
    if (_fovLocked) {
        _planar.fovH->setEnabled(false);
        _planar.fovV->setEnabled(false);
        updatePlanarLockedFov();
    }
    else {
        _planar.fovH->setEnabled(true);
        _planar.fovV->setEnabled(true);
    }
    _planar.buttonLockFov->setIcon(_fovLocked ? _lockIcon : _unlockIcon);
}

void WindowControl::updatePlanarLockedFov() {
    const bool landscapeOrientation =
        (_windowDimensions.width() >= _windowDimensions.height());
    float aspectRatio = 0.f;
    if (landscapeOrientation) {
        aspectRatio = _windowDimensions.width() / _windowDimensions.height();
    }
    else {
        aspectRatio = _windowDimensions.height() / _windowDimensions.width();
    }

    float adjustedFov = 2.f * std::atan(aspectRatio * std::tan(DefaultFovShortEdge
        * std::numbers::pi_v<float> / 180.f / 2.f));
    // Convert to degrees and limit to 180°
    adjustedFov *= 180.f / std::numbers::pi_v<float>;
    adjustedFov = std::min(adjustedFov, 180.f);

    _planar.fovH->setValue(landscapeOrientation ? adjustedFov : DefaultFovShortEdge);
    _planar.fovV->setValue(landscapeOrientation ? DefaultFovShortEdge : adjustedFov);
}
