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

#include "sgctedit/windowcontrol.h"

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include "sgctedit/displaywindowunion.h"
#include "windowcolors.h"
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
    std::array<std::pair<int, std::string>, 10> Quality = {
        std::pair{ 256, "Low (256)" },
        std::pair{ 512, "Medium (512)" },
        std::pair{ 1024, "High (1K)" },
        std::pair{ 1536, "1.5K (1536)" },
        std::pair{ 2048, "2K (2048)" },
        std::pair{ 4096, "4K (4096)" },
        std::pair{ 8192, "8K (8192)" },
        std::pair{ 16384, "16K (16384)" },
        std::pair{ 32768, "32K (32768)" },
        std::pair{ 65536, "64K (65536)" }
    };

    constexpr std::array<QRect, 4> DefaultWindowSizes = {
        QRect(50, 50, 1280, 720),
        QRect(150, 150, 1280, 720),
        QRect(50, 50, 1280, 720),
        QRect(150, 150, 1280, 720)
    };

    enum class ProjectionIndices {
        Planar = 0,
        Fisheye,
        SphericalMirror,
        Cylindrical,
        Equirectangular,
        Blit
    };

    constexpr int LineEditWidthFixedWindowSize = 95;
    constexpr float DefaultFovLongEdge = 80.f;
    constexpr float DefaultFovShortEdge = 50.534f;
    constexpr float DefaultHeightOffset = 0.f;
    constexpr int MaxWindowSizePixels = 10000;
    constexpr double FovEpsilon = 0.00001;

    QList<QString> monitorNames(const std::vector<QRect>& resolutions) {
        std::array<std::string, 4> MonitorNames = {
            "Primary", "Secondary", "Tertiary", "Quaternary"
        };

        QList<QString> monitorNames;
        for (size_t i = 0; i < resolutions.size(); i++) {
            const std::string name = i < 4 ? MonitorNames[i] : std::format("{}th", i);
            const std::string fullName = std::format(
                "{} ({}x{})",
                name,
                resolutions[i].width(),
                resolutions[i].height()
            );
            monitorNames.push_back(QString::fromStdString(fullName));
        }
        return monitorNames;
    }

    QStringList qualityList() {
        QStringList res;
        for (const std::pair<int, std::string>& p : Quality) {
            res.append(QString::fromStdString(p.second));
        }
        return res;
    }

    int indexForQuality(int quality) {
        auto it = std::find_if(
            Quality.cbegin(),
            Quality.cend(),
            [quality](const std::pair<int, std::string>& p) { return p.first == quality; }
        );
        ghoul_assert(it != Quality.cend(), "Combobox has too many values");
        return static_cast<int>(std::distance(Quality.cbegin(), it));
    }

} // namespace

WindowControl::WindowControl(int monitorIndex, int windowIndex,
                             const std::vector<QRect>& monitorDims, QWidget* parent)
    : QWidget(parent)
    , _monitorIndexDefault(monitorIndex)
    , _windowIndex(windowIndex)
    , _monitorResolutions(monitorDims)
    , _lockIcon(":/images/outline_locked.png")
    , _unlockIcon(":/images/outline_unlocked.png")
{
    //      Col 0      Col 1    Col 2     Col 3    Col 4    Col 5   Col 6   Col 7
    //  *----------*----------*-------*----------*-------*--------*-------*-------*
    //  |                                   Window {n}                            | R0
    //  | Name     * [oooooooooooooooooooooooooooooooooooooooooooooooooooooooooo] | R1
    //  | Monitor  * DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD> | R2
    //  | Size     * [xxxxxx] *    x  * [yyyyyy] *  px   * <lock> * < Set to      | R3
    //  | Offset   * [xxxxxx] *    ,  * [yyyyyy] *  px   *        *   Fullscreen> | R4
    //  | [] Window Decoration                    [] Render 2D                    | R5
    //  | [] Spout Output                         [] Render 3D                    | R6
    //  | ~~~~~~~~~~~~~~~~~~~~~~~~~Projection components~~~~~~~~~~~~~~~~~~~~~~~~~ | R7
    //  *----------*----------*-------*----------*-------*--------*-------*-------*

    QGridLayout* layout = new QGridLayout(this);
    const QMargins margins = layout->contentsMargins();
    layout->setContentsMargins(margins.left(), 0, margins.right(), 0);
    layout->setColumnStretch(6, 1);
    layout->setRowStretch(8, 1);

    //
    // Window title
    _windowNumber = new QLabel("Window " + QString::number(_windowIndex + 1));
    QColor windowColor = colorForWindow(_windowIndex);
    _windowNumber->setStyleSheet(QString::fromStdString(std::format(
        "QLabel {{ color : #{:02x}{:02x}{:02x}; }}",
        windowColor.red(), windowColor.green(), windowColor.blue()
    )));
    layout->addWidget(_windowNumber, 0, 0, 1, 8, Qt::AlignCenter);

    //
    // Name
    {
        const QString tip = "The name for the window (displayed in title bar)";

        QLabel* labelName = new QLabel("Name");
        labelName->setToolTip(tip);
        layout->addWidget(labelName, 1, 0);

        _windowName = new QLineEdit;
        _windowName->setToolTip(tip);
        layout->addWidget(_windowName, 1, 1, 1, 3);
    }
    const QString tip = "The monitor where this window is located";

    //
    // Monitor
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
        // We only add the monitor dropdown menu if we are running on a system with
        // multiple monitors. We are still creating the combobox to guard against
        // potential nullpointer accesses elsewhere in the code
        QLabel* labelLocation = new QLabel("Monitor");
        labelLocation->setToolTip(tip);
        layout->addWidget(labelLocation, 1, 4);

        layout->addWidget(_monitor, 1, 5, 1, 4);
    }

    //
    // Window size
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

    //
    // Position
    {
        QLabel* offset = new QLabel("Position");
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

    //
    // Fullscreen button
    {
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
        layout->addWidget(setFullscreen, 3, 6, 2, 2);
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
        layout->addWidget(_windowDecoration, 5, 0, 1, 4);
    }

    //
    // Spout output
    {
        _spoutOutput = new QCheckBox("Spout Output");
        _spoutOutput->setToolTip(
            "Share this window using the Spout library.\nThis library only supports the "
            "Windows operating system. Spout makes it possible to make the rendered\n"
            "images available to other real-time applications on the same machine for "
            "further processing"
        );
        layout->addWidget(_spoutOutput, 6, 0, 1, 4);
    }

    //
    // Render 2D & 3D
    {
        _render2D = new QCheckBox("Render GUI and Overlays");
        _render2D->setToolTip(
            "Determines whether any overlays should be\nrendered in this window. "
            "Overlays in this case are\nthe user interface, dashboard information, and "
            "other\nelements that are only useful for a pilot."
        );
        layout->addWidget(_render2D, 5, 4, 1, 4);

        _render3D = new QCheckBox("Render Scene");
        _render3D->setToolTip(
            "Determines whether the main 3D scene should be\nrendered in this window, "
            "like 3D models, the planets, stars, etc."
        );
        connect(
            _render3D, &QCheckBox::clicked,
            [this](bool checked) {
                _projectionGroup->setVisible(checked);
            }
        );
        layout->addWidget(_render3D, 6, 4, 1, 4);
    }

    //
    // Projections
    {
        _projectionGroup = new QFrame;
        _projectionGroup->setVisible(true);
        _projectionGroup->setFrameStyle(QFrame::StyledPanel | QFrame::Plain);

        //
        // Projection combobox
        QBoxLayout* projectionLayout = new QVBoxLayout(_projectionGroup);
        projectionLayout->setContentsMargins(0, 0, 0, 0);
        projectionLayout->setSpacing(0);

        _projectionType = new QComboBox;
        _projectionType->addItems({
            "Planar Projection", "Fisheye", "Spherical Mirror Projection",
            "Cylindrical Projection", "Equirectangular Projection", "Copy Window Contents"
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

        _blit.widget = createBlitWidget();
        projectionLayout->addWidget(_blit.widget);

        // We need to trigger this once to ensure that all of the defaults are correct
        onProjectionChanged(0);

        layout->addWidget(_projectionGroup, 7, 0, 1, 8);
    }

    resetToDefaults();
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

    QLabel* labelInfo = new QLabel(
        "This projection type is the 'regular' projection with a horizontal and a "
        "vertical field of view, given in degrees. The wider the field of view, the "
        "more content is shown at the same time, but everything becomes smaller. Very "
        "large values will introduce distortions on the corners."
    );
    labelInfo->setObjectName("info");
    labelInfo->setWordWrap(true);
    layout->addWidget(labelInfo, 0, 0, 1, 3);

    QLabel* labelFovH = new QLabel("Horizontal FOV");
    const QString hfovTip =
        "The total horizontal field of view of the viewport (degrees)";
    labelFovH->setToolTip(hfovTip);
    layout->addWidget(labelFovH, 1, 0);

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

    QLabel* labelFovV = new QLabel("Vertical FOV");
    const QString vfovTip = "The total vertical field of view of the viewport (degrees). "
        "Internally,\nthe values for 'up' & 'down' will each be half this value";
    labelFovV->setToolTip(vfovTip);
    layout->addWidget(labelFovV, 2, 0);

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
    //  | Tilt       * [oooooo]  |  Row 2
    //  *------------*-----------*
    
    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    QLabel* labelInfo = new QLabel(
        "This projection provides a rendering in a format that is suitable for "
        "planetariums and other immersive environments. A field-of-view of 180 degrees "
        "is presented as a circular image in the center of the screen. For this "
        "projection a square window is suggested, but not necessary."
    );
    labelInfo->setObjectName("info");
    labelInfo->setWordWrap(true);
    layout->addWidget(labelInfo, 0, 0, 1, 2);

    QLabel* labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times.";
    labelQuality->setToolTip(qualityTip);
    layout->addWidget(labelQuality, 1, 0);

    _fisheye.quality = new QComboBox;
    _fisheye.quality->addItems(qualityList());
    _fisheye.quality->setToolTip(qualityTip);
    _fisheye.quality->setCurrentIndex(2);
    layout->addWidget(_fisheye.quality, 1, 1);

    QLabel* labelTilt = new QLabel("Tilt");
    const QString tiltTip = "Determines the tilt (in degrees) of the fisheye rendering. "
        "Changing this value will cause the entire rendering to be tilted by the set "
        "number of degrees.";
    labelTilt->setToolTip(tiltTip);
    layout->addWidget(labelTilt, 2, 0);

    _fisheye.tilt = new QDoubleSpinBox;
    _fisheye.tilt->setToolTip(tiltTip);
    _fisheye.tilt->setMinimum(-180.0);
    _fisheye.tilt->setMaximum(180.0);
    layout->addWidget(_fisheye.tilt, 2, 1);

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

    QLabel* labelInfo = new QLabel(
        "This projection is rendering a image suite for use with a spherical mirror "
        "projection as described by Paul Bourke (http://paulbourke.net/dome/mirrordome/) "
        "and which is a low-cost yet effective way to provide content for a sphericalal "
        "display surface using a regular projector."
    );
    labelInfo->setObjectName("info");
    labelInfo->setWordWrap(true);
    layout->addWidget(labelInfo, 0, 0, 1, 2);

    QLabel* labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times";
    labelQuality->setToolTip(qualityTip);
    layout->addWidget(labelQuality, 1, 0);

    _sphericalMirror.quality = new QComboBox;
    _sphericalMirror.quality->addItems(qualityList());
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

    QLabel* labelInfo = new QLabel(
        "This projection type provides a cylindrical rendering that covers 360 degrees "
        "around the camera, which can be useful in immersive environments that are not "
        "spherical, but where, for example, all walls of a room are covered with "
        "projectors."
    );
    labelInfo->setObjectName("info");
    labelInfo->setWordWrap(true);
    layout->addWidget(labelInfo, 0, 0, 1, 2);

    QLabel* labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times";
    labelQuality->setToolTip(qualityTip);
    layout->addWidget(labelQuality, 1, 0);

    _cylindrical.quality = new QComboBox;
    _cylindrical.quality->addItems(qualityList());
    _cylindrical.quality->setToolTip(qualityTip);
    _cylindrical.quality->setCurrentIndex(2);
    layout->addWidget(_cylindrical.quality, 1, 1);

    QLabel* labelHeightOffset = new QLabel("Height Offset");
    const QString heightTip = "Offsets the height from which the cylindrical projection "
        "is generated.\nThis is, in general, only necessary if the user position is "
        "offset and\ncountering that offset is desired in order to continue producing\n"
        "a 'standard' cylindrical projection";
    labelHeightOffset->setToolTip(heightTip);
    layout->addWidget(labelHeightOffset, 2, 0);

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
    //  *------------*-----------*
    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    QLabel* labelInfo = new QLabel(
        "This projection provides the rendering as an image in equirectangular "
        "projection, which is a common display type for 360 surround video. When "
        "uploading a video in equirectangular projection to YouTube, for example, it "
        "will use it as a 360 video."
    );
    labelInfo->setObjectName("info");
    labelInfo->setWordWrap(true);
    layout->addWidget(labelInfo, 0, 0, 1, 2);

    QLabel* labelQuality = new QLabel("Quality");
    const QString qualityTip = "Determines the pixel resolution of the projection "
        "rendering. The higher resolution,\nthe better the rendering quality, but at the "
        "expense of increased rendering times";
    labelQuality->setToolTip(qualityTip);
    layout->addWidget(labelQuality, 1, 0);

    _equirectangular.quality = new QComboBox;
    _equirectangular.quality->addItems(qualityList());
    _equirectangular.quality->setToolTip(qualityTip);
    _equirectangular.quality->setCurrentIndex(2);
    layout->addWidget(_equirectangular.quality, 1, 1);

    return widget;
}

QWidget* WindowControl::createBlitWidget() {
    //    Column 0   Column 1
    //  *------------*-----------*
    //  | { Informational text } |  Row 0
    //  | Window ID  * [DDDDD>]  |  Row 1
    //  | { Unavailability }     |  Row 2
    //  *------------*-----------*

    QWidget* widget = new QWidget;
    QGridLayout* layout = new QGridLayout(widget);
    layout->setColumnStretch(1, 1);

    QLabel* labelInfo = new QLabel(
        "This projection type will reuse the contents of another window. This can be "
        "useful for GUI windows that should show the 3D scene, but not incur the cost of "
        "rendering the scene twice. Note that the contents of the rendering will be "
        "copied in their entirety, which means that if the rendering windows have "
        "different aspect ratios, the image in the receiving window will be stretched."
    );
    labelInfo->setObjectName("info");
    labelInfo->setWordWrap(true);
    layout->addWidget(labelInfo, 0, 0, 1, 2);

    QLabel* labelBlitId = new QLabel("Window ID");
    const QString blitTip = "Determines the window from which to copy the contents.";
    labelBlitId->setToolTip(blitTip);
    layout->addWidget(labelBlitId, 1, 0);

    _blit.windowId = new QComboBox;
    _blit.windowId->setToolTip(blitTip);
    layout->addWidget(_blit.windowId, 1, 1);

    _blit.unavailable = new QLabel(
        "It is only possible to copy the contents of another window if at least two "
        "windows have been created. Add a second window before selecting this projection "
        "type."
    );
    _blit.unavailable->setWordWrap(true);
    layout->addWidget(_blit.unavailable, 2, 0, 1, 2);

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
    _spoutOutput->setChecked(false);
    _render2D->setChecked(true);
    _render3D->setChecked(true);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Planar));
    _planar.fovV->setValue(DefaultFovLongEdge);
    _planar.fovV->setValue(DefaultFovShortEdge);
    _cylindrical.heightOffset->setValue(DefaultHeightOffset);
    _fisheye.quality->setCurrentIndex(2);
    _fisheye.tilt->setValue(0.0);
    _sphericalMirror.quality->setCurrentIndex(2);
    _cylindrical.quality->setCurrentIndex(2);
    _equirectangular.quality->setCurrentIndex(2);
    _blit.windowId->setCurrentIndex(0);
    emit windowChanged(_monitorIndexDefault, _windowIndex, _windowDimensions);
}

void WindowControl::setDimensions(int x, int y, int width, int height) {
    _windowDimensions = QRect(x, y, width, height);
    _sizeX->setValue(width);
    _sizeY->setValue(height);
    _offsetX->setValue(x);
    _offsetY->setValue(y);
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

void WindowControl::setSpoutOutputState(bool shouldSpoutOutput) {
    _spoutOutput->setChecked(shouldSpoutOutput);
}

void WindowControl::setRender2D(bool state) {
    _render2D->setChecked(state);
}

void WindowControl::setRender3D(bool state) {
    _render3D->setChecked(state);
}

void WindowControl::generateWindowInformation(sgct::config::Window& window) const {
    window.size = { _sizeX->value(), _sizeY->value() };
    window.monitor = _monitor->currentIndex();
    const QRect resolution = _monitorResolutions[_monitor->currentIndex()];
    window.pos = sgct::ivec2(
        resolution.x() + _offsetX->value(),
        resolution.y() + _offsetY->value()
    );
    window.draw2D = _render2D->isChecked();
    window.draw3D = _render3D->isChecked();

    window.isDecorated = _windowDecoration->isChecked();
    if (_spoutOutput->isChecked()) {
        window.spout = sgct::config::Window::Spout{
            .enabled = true
        };
    }
    if (!_windowName->text().isEmpty()) {
        window.name = _windowName->text().toStdString();
    }

    window.viewports.clear();

    // The rest of this function is just specifying the rendering, which we can skip if we
    // don't want to render 3D anyway
    if (!window.draw3D) {
        return;
    }

    sgct::config::Viewport vp;
    vp.isTracked = true;
    vp.position = sgct::vec2(0.f, 0.f);
    vp.size = sgct::vec2(1.f, 1.f);

    switch (static_cast<ProjectionIndices>(_projectionType->currentIndex())) {
        case ProjectionIndices::Fisheye:
            vp.projection = sgct::config::FisheyeProjection {
                .fov = 180.f,
                .quality = Quality[_fisheye.quality->currentIndex()].first,
                .tilt = static_cast<float>(_fisheye.tilt->value())
            };
            break;
        case ProjectionIndices::SphericalMirror:
            vp.projection = sgct::config::SphericalMirrorProjection {
                .quality = Quality[_sphericalMirror.quality->currentIndex()].first
            };
            break;
        case ProjectionIndices::Cylindrical:
            vp.projection = sgct::config::CylindricalProjection {
                .quality = Quality[_cylindrical.quality->currentIndex()].first,
                .heightOffset = static_cast<float>(_cylindrical.heightOffset->value())
            };
            break;
        case ProjectionIndices::Equirectangular:
            vp.projection = sgct::config::EquirectangularProjection {
                .quality = Quality[_equirectangular.quality->currentIndex()].first
            };
            break;
        case ProjectionIndices::Blit:
            // We have to subtract here as SGCT uses 0-indexing, but we present it to the
            // user as 1-indexing
            window.blitWindowId = _blit.windowId->currentText().toInt() - 1;
            window.draw3D = false;

            // We are falling through the planar value on purpose as for a variety of
            // reasons requires a projection to be defined even when we are blitting the
            // contents of another window.
            [[fallthrough]];
        case ProjectionIndices::Planar:
            {
                double fovH = _planar.fovH->value();
                fovH = std::clamp(fovH, FovEpsilon, 180.0 - FovEpsilon);
                
                double fovV = _planar.fovV->value();
                fovV = std::clamp(fovV, FovEpsilon, 180.0 - FovEpsilon);

                // The negative values for left & down are due to SGCT's convention
                sgct::config::PlanarProjection projection;
                projection.fov.right = fovH / 2.0;
                projection.fov.left = -projection.fov.right;
                projection.fov.up = fovV / 2.0;
                projection.fov.down = -projection.fov.up;
                vp.projection = projection;
                break;
            }
    }
    window.viewports.push_back(vp);
}

void WindowControl::setProjectionPlanar(float hfov, float vfov) {
    _planar.fovH->setValue(hfov);
    _planar.fovV->setValue(vfov);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Planar));
}

void WindowControl::setProjectionFisheye(int quality, float tilt) {
    _fisheye.quality->setCurrentIndex(indexForQuality(quality));
    _fisheye.tilt->setValue(tilt);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Fisheye));
}

void WindowControl::setProjectionSphericalMirror(int quality) {
    _sphericalMirror.quality->setCurrentIndex(indexForQuality(quality));
    _projectionType->setCurrentIndex(
        static_cast<int>(ProjectionIndices::SphericalMirror)
    );
}

void WindowControl::setProjectionCylindrical(int quality, float heightOffset) {
    _cylindrical.quality->setCurrentIndex(indexForQuality(quality));
    _cylindrical.heightOffset->setValue(heightOffset);
    _projectionType->setCurrentIndex(static_cast<int>(ProjectionIndices::Cylindrical));
}

void WindowControl::setProjectionEquirectangular(int quality) {
    _equirectangular.quality->setCurrentIndex(indexForQuality(quality));
    _projectionType->setCurrentIndex(
        static_cast<int>(ProjectionIndices::Equirectangular)
    );
}

void WindowControl::setProjectionBlit(int windowBlitId) {
    // We add 1 here as SGCT uses a 0-indexing for the window idx, but we present it to
    // the user as a 1-indexing
    int idx = _blit.windowId->findText(QString::number(windowBlitId + 1));
    ghoul_assert(idx != -1, "Could not find window blit id");
    _blit.windowId->setCurrentIndex(idx);
    _projectionType->setCurrentIndex(
        static_cast<int>(ProjectionIndices::Blit)
    );
}

void WindowControl::updateWindowCount(int newWindowCount) {
    QString currentIdx = _blit.windowId->currentText();
    _blit.windowId->clear();
    for (int idx = 0; idx < newWindowCount; idx++) {
        if (idx == _windowIndex) {
            continue;
        }
        _blit.windowId->addItem(QString::number(idx + 1));
    }
    _blit.windowId->setCurrentText(currentIdx);

    // Set the correct visibility
    _blit.unavailable->setVisible(newWindowCount == 1);
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
    _blit.widget->setVisible(selected == ProjectionIndices::Blit);
}

void WindowControl::onAspectRatioLockClicked() {
    _aspectRatioLocked = !_aspectRatioLocked;
    if (_aspectRatioLocked) {
        const float w = static_cast<float>(_windowDimensions.width());
        const float h = static_cast<float>(_windowDimensions.height());
        _aspectRatioSize = w / h;
    }
}

void WindowControl::onFovLockClicked() {
    _fovLocked = !_fovLocked;
    _planar.fovH->setEnabled(!_fovLocked);
    _planar.fovV->setEnabled(!_fovLocked);
    if (_fovLocked) {
        updatePlanarLockedFov();
    }
    _planar.buttonLockFov->setIcon(_fovLocked ? _lockIcon : _unlockIcon);
}

void WindowControl::updatePlanarLockedFov() {
    const float w = static_cast<float>(_windowDimensions.width());
    const float h = static_cast<float>(_windowDimensions.height());
    const bool landscapeOrientation =
        (_windowDimensions.width() >= _windowDimensions.height());
    const float aspectRatio = landscapeOrientation ? (w / h) : (h / w);

    float adjustedFov = 2.f * std::atan(aspectRatio * std::tan(DefaultFovShortEdge
        * std::numbers::pi_v<float> / 180.f / 2.f));
    // Convert to degrees and limit to 180°
    adjustedFov *= 180.f / std::numbers::pi_v<float>;
    adjustedFov = std::min(adjustedFov, 180.f);

    _planar.fovH->setValue(landscapeOrientation ? adjustedFov : DefaultFovShortEdge);
    _planar.fovV->setValue(landscapeOrientation ? DefaultFovShortEdge : adjustedFov);
}
