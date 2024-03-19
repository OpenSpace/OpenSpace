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

#include "profile/cameradialog.h"

#include "profile/line.h"
#include <openspace/navigation/navigationstate.h>
#include <QDialogButtonBox>
#include <QDoubleValidator>
#include <QFileDialog>
#include <QFrame>
#include <QGridLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QTabWidget>

namespace {
    constexpr int CameraTypeNode = 0;
    constexpr int CameraTypeNav = 1;
    constexpr int CameraTypeGeo = 2;

    template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
    template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

    bool inNumericalRange(QLineEdit* le, float min, float max) {
        const QString s = le->text();
        bool validConversion = false;
        const float value = s.toFloat(&validConversion);
        if (!validConversion) {
            return false;
        }
        return (value >= min) && (value <= max);
    }

    bool isNumericalLargerThan(QLineEdit* le, float limit) {
        const QString s = le->text();
        bool validConversion = false;
        const float value = s.toFloat(&validConversion);
        if (!validConversion) {
            return false;
        }
        return value > limit;
    }
} // namespace

CameraDialog::CameraDialog(QWidget* parent,
                           std::optional<openspace::Profile::CameraType>* camera)
    : QDialog(parent)
    , _camera(camera)
{
    setWindowTitle("Set Camera Position");
    createWidgets();

    if (_camera->has_value()) {
        const openspace::Profile::CameraType& type = **_camera;
        std::visit(overloaded {
            [this](const openspace::Profile::CameraGoToNode& node) {
                _tabWidget->setCurrentIndex(CameraTypeNode);
                _nodeState.anchor->setText(QString::fromStdString(node.anchor));
                if (node.height.has_value()) {
                    _nodeState.height->setText(QString::number(*node.height, 'g', 17));
                }
                tabSelect(CameraTypeNode);
            },
            [this](const openspace::Profile::CameraNavState& nav) {
                _tabWidget->setCurrentIndex(CameraTypeNav);
                _navState.anchor->setText(QString::fromStdString(nav.anchor));
                _navState.aim->setText(QString::fromStdString(*nav.aim));
                _navState.refFrame->setText(QString::fromStdString(nav.referenceFrame));
                _navState.positionX->setText(QString::number(nav.position.x, 'g', 17));
                _navState.positionY->setText(QString::number(nav.position.y, 'g', 17));
                _navState.positionZ->setText(QString::number(nav.position.z, 'g', 17));
                if (nav.up.has_value()) {
                    _navState.upX->setText(QString::number(nav.up.value().x, 'g', 17));
                    _navState.upY->setText(QString::number(nav.up.value().y, 'g', 17));
                    _navState.upZ->setText(QString::number(nav.up.value().z, 'g', 17));
                }
                else {
                    _navState.upX->clear();
                    _navState.upY->clear();
                    _navState.upZ->clear();
                }
                if (nav.yaw.has_value()) {
                    _navState.yaw->setText(QString::number(*nav.yaw, 'g', 17));
                }
                else {
                    _navState.yaw->clear();
                }
                if (nav.pitch.has_value()) {
                    _navState.pitch->setText(QString::number(*nav.pitch, 'g', 17));
                }
                else {
                    _navState.pitch->clear();
                }
                tabSelect(CameraTypeNav);
            },
            [this](const openspace::Profile::CameraGoToGeo& geo) {
                _tabWidget->setCurrentIndex(CameraTypeGeo);
                _geoState.anchor->setText(QString::fromStdString(geo.anchor));
                _geoState.latitude->setText(QString::number(geo.latitude, 'g', 17));
                _geoState.longitude->setText(QString::number(geo.longitude, 'g', 17));
                if (geo.altitude.has_value()) {
                    _geoState.altitude->setText(QString::number(*geo.altitude, 'g', 17));
                }
                else {
                    _geoState.altitude->clear();
                }
                tabSelect(CameraTypeGeo);
            }
        }, type);
    }
    else {
        _tabWidget->setCurrentIndex(CameraTypeNode);

        _nodeState.anchor->clear();
        _nodeState.height->clear();

        _navState.anchor->clear();
        _navState.aim->clear();
        _navState.refFrame->clear();
        _navState.positionX->clear();
        _navState.positionY->clear();
        _navState.positionZ->clear();
        _navState.upX->clear();
        _navState.upY->clear();
        _navState.upZ->clear();
        _navState.yaw->clear();
        _navState.pitch->clear();

        _geoState.anchor->clear();
        _geoState.latitude->clear();
        _geoState.longitude->clear();
        _geoState.altitude->clear();
    }
}

void CameraDialog::createWidgets() {
    QBoxLayout* layout = new QVBoxLayout(this);
    _tabWidget = new QTabWidget;
    connect(_tabWidget, &QTabWidget::tabBarClicked, this, &CameraDialog::tabSelect);
    _tabWidget->addTab(createNodeWidget(), "Scene Graph Node");
    _tabWidget->addTab(createNavStateWidget(), "Navigation State");
    _tabWidget->addTab(createGeoWidget(), "Geo State");
    layout->addWidget(_tabWidget);

    layout->addWidget(new Line);

    {
        QBoxLayout* footerLayout = new QHBoxLayout;

        _errorMsg = new QLabel;
        _errorMsg->setObjectName("error-message");
        _errorMsg->setWordWrap(true);
        footerLayout->addWidget(_errorMsg);

        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(buttons, &QDialogButtonBox::accepted, this, &CameraDialog::approved);
        connect(buttons, &QDialogButtonBox::rejected, this, &CameraDialog::reject);
        footerLayout->addWidget(buttons);

        layout->addLayout(footerLayout);
    }
}

QWidget* CameraDialog::createNodeWidget() {
    QWidget* tab = new QWidget;
    QVBoxLayout* mainLayout = new QVBoxLayout(tab);

    QLabel* description = new QLabel;
    description->setWordWrap(true);
    description->setAlignment(Qt::AlignCenter);
    description->setObjectName("camera-description");
    description->setText(
        "Set the camera position based on a given scene graph node. \n \n "
        "Automatically computes a position that frames the object, and if possible, "
        "shows it from Sun-lit side. The exact position might change between start-ups."
    );

    mainLayout->addWidget(description);

    {
        QWidget* box = new QWidget;
        QGridLayout* layout = new QGridLayout(box);

        layout->addWidget(new QLabel("Anchor Node:"), 0, 0);
        _nodeState.anchor = new QLineEdit;
        _nodeState.anchor->setToolTip("Anchor camera to this scene graph node");
        layout->addWidget(_nodeState.anchor, 0, 1);

        layout->addWidget(new QLabel("Height [m]:"), 1, 0);
        _nodeState.height = new QLineEdit;
        _nodeState.height->setValidator(new QDoubleValidator);
        _nodeState.height->setToolTip(
            "If specified, the camera will placed at the given height away from object. "
            "The height is computed from the bounding sphere of the anchor node"
        );
        _nodeState.height->setPlaceholderText("optional");
        layout->addWidget(_nodeState.height, 1, 1);

        mainLayout->addWidget(box);
    }

    // Add spacer at the end to prevent previous components from growing as much
    mainLayout->addSpacerItem(
        new QSpacerItem(0, 0, QSizePolicy::Expanding, QSizePolicy::Expanding)
    );

    return tab;
}

QWidget* CameraDialog::createNavStateWidget() {
    QWidget* tab = new QWidget;
    QVBoxLayout* mainLayout = new QVBoxLayout(tab);

    QLabel* description = new QLabel;
    description->setWordWrap(true);
    description->setAlignment(Qt::AlignCenter);
    description->setObjectName("camera-description");
    description->setText(
        "Set the camera position from a navigation state. \n"
        "Allows for setting an exact view or camera position at start-up."
    );

    mainLayout->addWidget(description);

    {
        QWidget* box = new QWidget;
        QGridLayout* layout = new QGridLayout(box);

        layout->addWidget(new QLabel("Anchor:"), 0, 0);
        _navState.anchor = new QLineEdit;
        _navState.anchor->setToolTip("Anchor camera to this node");
        layout->addWidget(_navState.anchor, 0, 1);

        layout->addWidget(new QLabel("Aim:"), 1, 0);
        _navState.aim = new QLineEdit;
        _navState.aim->setToolTip(
            "If specified, camera will be aimed at this node while keeping the anchor node "
            "in the same view location"
        );
        _navState.aim->setPlaceholderText("optional");
        layout->addWidget(_navState.aim, 1, 1);

        layout->addWidget(new QLabel("Reference Frame:"), 2, 0);
        _navState.refFrame = new QLineEdit;
        _navState.refFrame->setToolTip("Camera location in reference to this frame");
        _navState.refFrame->setPlaceholderText("optional");
        layout->addWidget(_navState.refFrame, 2, 1);

        layout->addWidget(new QLabel("Position:"), 3, 0);
        {
            QWidget* posBox = new QWidget;
            QBoxLayout* posLayout = new QHBoxLayout(posBox);
            posLayout->setContentsMargins(0, 0, 0, 0);
            posLayout->addWidget(new QLabel("X [m]"));
            _navState.positionX = new QLineEdit;
            _navState.positionX->setValidator(new QDoubleValidator);
            _navState.positionX->setToolTip("Camera position vector (x) [m]");
            posLayout->addWidget(_navState.positionX);

            posLayout->addWidget(new QLabel("Y [m]"));
            _navState.positionY = new QLineEdit;
            _navState.positionY->setValidator(new QDoubleValidator);
            _navState.positionY->setToolTip("Camera position vector (y) [m]");
            posLayout->addWidget(_navState.positionY);

            posLayout->addWidget(new QLabel("Z [m]"));
            _navState.positionZ = new QLineEdit;
            _navState.positionZ->setValidator(new QDoubleValidator);
            _navState.positionZ->setToolTip("Camera position vector (z) [m]");
            posLayout->addWidget(_navState.positionZ);
            layout->addWidget(posBox, 3, 1);
        }

        layout->addWidget(new QLabel("Up:"), 4, 0);
        {
            QWidget* upBox = new QWidget;
            QBoxLayout* upLayout = new QHBoxLayout(upBox);
            upLayout->setContentsMargins(0, 0, 0, 0);
            upLayout->addWidget(new QLabel("X"));
            _navState.upX = new QLineEdit;
            _navState.upX->setValidator(new QDoubleValidator);
            _navState.upX->setToolTip("Camera up vector (x)");
            _navState.upX->setPlaceholderText("semioptional");
            upLayout->addWidget(_navState.upX);

            upLayout->addWidget(new QLabel("Y"));
            _navState.upY = new QLineEdit;
            _navState.upY->setValidator(new QDoubleValidator);
            _navState.upY->setToolTip("Camera up vector (y)");
            _navState.upY->setPlaceholderText("semioptional");
            upLayout->addWidget(_navState.upY);

            upLayout->addWidget(new QLabel("Z"));
            _navState.upZ = new QLineEdit;
            _navState.upZ->setValidator(new QDoubleValidator);
            _navState.upZ->setToolTip("Camera up vector (z)");
            _navState.upZ->setPlaceholderText("semioptional");
            upLayout->addWidget(_navState.upZ);
            layout->addWidget(upBox, 4, 1);
        }

        layout->addWidget(new QLabel("Yaw angle:"), 5, 0);
        _navState.yaw = new QLineEdit;
        _navState.yaw->setValidator(new QDoubleValidator);
        _navState.yaw->setToolTip("Yaw angle +/- 360 degrees");
        _navState.yaw->setPlaceholderText("optional");
        layout->addWidget(_navState.yaw, 5, 1);

        layout->addWidget(new QLabel("Pitch angle:"), 6, 0);
        _navState.pitch = new QLineEdit;
        _navState.pitch->setValidator(new QDoubleValidator);
        _navState.pitch->setToolTip("Pitch angle +/- 360 degrees");
        _navState.pitch->setPlaceholderText("optional");
        layout->addWidget(_navState.pitch, 6, 1);

        mainLayout->addWidget(box);
    }

    QPushButton* loadFile = new QPushButton("Load state from file");
    loadFile->setIcon(loadFile->style()->standardIcon(QStyle::SP_FileIcon));
    connect(
        loadFile, &QPushButton::clicked,
        [this]() {
            const QString file = QFileDialog::getOpenFileName(
                this,
                "Select navigate state file"
            );

            std::ifstream f = std::ifstream(file.toStdString());
            const std::string contents = std::string(
                std::istreambuf_iterator<char>(f),
                std::istreambuf_iterator<char>()
            );
            const nlohmann::json json = nlohmann::json::parse(contents);

            using namespace openspace::interaction;
            NavigationState state = NavigationState(json);

            _navState.anchor->setText(QString::fromStdString(state.anchor));
            _navState.aim->setText(QString::fromStdString(state.aim));
            _navState.refFrame->setText(QString::fromStdString(state.referenceFrame));
            _navState.positionX->setText(QString::number(state.position.x));
            _navState.positionY->setText(QString::number(state.position.y));
            _navState.positionZ->setText(QString::number(state.position.z));

            if (state.up.has_value()) {
                _navState.upX->setText(QString::number(state.up->x));
                _navState.upY->setText(QString::number(state.up->y));
                _navState.upZ->setText(QString::number(state.up->z));
            }

            _navState.yaw->setText(QString::number(state.yaw));
            _navState.pitch->setText(QString::number(state.pitch));
        }
    );
    mainLayout->addWidget(loadFile);

    return tab;
}

QWidget* CameraDialog::createGeoWidget() {
    QWidget* tab = new QWidget;
    QVBoxLayout* mainLayout = new QVBoxLayout(tab);

    QLabel* description = new QLabel;
    description->setWordWrap(true);
    description->setAlignment(Qt::AlignCenter);
    description->setObjectName("camera-description");
    description->setText(
        "Sets the camera position from a geodetic position of a globe (e.g a planet/moon)."
    );

    mainLayout->addWidget(description);

    {

        QWidget* box = new QWidget;
        QGridLayout* layout = new QGridLayout(box);

        layout->addWidget(new QLabel("Anchor:"), 0, 0);
        _geoState.anchor = new QLineEdit;
        _geoState.anchor->setToolTip("Anchor camera to this globe (planet/moon)");
        layout->addWidget(_geoState.anchor, 0, 1);

        layout->addWidget(new QLabel("Latitude"), 1, 0);
        _geoState.latitude = new QLineEdit;
        _geoState.latitude->setValidator(new QDoubleValidator);
        _geoState.latitude->setToolTip("Latitude of camera focus point (+/- 90 degrees)");
        layout->addWidget(_geoState.latitude, 1, 1);

        layout->addWidget(new QLabel("Longitude"), 2, 0);
        _geoState.longitude = new QLineEdit;
        _geoState.longitude->setValidator(new QDoubleValidator);
        _geoState.longitude->setToolTip("Longitude of camera focus point (+/- 180 degrees)");
        layout->addWidget(_geoState.longitude, 2, 1);

        layout->addWidget(new QLabel("Altitude [m]"), 3, 0);
        _geoState.altitude = new QLineEdit;
        _geoState.altitude->setValidator(new QDoubleValidator);
        _geoState.altitude->setToolTip("Altitude of camera (meters)");
        _geoState.altitude->setPlaceholderText("optional");
        layout->addWidget(_geoState.altitude, 3, 1);

        mainLayout->addWidget(box);
    }

    // Add spacer at the end to prevent previous components from growing as much
    mainLayout->addSpacerItem(
        new QSpacerItem(0, 0, QSizePolicy::Expanding, QSizePolicy::Expanding)
    );

    return tab;
}

bool CameraDialog::areRequiredFormsFilledAndValid() {
    bool allFormsOk = true;
    _errorMsg->clear();

    if (_tabWidget->currentIndex() == CameraTypeNode) {
        if (_nodeState.anchor->text().isEmpty()) {
            allFormsOk = false;
            addErrorMsg("Anchor is empty");
        }
        if (!_nodeState.height->text().isEmpty()) {
            if (!isNumericalLargerThan(_nodeState.height, 0.f)) {
                allFormsOk = false;
                addErrorMsg("Height must be larger than zero");
            }
        }
    }

    if (_tabWidget->currentIndex() == CameraTypeNav) {
        if (_navState.anchor->text().isEmpty()) {
            allFormsOk = false;
            addErrorMsg("Anchor is empty");
        }
        if (_navState.positionX->text().isEmpty()) {
            allFormsOk = false;
            addErrorMsg("Position X is empty");
        }
        if (_navState.positionY->text().isEmpty()) {
            allFormsOk = false;
            addErrorMsg("Position Y is empty");
        }
        if (_navState.positionZ->text().isEmpty()) {
            allFormsOk = false;
            addErrorMsg("Position Z is empty");
        }

        const bool hasUpX = !_navState.upX->text().isEmpty();
        const bool hasUpY = !_navState.upY->text().isEmpty();
        const bool hasUpZ = !_navState.upZ->text().isEmpty();

        if (hasUpX || hasUpY || hasUpZ) {
            if (!hasUpX) {
                allFormsOk = false;
                addErrorMsg("Up X is empty");
            }
            if (!hasUpY) {
                allFormsOk = false;
                addErrorMsg("Up Y is empty");
            }
            if (!hasUpZ) {
                allFormsOk = false;
                addErrorMsg("Up Z is empty");
            }
        }
        if (!_navState.yaw->text().isEmpty()) {
            if (!inNumericalRange(_navState.yaw, -360.0, 360.0)) {
                allFormsOk = false;
                addErrorMsg("Yaw value is not in +/- 360.0 range");
            }
        }
        if (!_navState.pitch->text().isEmpty()) {
            if (!inNumericalRange(_navState.pitch, -360.0, 360.0)) {
                allFormsOk = false;
                addErrorMsg("Pitch value is not in +/- 360.0 range");
            }
        }
    }

    if (_tabWidget->currentIndex() == CameraTypeGeo) {
        if (_geoState.anchor->text().isEmpty()) {
            allFormsOk = false;
            addErrorMsg("Anchor is empty");
        }
        if (!inNumericalRange(_geoState.latitude, -90.0, 90.0)) {
            allFormsOk = false;
            addErrorMsg("Latitude value is not in +/- 90.0 range");
        }
        if (!inNumericalRange(_geoState.longitude, -180.0, 180.0)) {
            allFormsOk = false;
            addErrorMsg("Longitude value is not in +/- 180.0 range");
        }
    }

    return allFormsOk;
}

void CameraDialog::addErrorMsg(const QString& errorDescription) {
    QString contents = _errorMsg->text();
    if (!contents.isEmpty()) {
        contents += ", ";
    }
    contents += errorDescription;
    _errorMsg->setText(contents);
}

void CameraDialog::approved() {
    if (!areRequiredFormsFilledAndValid()) {
        return;
    }

    if (_tabWidget->currentIndex() == CameraTypeNode) {
        openspace::Profile::CameraGoToNode node;
        node.anchor = _nodeState.anchor->text().toStdString();
        if (!_nodeState.height->text().isEmpty()) {
            node.height = _nodeState.height->text().toDouble();
        }
        *_camera = std::move(node);
    }
    else if (_tabWidget->currentIndex() == CameraTypeNav) {
        openspace::Profile::CameraNavState nav;
        nav.anchor = _navState.anchor->text().toStdString();
        nav.aim = _navState.aim->text().toStdString();
        nav.referenceFrame = _navState.refFrame->text().toStdString();
        nav.position.x = _navState.positionX->text().toDouble();
        nav.position.y = _navState.positionY->text().toDouble();
        nav.position.z = _navState.positionZ->text().toDouble();
        if (!_navState.upX->text().isEmpty() &&
            !_navState.upY->text().isEmpty() &&
            !_navState.upZ->text().isEmpty())
        {
            const glm::dvec3 u = glm::dvec3(
                _navState.upX->text().toDouble(),
                _navState.upY->text().toDouble(),
                _navState.upZ->text().toDouble()
            );
            nav.up = u;
        }
        else {
            nav.up = std::nullopt;
        }
        if (!_navState.yaw->text().isEmpty()) {
            nav.yaw = _navState.yaw->text().toDouble();
        }
        else {
            nav.yaw = std::nullopt;
        }
        if (!_navState.pitch->text().isEmpty()) {
            nav.pitch = _navState.pitch->text().toDouble();
        }
        else {
            nav.pitch = std::nullopt;
        }
        *_camera = std::move(nav);
    }
    else if (_tabWidget->currentIndex() == CameraTypeGeo) {
        openspace::Profile::CameraGoToGeo geo;
        geo.anchor = _geoState.anchor->text().toStdString();
        geo.latitude = _geoState.latitude->text().toDouble();
        geo.longitude = _geoState.longitude->text().toDouble();
        if (!_geoState.altitude->text().isEmpty()) {
            geo.altitude = _geoState.altitude->text().toDouble();
        }
        *_camera = std::move(geo);
    }

    accept();
}

void CameraDialog::tabSelect(int tabIndex) {
    _errorMsg->clear();

    if (tabIndex == CameraTypeNode) {
        _nodeState.anchor->setFocus(Qt::OtherFocusReason);
    }
    else if (tabIndex == CameraTypeNav) {
        _navState.anchor->setFocus(Qt::OtherFocusReason);
    }
    else if (tabIndex == CameraTypeGeo) {
        _geoState.anchor->setFocus(Qt::OtherFocusReason);
    }
    else {
        throw std::logic_error("Unknown tab index");
    }
}
