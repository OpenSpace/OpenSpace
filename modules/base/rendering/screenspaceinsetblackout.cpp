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

#include <modules/base/rendering/screenspaceinsetblackout.h>
#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/clipboard.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo CopyToClipboardInfo = {
        "CopyToClipboard",
        "Copy to clipboard",
        "Copies the current configuration to the clipboard so that it can be pasted"
        "into the asset file.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo CalibrationPatternInfo = {
        "EnableCalibrationPattern",
        "Enable Calibration Pattern",
        "Enables the calibration pattern.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo CalibrationColorInfo = {
        "EnableCalibrationColor",
        "Enable Calibration Color",
        "Set Blackout Shape to a bright color for easier calibration.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AddControlPointInfo = {
        "AddControlPoint",
        "Add Control Point",
        "Adds a new control point to the spline.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RemoveControlPointInfo = {
        "RemoveControlPoint",
        "Remove Control Point",
        "Removes the selected control point from the spline.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RemoveSelectorInfo = {
        "RemoveSelector",
        "Select Point To Remove",
        "Removes the selected control point.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AddSelectorInfo = {
        "AddSelector",
        "Select Where To Add",
        "Select where to add a new point.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo NewPointPositionInfo = {
        "NewPointPosition",
        "Point Position",
        "X and Y coordinates for where to add the new control point.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo CalibrationTextureInfo = {
        "CalibrationTexture",
        "Calibration Texture",
        "Texture used as calibration pattern.",
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(ScreenSpaceInsetBlackout)]] Parameters {
        struct BlackoutShape {
            std::vector<glm::vec2> corners;
            std::optional<std::vector<glm::vec2>> top;
            std::optional<std::vector<glm::vec2>> bottom;
            std::optional<std::vector<glm::vec2>> left;
            std::optional<std::vector<glm::vec2>> right;
            std::optional<std::string> calibrationTexturePath;
        };
        BlackoutShape blackoutshape;
        std::optional<std::string> identifier;
    };
#include "screenspaceinsetblackout_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceInsetBlackout::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_inset_blackout");
}

ScreenSpaceInsetBlackout::Spline::Point::Point(glm::vec2& dataRef, const std::string& id,
                                               const std::string& name)
{
    rawDataPointer = &dataRef;
    strcpy(propIdentifier.cstr, id.c_str());
    strcpy(guiName.cstr, name.c_str());
}

void ScreenSpaceInsetBlackout::Spline::Point::updateRawDataPointerValue() {
    *rawDataPointer = prop->value();
}

ScreenSpaceInsetBlackout::Spline::Point::~Point() {
    delete prop;
    delete propInfo;
}

ScreenSpaceInsetBlackout::Spline::Spline(const ghoul::Dictionary& dictionary,
                                        BlackoutShape& shape,
                                        const std::string identifier,
                                        const std::string guiName,
                                        const std::string baseId,
                                        const std::string baseName,
                                        const Spline::Side side)
    : parentShape(shape)
    , properties::PropertyOwner({ identifier, guiName, "" })
    , baseIdentifier(baseId)
    , baseGuiName(baseName)
    , addControlPoint(AddControlPointInfo)
    , addSelector(AddSelectorInfo)
    , newPointPosition(
        NewPointPositionInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(1.f),
        glm::vec2(0.001f)
    )
    , removeControlPoint(RemoveControlPointInfo)
    , removeSelector(RemoveSelectorInfo)
    , shapeSide(side)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    // Parse from asset file definitions and create rawPoint vector
    std::vector<glm::vec2> vec;
    rawData.emplace_back();
    switch (side) {
        case Spline::Side::Top:
            vec = p.blackoutshape.top.value_or(std::vector<glm::vec2>());
            break;
        case Spline::Side::Right:
            vec = p.blackoutshape.right.value_or(std::vector<glm::vec2>());
            break;
        case Spline::Side::Bottom:
            vec = p.blackoutshape.bottom.value_or(std::vector<glm::vec2>());
            break;
        case Spline::Side::Left:
            vec = p.blackoutshape.left.value_or(std::vector<glm::vec2>());
            break;
    }
    std::for_each(vec.begin(), vec.end(), [this](glm::vec2& v) {
        rawData.push_back(v);
    });
    rawData.emplace_back();
}

void ScreenSpaceInsetBlackout::Spline::addSelectionOptions() {
    // Add selector used when inserting a new point
    for (int i = 1; i < points.size(); ++i) {
        addSelector.addOption(i, std::format("Between {} and {}", i, i + 1));
    }

    // Add selector used when removing a point
    for (int i = 1; i < points.size() - 1; ++i) {
        removeSelector.addOption(i, std::format("Point {}", i + 1));
    }
}

void ScreenSpaceInsetBlackout::Spline::updatePropertyTree() {
    // Generate Vec2Property from all spline points
    generateProperties();

    // The number of Vec2Properties generated by generateProperties()
    const int numberOfPoints = static_cast<int>(points.size());

    // Add Vec2Property entries to Property Tree
    for (int i = 0; i < numberOfPoints; ++i) {
        addProperty(points[i]->prop);
    }

    // Add additional GUI controls
    addProperty(addSelector);
    addProperty(newPointPosition);
    addProperty(addControlPoint);
    addProperty(removeSelector);
    addProperty(removeControlPoint);

    // Adds the options for addSelector and removeSelector
    addSelectionOptions();
}

void ScreenSpaceInsetBlackout::Spline::createPropertyTree() {
    // Generate Vec2Property from all spline points
    generateProperties();

    // The number of Vec2Properties generated by generateProperties()
    const int numberOfPoints = static_cast<int>(points.size());

    // Add Vec2Property entries to Property Tree
    for (int i = 0; i < numberOfPoints; ++i) {
        addProperty(points[i]->prop);
    }

    // Adds the options for addSelector and removeSelector
    addSelectionOptions();

    // Adds controls to property tree
    addProperty(addSelector);
    addProperty(newPointPosition);
    addControlPoint.onChange([this]() {
        addPointFlag = true;
        isTextureDirty = true;
    });
    addProperty(addControlPoint);
    addProperty(removeSelector);
    removeControlPoint.onChange([this]() {
        removePointFlag = true;
        isTextureDirty = true;
    });
    addProperty(removeControlPoint);
}

void ScreenSpaceInsetBlackout::Spline::generateProperties() {
    // Remove any existing points
    points.clear();

    std::pair<glm::vec2, glm::vec2> corners = parentShape.readCornerData(shapeSide);
    rawData.front() = corners.first;
    rawData.back() = corners.second;

    for (int i = 0; i < rawData.size(); ++i) {
        // Create the point
        std::string id = baseIdentifier + std::to_string(i);
        std::string name = std::format("{} #{}", baseGuiName, i);
        Point* point = new Point(rawData[i], id, name);

        // Create PropertyInfo and Vec2Property and add them
        point->propInfo = new properties::Property::PropertyInfo(
            point->propIdentifier.cstr,
            point->guiName.cstr,
            "Position (x,y) for where the control point should be.",
            openspace::properties::Property::Visibility::User
        );

        point->prop = new properties::Vec2Property(
            *(point->propInfo),
            rawData[i],
            glm::vec2(0.f),
            glm::vec2(1.f),
            glm::vec2(0.001f)
        );

        // Special onChange for corners
        if (i == 0 || i == rawData.size() - 1) {
            point->prop->onChange([this, point]() {
                updateCornerData();
                point->updateRawDataPointerValue();
                isTextureDirty = true;
            });
        }
        else {
            point->prop->onChange([this, point]() {
                point->updateRawDataPointerValue();
                isTextureDirty = true;
            });
        }

        // Addo to points vector
        points.push_back(std::unique_ptr<Point>(point));
    }
}

void ScreenSpaceInsetBlackout::Spline::updateCornerData() {
    parentShape.setCornerData(
        shapeSide,
        points.front()->prop->value(),
        points.back()->prop->value()
    );
}

void ScreenSpaceInsetBlackout::Spline::removePoint() {
    // Remove from data vector
    const int index = removeSelector.value();
    rawData.erase(rawData.begin() + index);

    // Remove all existing Points in the property tree and resets selection options
    cleanPropertyTree();

    // Updates the property tree by regenerating points
    updatePropertyTree();
}

void ScreenSpaceInsetBlackout::Spline::addPoint() {
    // Insert into data vector
    const int index = addSelector.value();
    glm::vec2 newPoint = newPointPosition.value();
    rawData.insert(rawData.begin() + index, newPoint);

    // Remove all existing Points and resets selection options
    cleanPropertyTree();

    // Updates the property tree by regenerating points
    updatePropertyTree();
}

void ScreenSpaceInsetBlackout::Spline::cleanPropertyTree() {
    // Remove all Points from GUI
    for (int i = 0; i < points.size(); ++i) {
        removeProperty(points[i]->prop);
    }

    // Remove all options
    addSelector.clearOptions();
    removeSelector.clearOptions();

    // Remove additional GUI controls
    removeProperty(addSelector);
    removeProperty(addControlPoint);
    removeProperty(newPointPosition);
    removeProperty(removeSelector);
    removeProperty(removeControlPoint);
}

void ScreenSpaceInsetBlackout::Spline::syncCornerData() {
    std::pair<glm::vec2, glm::vec2> corners = parentShape.readCornerData(shapeSide);
    *points.front()->prop = corners.first;
    *points.back()->prop = corners.second;
}

ScreenSpaceInsetBlackout::BlackoutShape::BlackoutShape(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "BlackoutShape", "Blackout Shape", "" })
    , top(dictionary, *this, "Top", "Top", "TopSpline","Top Point",
        Spline::Side::Top)
    , right(dictionary, *this, "Right", "Right", "RightSpline", "Right Point",
        Spline::Side::Right)
    , bottom(dictionary, *this, "Bottom", "Bottom", "BottomSpline", "Bottom Point",
        Spline::Side::Bottom)
    , left(dictionary, *this, "Left", "Left", "LeftSpline", "Left Point",
        Spline::Side::Left)
    , copyToClipboardTrigger(CopyToClipboardInfo)
    , enableCalibrationPattern(CalibrationPatternInfo, false)
    , calibrationTexturePath(CalibrationTextureInfo)
    , enableCalibrationColor(CalibrationColorInfo, false)
{
    addPropertySubOwner(top);
    addPropertySubOwner(right);
    addPropertySubOwner(bottom);
    addPropertySubOwner(left);

    const Parameters p = codegen::bake<Parameters>(dictionary);
    std::vector<glm::vec2> corners = p.blackoutshape.corners;

    // Corners values are shared and accessed by Spline instances
    {
        std::unique_lock lock(cornerMutex);
        topLeftCorner = corners[0];
        topRightCorner = corners[1];
        bottomRightCorner = corners[2];
        bottomLeftCorner = corners[3];
    }

    // Populates the GUI and property tree
    top.createPropertyTree();
    right.createPropertyTree();
    bottom.createPropertyTree();
    left.createPropertyTree();

    enableCalibrationPattern.onChange([this]() {
        isTextureDirty = true;
    });
    addProperty(enableCalibrationPattern);

    enableCalibrationColor.onChange([this]() {
        isTextureDirty = true;
    });
    addProperty(enableCalibrationColor);

    copyToClipboardTrigger.onChange([this]() {
        copyToClipboard();
    });
    addProperty(copyToClipboardTrigger);
}

void ScreenSpaceInsetBlackout::BlackoutShape::copyToClipboard() {
    std::pair<glm::vec2, glm::vec2> ct = readCornerData(Spline::Side::Top);
    std::pair<glm::vec2, glm::vec2> cb = readCornerData(Spline::Side::Bottom);
    std::vector<glm::vec2> allCorners = { ct.first, ct.second, cb.first, cb.second };

    std::string strCorners = formatLine("Corners", allCorners, true);
    std::string strTop = formatLine("Top", top.rawData);
    std::string strRight = formatLine("Right", right.rawData);
    std::string strBottom = formatLine("Bottom", bottom.rawData);
    std::string strLeft = formatLine("Left", left.rawData);

    ghoul::setClipboardText(strCorners + strTop + strRight + strBottom + strLeft);
}

std::string ScreenSpaceInsetBlackout::BlackoutShape::formatLine(std::string id,
                                                    const std::vector<glm::vec2>& data,
                                                    const bool isCorner)
{
    const int start = (isCorner) ? 0 : 1;
    const int end = static_cast<int>((isCorner) ? data.size() : (data.size() - 1));
    std::string str = std::format("{} = {{ ", id);
    for (int i = start; i < end; ++i) {
        std::string xVal = std::format("{}", data[i].x);
        std::string yVal = std::format("{}", data[i].y);
        xVal += (xVal.find(".") == std::string::npos) ? ".0" : "";
        yVal += (yVal.find(".") == std::string::npos) ? ".0" : "";
        str.append(std::format("{{{}, {}}}", xVal, yVal));
        std::string delimiter = (i < end - 1) ? ", " : " ";
        str.append(delimiter);
    }
    str.append("},\n");

    return str;
}

std::pair<glm::vec2, glm::vec2> ScreenSpaceInsetBlackout::BlackoutShape::readCornerData(
                                                                const Spline::Side& side)
{
    std::shared_lock lock(cornerMutex);
    std::pair<glm::vec2, glm::vec2> result;
    switch (side) {
        case Spline::Side::Top:
            result = std::pair(topLeftCorner, topRightCorner);
            break;
        case Spline::Side::Right:
            result = std::pair(topRightCorner, bottomRightCorner);
            break;
        case Spline::Side::Bottom:
            result = std::pair(bottomRightCorner, bottomLeftCorner);
            break;
        case Spline::Side::Left:
            result = std::pair(bottomLeftCorner, topLeftCorner);
            break;
    }
    return result;
}

void ScreenSpaceInsetBlackout::BlackoutShape::setCornerData(const Spline::Side& side,
                                                            const glm::vec2 corner0,
                                                            const glm::vec2 corner1)
{
    std::unique_lock lock(cornerMutex);
    switch (side) {
        case Spline::Side::Top:
            topLeftCorner = corner0;
            topRightCorner = corner1;
            break;
        case Spline::Side::Right:
            topRightCorner = corner0;
            bottomRightCorner = corner1;
            break;
        case Spline::Side::Bottom:
            bottomRightCorner = corner0;
            bottomLeftCorner = corner1;
            break;
        case Spline::Side::Left:
            bottomLeftCorner = corner0;
            topLeftCorner = corner1;
            break;
    }
}

bool ScreenSpaceInsetBlackout::BlackoutShape::checkTextureStatus() {
    return isTextureDirty ||
        top.isTextureDirty ||
        right.isTextureDirty ||
        bottom.isTextureDirty ||
        left.isTextureDirty;
}

void ScreenSpaceInsetBlackout::BlackoutShape::updateSplineAndGui() {
    std::array<Spline*, 4> refs = { &top, &right, &bottom, &left };
    for (Spline* spline : refs) {
        bool updatePropertyTree = false;
        if (spline->addPointFlag) {
            spline->addPoint();
            spline->addPointFlag = false;
            updatePropertyTree = true;
        }
        if (spline->removePointFlag) {
            spline->removePoint();
            spline->removePointFlag = false;
            updatePropertyTree = true;
        }
        // Need to remove and add the PropertySubOwner as the GUI does not reflect
        // changes otherwise
        if (updatePropertyTree) {
            removePropertySubOwner(spline);
            addPropertySubOwner(spline);
        }
    }
}

void ScreenSpaceInsetBlackout::BlackoutShape::resetDirtyTextureFlag() {
    top.isTextureDirty = false;
    right.isTextureDirty = false;
    bottom.isTextureDirty = false;
    left.isTextureDirty = false;
    isTextureDirty = false;
}

ScreenSpaceInsetBlackout::ScreenSpaceInsetBlackout(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _shape(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier = p.identifier.value_or("ScreenSpaceInsetBlackout");
    setIdentifier(makeUniqueIdentifier(std::move(identifier)));

    // Makes sure that User has specified 4 corners in the asset file
    checkCornerSpecification(p.blackoutshape.corners);

    // GUI
    addPropertySubOwner(_shape);

    // Setup and render to texture
    initializeShadersAndFBO();
    generateTexture();

    // Handling calibration texture
    std::optional<std::string> optTexturePath = p.blackoutshape.calibrationTexturePath;
    if (optTexturePath.has_value()) {
        if (std::filesystem::is_regular_file(absPath(*optTexturePath))) {
            std::string path = absPath(*optTexturePath).string();
            std::unique_ptr<ghoul::opengl::Texture> texture =
                ghoul::io::TextureReader::ref().loadTexture(absPath(path), 2);
            if (texture) {
                // Images don't need to start on 4-byte boundaries, for example if the
                // image is only RGB
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

                if (texture->format() == ghoul::opengl::Texture::Format::Red) {
                    texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_ONE });
                }

                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                texture->purgeFromRAM();

                _calibrationTexture = std::move(texture);
                _objectSize = _calibrationTexture->dimensions();
            }
        }
        else {
            LWARNINGC(
                "ScreenSpaceInsetBlackout",
                std::format(
                    "Calibration texture '{}' is missing.",
                    *optTexturePath
                )
            );
        }
    }
}

void ScreenSpaceInsetBlackout::checkCornerSpecification(std::vector<glm::vec2> corners) {
    if (corners.size() != 4) {
        documentation::TestResult res;
        res.offenses.push_back(documentation::TestResult::Offense());

        res.success = false;
        res.offenses[0].offender = "ScreenSpaceInsetBlackout.Blackoutshape.Corners";
        res.offenses[0].explanation = "Asset must contain exactly 4 corners";
        res.offenses[0].reason =
            documentation::TestResult::Offense::Reason::Verification;

        throw documentation::SpecificationError(
            res,
            "ScreenSpaceInsetBlackout"
        );
    }
}

void ScreenSpaceInsetBlackout::initializeShadersAndFBO() {
    // Setup vertex buffer
    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    // Setup program object and shaders
    _fboProgram = BaseModule::ProgramObjectManager.request(
        "ScreenSpaceInsetBlackout",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "ScreenSpaceInsetBlackout",
                absPath("${MODULE_BASE}/shaders/screenspaceinsetblackout_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/screenspaceinsetblackout_fs.glsl")
            );
        }
    );

    // Setup FBO & Texture (UHD resolution)
    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);

    _blackoutTexture = std::unique_ptr<ghoul::opengl::Texture>(
        new ghoul::opengl::Texture(glm::uvec3(3840,2160,1),
        GL_TEXTURE_2D,ghoul::opengl::Texture::Format::RGBA)
    );

    _blackoutTexture->bind();
    glTexImage2D(
        GL_TEXTURE_2D, 0, GL_RGBA, 3840, 2160, 0,GL_RGBA, GL_UNSIGNED_BYTE, NULL
    );

    _objectSize = _blackoutTexture->dimensions();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glFramebufferTexture2D(
        GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, *_blackoutTexture, 0
    );

    ghoul_assert(
        glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE,
        "Failed to complete frame buffer creation"
    );

    _blackoutTexture->purgeFromRAM();

    glBindTexture(GL_TEXTURE_2D, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // Uniform Cache
    _uniformCache.color = _fboProgram->uniformLocation("color");
}

void ScreenSpaceInsetBlackout::generateVertexArray() {
    // Clear old data
    _vboData.clear();

    // Update all corners
    _shape.top.syncCornerData();
    _shape.right.syncCornerData();
    _shape.bottom.syncCornerData();
    _shape.left.syncCornerData();


    // Quad used for displaying calibration grid texture
    const glm::vec2 calibrationQuad[4] = {
        glm::vec2(-1.f, 1.f),
        glm::vec2(1.f, 1.f),
        glm::vec2(1.f, -1.f),
        glm::vec2(-1.f, -1.f)
    };

    if (_shape.enableCalibrationPattern) {
        for (int i = 0; i < 4; ++i) {
            _vboData.push_back(calibrationQuad[i]);
        }
    }
    else {
        std::vector<glm::vec2> pointsTop;
        std::vector<glm::vec2> pointsRight;
        std::vector<glm::vec2> pointsBottom;
        std::vector<glm::vec2> pointsLeft;

        // We must add padding (mirrored points) before the first and after the last
        // points of spline in order to perform Catmull-Rom
        std::pair<glm::vec2, glm::vec2> tPadding = calculatePadding(_shape.top.points);
        std::pair<glm::vec2, glm::vec2> rPadding = calculatePadding(_shape.right.points);
        std::pair<glm::vec2, glm::vec2> bPadding = calculatePadding(_shape.bottom.points);
        std::pair<glm::vec2, glm::vec2> lPadding = calculatePadding(_shape.left.points);

        // Insert first point (mirrored)
        pointsTop.push_back(tPadding.first);
        pointsRight.push_back(rPadding.first);
        pointsBottom.push_back(bPadding.first);
        pointsLeft.push_back(lPadding.first);

        // Copy all spline points data to temporary point vector
        copyToPointsVector(_shape.top.points, pointsTop);
        copyToPointsVector(_shape.right.points, pointsRight);
        copyToPointsVector(_shape.bottom.points, pointsBottom);
        copyToPointsVector(_shape.left.points, pointsLeft);

        // Insert last point (mirrored)
        pointsTop.push_back(tPadding.second);
        pointsRight.push_back(rPadding.second);
        pointsBottom.push_back(bPadding.second);
        pointsLeft.push_back(lPadding.second);

        // Sample points on the spline (Catmull-Rom)
        std::vector<glm::vec2> splineTop = sampleSpline(pointsTop);
        std::vector<glm::vec2> splineRight = sampleSpline(pointsRight);
        std::vector<glm::vec2> splineBottom = sampleSpline(pointsBottom);
        std::vector<glm::vec2> splineLeft = sampleSpline(pointsLeft);

        // Offset points from [0,1] to [-1,1] for x and y
        offsetCoordinates(splineTop);
        offsetCoordinates(splineRight);
        offsetCoordinates(splineBottom);
        offsetCoordinates(splineLeft);

        // Incoming vertex data should be top -> right -> bottom -> left (clockwise)
        // Data to render needs to be counter-clockwise for correct winding
        // Also adds an extra point at the end for Triangle Fan
        _vboData.push_back(glm::vec2(0.0, 0.0));
        _vboData.insert(_vboData.end(), splineTop.rbegin(), splineTop.rend());
        _vboData.insert(_vboData.end(), splineLeft.rbegin(), splineLeft.rend());
        _vboData.insert(_vboData.end(), splineBottom.rbegin(), splineBottom.rend());
        _vboData.insert(_vboData.end(), splineRight.rbegin(), splineRight.rend());
        _vboData.push_back(splineTop.back());
    }
}

void ScreenSpaceInsetBlackout::copyToPointsVector(
                            const std::vector<std::unique_ptr<Spline::Point>>& points,
                            std::vector<glm::vec2>& vertexData)
{
    for (int i = 0; i < points.size(); ++i) {
        vertexData.push_back(*points[i]->prop);
    }
}

std::vector<glm::vec2> ScreenSpaceInsetBlackout::sampleSpline(
                                            const std::vector<glm::vec2>& controlPoints)
{
    // Number of samples we do per spline segmentr
    const int subdivisions = 250;
    std::vector<glm::vec2> outSplineData;
    const int numberOfSegments = static_cast<int>(controlPoints.size() - 3);
    const float stepSize = 1.f / subdivisions;
    for (int i = 0; i < numberOfSegments; ++i) {
        for (int s = 0; s < subdivisions; ++s) {
            float tValue = stepSize * s;
            glm::vec2 splinePosition = calculateCatmullRom(
                *(controlPoints.begin() + i + 0),
                *(controlPoints.begin() + i + 1),
                *(controlPoints.begin() + i + 2),
                *(controlPoints.begin() + i + 3),
                tValue
            );
            outSplineData.push_back(splinePosition);
        }
    }
    return outSplineData;
}

glm::vec2 ScreenSpaceInsetBlackout::calculateCatmullRom(const glm::vec2& p0,
                                                        const glm::vec2& p1,
                                                        const glm::vec2& p2,
                                                        const glm::vec2& p3,
                                                        const float t)
{
    glm::vec2 newPoint;
    const float t3 = powf(t, 3.f);
    const float t2 = powf(t, 2.f);
    const float alpha = 0.5f;

    const float p0t = ((-t) + (2.f * t2) - t3);
    const float p1t = (2.f - (5.f * t2) + (3.f * t3));
    const float p2t = (t + (4.f * t2) - (3.f * t3));
    const float p3t = (-t2 + t3);

    const glm::vec2 np0 = p0 * p0t;
    const glm::vec2 np1 = p1 * p1t;
    const glm::vec2 np2 = p2 * p2t;
    const glm::vec2 np3 = p3 * p3t;

    newPoint = alpha * (np0 + np1 + np2 + np3);

    return newPoint;
}

void ScreenSpaceInsetBlackout::generateTexture() {
    // Generate new vertex array data
    generateVertexArray();

    // OpenGL stuff
    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vboData.size() * sizeof(glm::vec2),
        _vboData.data(),
        GL_STATIC_DRAW
    );
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(glm::vec2), nullptr);
    glBindVertexArray(0);

    _fboProgram->activate();

    // Set uniform
    _fboProgram->setUniform("color",
        _shape.enableCalibrationColor ? glm::vec3(0.f, 1.f, 0.f) : glm::vec3(0.f)
    );

    // Resolution
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    // bind FBO and Texture
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);

    bindTexture();

    // Clear current buffer
    glViewport(0, 0, 3840, 2160);
    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);

    // Draw
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLE_FAN, 0, static_cast<gl::GLsizei>(_vboData.size()));
    glBindVertexArray(0);

    glBindTexture(GL_TEXTURE_2D, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
    _fboProgram->deactivate();

    // Reset flags
    _shape.resetDirtyTextureFlag();
}

void ScreenSpaceInsetBlackout::update() {
    _shape.updateSplineAndGui();
    bool dirtyTexture = _shape.checkTextureStatus();
    if (dirtyTexture) {
        generateTexture();
    }
}

void ScreenSpaceInsetBlackout::bindTexture() {
    if (_shape.enableCalibrationPattern) {
        if (_calibrationTexture) {
            _calibrationTexture->bind();
        }
    }
    else {
        if (_blackoutTexture) {
            _blackoutTexture->bind();
        }
    }
}

bool ScreenSpaceInsetBlackout::deinitializeGL() {
    _blackoutTexture = nullptr;
    _calibrationTexture = nullptr;

    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
    glDeleteFramebuffers(1, &_fbo);

    if (_fboProgram) {
        BaseModule::ProgramObjectManager.release(
            _fboProgram,
            [](ghoul::opengl::ProgramObject* p) {
                global::renderEngine->removeRenderProgram(p);
            }
        );
        _fboProgram = nullptr;
    }

    return ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceInsetBlackout::offsetCoordinates(std::vector<glm::vec2> &vec) {
    for (int i = 0; i < vec.size(); ++i) {
        vec[i].x = (vec[i].x * 2.f) - 1.f;
        vec[i].y = (vec[i].y * 2.f) - 1.f;
    }
}

std::pair<glm::vec2, glm::vec2> ScreenSpaceInsetBlackout::calculatePadding(
    const std::vector<std::unique_ptr<ScreenSpaceInsetBlackout::Spline::Point>>& pVec)
{
    const glm::vec2& pf0 = pVec[0]->prop->value();
    const glm::vec2& pf1 = pVec[1]->prop->value();
    const glm::vec2& pb0 = pVec[pVec.size() - 1]->prop->value();
    const glm::vec2& pb1 = pVec[pVec.size() - 2]->prop->value();

    const glm::vec2 startPoint = pf0 + ((pf0 - pf1) * -1.f);
    const glm::vec2 endPoint = pb0 + ((pb0 - pb1) * -1.f);
    return std::pair(startPoint, endPoint);
}

} // namespace openspace
