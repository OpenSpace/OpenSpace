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
#include <ghoul/misc/interpolator.h>

namespace {
    constexpr glm::uvec2 BlackoutTextureSize = glm::uvec2(3840, 2160);

    void checkCornerSpecification(std::vector<glm::vec2> corners) {
        if (corners.size() != 4) {
            openspace::documentation::TestResult res;
            res.success = false;
            res.offenses.push_back(openspace::documentation::TestResult::Offense());
            res.offenses[0].offender = "ScreenSpaceInsetBlackout.Blackoutshape.Corners";
            res.offenses[0].explanation = "Asset must contain exactly 4 corners";
            res.offenses[0].reason =
                openspace::documentation::TestResult::Offense::Reason::Verification;
            throw openspace::documentation::SpecificationError(
                res,
                "ScreenSpaceInsetBlackout"
            );
        }
    }

    std::pair<glm::vec2, glm::vec2> calculatePadding(const std::vector<glm::vec2>& v) {
        ghoul_assert(v.size() >= 2, "Too few points in the list");

        const glm::vec2& pf0 = v[1];
        const glm::vec2& pf1 = v[2];
        const glm::vec2& pb0 = v[v.size() - 1];
        const glm::vec2& pb1 = v[v.size() - 2];
        const glm::vec2 firstPaddingPoint = 2.f * pf0 - pf1;
        const glm::vec2 lastPaddingPoint = 2.f * pb0 - pb1;
        return std::pair(firstPaddingPoint, lastPaddingPoint);
    }

    std::vector<glm::vec2> sampleSpline(const std::vector<glm::vec2>& controlPoints) {
        ghoul_assert(controlPoints.size() >= 3, "Too few control points");

        constexpr int Subdivisions = 100;
        std::vector<glm::vec2> splineData;
        const int numberOfSegments = static_cast<int>(controlPoints.size() - 3);
        const float stepSize = 1.f / Subdivisions;
        for (int i = 0; i < numberOfSegments; i++) {
            for (int s = 0; s < Subdivisions; s++) {
                float tValue = stepSize * s;
                splineData.push_back(ghoul::interpolateCatmullRom(
                    tValue,
                    *(controlPoints.begin() + i + 0),
                    *(controlPoints.begin() + i + 1),
                    *(controlPoints.begin() + i + 2),
                    *(controlPoints.begin() + i + 3)
                ));
            }
        }
        return splineData;
    }

    void offsetCoordinates(std::vector<glm::vec2>& vec) {
        for (glm::vec2& v : vec) {
            v.x = (v.x * 2.f) - 1.f;
            v.y = (v.y * 2.f) - 1.f;
        }
    }

    std::string formatLine(std::string id, const std::vector<glm::vec2>& data)
    {
        if (data.empty()) {
            return "";
        }

        std::string str = std::format("{} = {{ ", id);
        for (int i = 0; i < data.size(); ++i) {
            std::string xVal = std::format("{}", data[i].x);
            std::string yVal = std::format("{}", data[i].y);
            xVal += (xVal.find(".") == std::string::npos) ? ".0" : "";
            yVal += (yVal.find(".") == std::string::npos) ? ".0" : "";
            str.append(std::format("{{{}, {}}}", xVal, yVal));
            std::string delimiter = (i < data.size() - 1) ? ", " : " ";
            str.append(delimiter);
        }
        str.append("},\n");

        return str;
    }

    void copyToClipboard(std::vector<glm::vec2>& cornerData,
                         std::vector<glm::vec2>& topSplineData,
                         std::vector<glm::vec2>& rightSplineData,
                         std::vector<glm::vec2>& bottomSplineData,
                         std::vector<glm::vec2>& leftSplineData)
    {
        std::string strCorners = formatLine("Corners", cornerData);
        std::string strTop = formatLine("Top", topSplineData);
        std::string strRight = formatLine("Right", rightSplineData);
        std::string strBottom = formatLine("Bottom", bottomSplineData);
        std::string strLeft = formatLine("Left", leftSplineData);

        ghoul::setClipboardText(strCorners + strTop + strRight + strBottom + strLeft);
    }

    constexpr openspace::properties::Property::PropertyInfo CopyToClipboardInfo = {
        "CopyToClipboard",
        "Copy to clipboard",
        "Copies the current configuration to the clipboard so that it can be pasted "
        "into the asset file.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo CalibrationPatternInfo = {
        "EnableCalibrationPattern",
        "Enable Calibration Pattern",
        "Enables the calibration pattern. The calibration can be used to find which "
        "values to use when setting up the blackout shape.",
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

    // A ScreenSpaceInsetBlackout can be used to render a screen-space shape used to
    // black out part of the rendering. This can be useful in a dome environment where
    // you have a secondary presentation projector that can project on the dome surface.
    // The blackout is used to avoid overlapping rendering between the dome projectors
    // and the presentation projector.
    struct [[codegen::Dictionary(ScreenSpaceInsetBlackout)]] Parameters {
        struct BlackoutShape {
            // List of corner positions for the blackout shape. The order of
            // corner points are Top-Left, Top-Right, Bottom-Right, Bottom-Left with the
            // range of 0 to 1 for `{X,Y}`, where `{0,1}` is the Top-Left corner and
            // `{1,0}` is the Bottom-Right corner.
            std::vector<glm::vec2> corners;

            // List of points between the Top-Left and Top-Right corners that will be
            // used to define top spline of the blackout shape. Each point is specified
            // in the range of 0 to 1, where `{0,1}` is the Top-Left corner and `{1,1}`
            // is the Top-Right corner.
            std::optional<std::vector<glm::vec2>> top;

            // List of points between the Bottom-Right and Bottom-Left corners that will
            // be used to define bottom spline of the blackout shape. Each point is
            // specified in the range of 0 to 1, where `{1,0}` is the Bottom-Right
            // corner and `{0,0}` is the Bottom-Left corner.
            std::optional<std::vector<glm::vec2>> bottom;

            // List of points between the Bottom-Left and Top-Left corners that will be
            // used to define left spline of the blackout shape. Each point is specified
            // in the range of 0 to 1, where `{0,0}` is the Bottom-Left corner and
            // `{0,1}` is the Top-Left corner.
            std::optional<std::vector<glm::vec2>> left;

            // List of points between the Top-Right and Bottom-Right corners that will
            // be used to define right spline of the blackout shape. Each point is
            // specified in the range of 0 to 1, where `{1,1}` is the Top-Right corner
            // and `{1,0}` is the Bottom-Right corner.
            std::optional<std::vector<glm::vec2>> right;

            // File path for the texture that should be used when displaying the
            // calibration grid.
            std::optional<std::filesystem::path> calibrationTexturePath;
        };
        BlackoutShape blackoutshape;
    };
#include "screenspaceinsetblackout_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceInsetBlackout::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_inset_blackout");
}

ScreenSpaceInsetBlackout::BlackoutShape::Point::Point(glm::vec2& inData,
                                                                  std::string identifier,
                                                                  std::string guiName)
{
    // Creates PropertyInfo used to create Property
    propInfo = std::make_unique<properties::Property::PropertyInfo>(
        identifier.c_str(),
        guiName.c_str(),
        "Position (x,y) for where the control point is located."
    );

    // Stores pointer to data and creates Vec2Property for the given position
    dataptr = &inData;
    prop = std::make_unique<properties::Vec2Property>(
        *propInfo,
        *dataptr,
        glm::vec2(0.f),
        glm::vec2(1.f),
        glm::vec2(0.01f)
    );
}

void ScreenSpaceInsetBlackout::BlackoutShape::Point::updateData() {
    *dataptr = prop->value();
}

ScreenSpaceInsetBlackout::BlackoutShape::Spline::Spline(std::vector<glm::vec2>& inData,
                                                        std::string baseString)
    : properties::PropertyOwner({ baseString , baseString, "" })
    , data(inData)
    , newPointPosition(
        NewPointPositionInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(1.f)
    )
    , addSelector(AddSelectorInfo)
    , addButton(AddControlPointInfo)
    , removeSelector(RemoveSelectorInfo)
    , removeButton(RemoveControlPointInfo)
{
    base = baseString;

    // Generate all Point objects and add them to GUI
    for (int i = 0; i < data.size(); i++) {
        points.push_back(std::make_unique<Point>(
            data[i],
            std::format("Point{}Position", i),
            std::format("{} Point #{}", base, i + 1)
        ));
        points[i]->prop->onChange([this, i]() {
            points[i]->updateData();
            dataHasChanged = true;
        });
        addProperty(points[i]->prop.get());
    }

    // Configure onChange and adding GUI controls
    addButton.onChange([this]() {
        dataHasChanged = true;
        pointAdded = true;
    });
    addProperty(addSelector);
    addProperty(newPointPosition);
    addProperty(addButton);

    // Add options used when inserting a new point
    for (int i = 0; i < points.size() + 1; i++) {
        addSelector.addOption(i, std::format("At position #{}", i + 1));
    }

    // Only add controls for removing a point if there are any points that can be removed
    if (!data.empty()) {
        removeButton.onChange([this]() {
            dataHasChanged = true;
            pointRemoved = true;
        });
        addProperty(removeSelector);
        addProperty(removeButton);
        for (int i = 0; i < points.size(); i++) {
            removeSelector.addOption(i, std::format("Point #{}", i + 1));
        }
    }
}

void ScreenSpaceInsetBlackout::BlackoutShape::Spline::addPoint() {
    // Add new position to raw data
    const int index = addSelector;
    data.insert(data.begin() + index, newPointPosition);
}

void ScreenSpaceInsetBlackout::BlackoutShape::Spline::removePoint() {
    // Remove raw data for the deleted point
    const int indexToRemove = removeSelector;
    data.erase(data.begin() + indexToRemove);
 }

ScreenSpaceInsetBlackout::BlackoutShape::Corners::Corners(std::vector<glm::vec2>& inData)
    : properties::PropertyOwner({ "Corners" , "Corners", "" })
    , data(inData)
{
    // Create corner Points (TopLeft -> TopRight -> BottomRight -> BottomLeft)
    points.push_back(std::make_unique<Point>(data[0], "TopLeft", "Top-Left"));
    points.push_back(std::make_unique<Point>(data[1], "TopRight", "Top-Right"));
    points.push_back(std::make_unique<Point>(data[2], "BottomRight", "Bottom-Right"));
    points.push_back(std::make_unique<Point>(data[3], "BottomLeft", "Bottom-Left"));

    // OnChange functions and add to GUI
    for (int i = 0; i < 4; i++) {
        points[i]->prop->onChange([this, i]() {
            points[i]->updateData();
            dataHasChanged = true;
        });
        addProperty(points[i]->prop.get());
    }
}

ScreenSpaceInsetBlackout::BlackoutShape::BlackoutShape(
                                                      const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "BlackoutShape", "Blackout Shape", "" })
    , enableCalibrationColor(CalibrationColorInfo, false)
    , enableCalibrationPattern(CalibrationPatternInfo, false)
    , calibrationTexturePath(CalibrationTextureInfo)
    , copyToClipboardTrigger(CopyToClipboardInfo)
{
    const Parameters params = codegen::bake<Parameters>(dictionary);

    cornerData = params.blackoutshape.corners;
    corners = std::unique_ptr<Corners>(new Corners(cornerData));
    addPropertySubOwner(*corners);

    topSplineData = params.blackoutshape.top.value_or(std::vector<glm::vec2>());
    topSpline = std::unique_ptr<Spline>(new Spline(topSplineData, "Top"));
    addPropertySubOwner(*topSpline);

    rightSplineData = params.blackoutshape.right.value_or(std::vector<glm::vec2>());
    rightSpline = std::unique_ptr<Spline>(new Spline(rightSplineData, "Right"));
    addPropertySubOwner(*rightSpline);

    bottomSplineData = params.blackoutshape.bottom.value_or(std::vector<glm::vec2>());
    bottomSpline = std::unique_ptr<Spline>(new Spline(bottomSplineData, "Bottom"));
    addPropertySubOwner(*bottomSpline);

    leftSplineData = params.blackoutshape.left.value_or(std::vector<glm::vec2>());
    leftSpline = std::unique_ptr<Spline>(new Spline(leftSplineData, "Left"));
    addPropertySubOwner(*leftSpline);

    // Add additional controls to GUI
    enableCalibrationPattern.onChange([this]() {
        textureTypeHasChanged = true;
    });
    addProperty(enableCalibrationPattern);

    enableCalibrationColor.onChange([this]() {
        textureTypeHasChanged = true;
    });
    addProperty(enableCalibrationColor);

    copyToClipboardTrigger.onChange([this]() {
        copyToClipboard(
            cornerData,
            topSplineData,
            rightSplineData,
            bottomSplineData,
            leftSplineData
        );
    });
    addProperty(copyToClipboardTrigger);
}

bool ScreenSpaceInsetBlackout::BlackoutShape::checkHasChanged() {
    return textureTypeHasChanged ||
        corners->dataHasChanged ||
        topSpline->dataHasChanged ||
        rightSpline->dataHasChanged ||
        bottomSpline->dataHasChanged ||
        leftSpline->dataHasChanged;
}

void ScreenSpaceInsetBlackout::BlackoutShape::resetHasChanged() {
    textureTypeHasChanged = false;
    corners->dataHasChanged = false;
    topSpline->dataHasChanged = false;
    rightSpline->dataHasChanged = false;
    bottomSpline->dataHasChanged = false;
    leftSpline->dataHasChanged = false;
}

void ScreenSpaceInsetBlackout::BlackoutShape::checkAndUpdateGUI() {
    std::array<std::reference_wrapper<std::unique_ptr<Spline>>,4> refs = {
        std::ref(topSpline),
        std::ref(rightSpline),
        std::ref(bottomSpline),
        std::ref(leftSpline)
    };

    // Check which splines that needs to be updated and perform correct actions
    // Note: Need to remove propertySubOwner and add it again for GUI to work properly
    bool updatePropertyTree = false;
    for (std::unique_ptr<Spline>& spline : refs) {
        if (spline->pointAdded) {
            std::string baseString = spline->base;
            std::vector<glm::vec2>& dataRef = spline->data;
            removePropertySubOwner(*spline);
            spline->addPoint();
            spline.reset();
            spline = std::unique_ptr<Spline>(new Spline(dataRef, baseString));
            spline->dataHasChanged = true;
            updatePropertyTree = true;
        }
        else if (spline->pointRemoved) {
            std::string baseString = spline->base;
            std::vector<glm::vec2>& dataRef = spline->data;
            removePropertySubOwner(*spline);
            spline->removePoint();
            spline.reset();
            spline = std::unique_ptr<Spline>(new Spline(dataRef, baseString));
            spline->dataHasChanged = true;
            updatePropertyTree = true;
        }
    }

    // Remove GUI elements so that we can add them in correct order again
    if (updatePropertyTree) {
        std::vector<openspace::properties::PropertyOwner*> subs = propertySubOwners();
        for (int i = 0; i < subs.size(); i++) {
            removePropertySubOwner(subs[i]);
        }
        addPropertySubOwner(*corners);
        for (std::unique_ptr<Spline>& spline : refs) {
            addPropertySubOwner(*spline);
        }
    }
}

ScreenSpaceInsetBlackout::ScreenSpaceInsetBlackout(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _blackoutShape(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    // Makes sure that User has specified 4 corners in the asset file
    checkCornerSpecification(p.blackoutshape.corners);

    addPropertySubOwner(_blackoutShape);

    // Handling of calibration texture
    std::optional<std::filesystem::path> optTexturePath =
        p.blackoutshape.calibrationTexturePath;
    if (optTexturePath.has_value()) {
        if (std::filesystem::is_regular_file(*optTexturePath)) {
            std::unique_ptr<ghoul::opengl::Texture> texture =
                ghoul::io::TextureReader::ref().loadTexture(*optTexturePath, 2);
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
            }
        }
        else {
            LWARNINGC(
                "ScreenSpaceInsetBlackout",
                std::format(
                    "Could not find calibration texture '{}'.",
                    *optTexturePath
                )
            );
        }
    }
}

void ScreenSpaceInsetBlackout::initializeGL() {
    ScreenSpaceRenderable::initializeGL();

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

    _blackoutTexture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(BlackoutTextureSize, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGBA
    );

    _blackoutTexture->bind();
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA,
        BlackoutTextureSize.x,
        BlackoutTextureSize.y,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        nullptr
    );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *_blackoutTexture,
        0
    );

    _blackoutTexture->purgeFromRAM();

    glBindTexture(GL_TEXTURE_2D, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    _uniformCache.color = _fboProgram->uniformLocation("color");

    // Generate vertex data and texture
    generateVertexArrayData();
    generateTexture();
}

void ScreenSpaceInsetBlackout::deinitializeGL() {
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

    ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceInsetBlackout::update() {
    _blackoutShape.checkAndUpdateGUI();
    if (_blackoutShape.checkHasChanged()) {
        generateVertexArrayData();
        generateTexture();
        _blackoutShape.resetHasChanged();
    }
}

void ScreenSpaceInsetBlackout::generateVertexArrayData() {
    // Clear old data
    _vboData.clear();

    // Set vertex buffer data based on calibration pattern or blackout shape
    if (_blackoutShape.enableCalibrationPattern && _calibrationTexture.get()) {
        _vboData = {
            glm::vec2(-1.f, 1.f),
            glm::vec2(1.f, 1.f),
            glm::vec2(1.f, -1.f),
            glm::vec2(-1.f, -1.f)
        };
        return;
    }

    // Create vector with a temporary element as first element
    std::vector<glm::vec2> pointsTop = { glm::vec2(0.f) };
    std::vector<glm::vec2> pointsRight = { glm::vec2(0.f) };
    std::vector<glm::vec2> pointsBottom = { glm::vec2(0.f) };
    std::vector<glm::vec2> pointsLeft = { glm::vec2(0.f) };

    // Push first corner to each spline
    pointsTop.push_back(_blackoutShape.cornerData[0]);
    pointsRight.push_back(_blackoutShape.cornerData[1]);
    pointsBottom.push_back(_blackoutShape.cornerData[2]);
    pointsLeft.push_back(_blackoutShape.cornerData[3]);

    // Insert the existing control points for each spline
    pointsTop.insert(
        pointsTop.begin() + 2,
        _blackoutShape.topSplineData.begin(),
        _blackoutShape.topSplineData.end()
    );
    pointsRight.insert(
        pointsRight.begin() + 2,
        _blackoutShape.rightSplineData.begin(),
        _blackoutShape.rightSplineData.end()
    );
    pointsBottom.insert(
        pointsBottom.begin() + 2,
        _blackoutShape.bottomSplineData.begin(),
        _blackoutShape.bottomSplineData.end()
    );
    pointsLeft.insert(
        pointsLeft.begin() + 2,
        _blackoutShape.leftSplineData.begin(),
        _blackoutShape.leftSplineData.end()
    );

    // Push the last corner for each spline
    pointsTop.push_back(_blackoutShape.cornerData[1]);
    pointsRight.push_back(_blackoutShape.cornerData[2]);
    pointsBottom.push_back(_blackoutShape.cornerData[3]);
    pointsLeft.push_back(_blackoutShape.cornerData[0]);

    // Calculates the extra first and last point needed during spline sampling step
    std::pair<glm::vec2, glm::vec2> tPad = calculatePadding(pointsTop);
    std::pair<glm::vec2, glm::vec2> rPad = calculatePadding(pointsRight);
    std::pair<glm::vec2, glm::vec2> bPad = calculatePadding(pointsBottom);
    std::pair<glm::vec2, glm::vec2> lPad = calculatePadding(pointsLeft);

    // Adds the extra points
    pointsTop[0] = tPad.first;
    pointsRight[0] = rPad.first;
    pointsBottom[0] = bPad.first;
    pointsLeft[0] = lPad.first;
    pointsTop.push_back(tPad.second);
    pointsRight.push_back(rPad.second);
    pointsBottom.push_back(bPad.second);
    pointsLeft.push_back(lPad.second);

    // Samples the spline and returns all points along the spline curve
    std::vector<glm::vec2> splineTop = sampleSpline(pointsTop);
    std::vector<glm::vec2> splineRight = sampleSpline(pointsRight);
    std::vector<glm::vec2> splineBottom = sampleSpline(pointsBottom);
    std::vector<glm::vec2> splineLeft = sampleSpline(pointsLeft);

    // Translate points from range [0,1] to [-1,1] for X and Y
    offsetCoordinates(splineTop);
    offsetCoordinates(splineRight);
    offsetCoordinates(splineBottom);
    offsetCoordinates(splineLeft);

    // Incoming vertex data is: top -> right -> bottom -> left (clockwise)
    // VBO data needs to be counter-clockwise for correct winding
    // Also adds extra point first and last for Triangle Fan drawing
    _vboData.push_back(glm::vec2(0.f, 0.f));
    _vboData.insert(_vboData.end(), splineTop.rbegin(), splineTop.rend());
    _vboData.insert(_vboData.end(), splineLeft.rbegin(), splineLeft.rend());
    _vboData.insert(_vboData.end(), splineBottom.rbegin(), splineBottom.rend());
    _vboData.insert(_vboData.end(), splineRight.rbegin(), splineRight.rend());
    _vboData.push_back(splineTop.back());
}

void ScreenSpaceInsetBlackout::generateTexture() {
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
    _fboProgram->setUniform(
        "color",
        _blackoutShape.enableCalibrationColor ? glm::vec3(0.f, 1.f, 0.f) : glm::vec3(0.f)
    );

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);

    bindTexture();

    // Clear current buffer
    glViewport(0, 0, BlackoutTextureSize.x, BlackoutTextureSize.y);
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
}

void ScreenSpaceInsetBlackout::bindTexture() {
    if (_blackoutShape.enableCalibrationPattern && _calibrationTexture.get()) {
        _calibrationTexture->bind();
        _objectSize = _calibrationTexture->dimensions();
    }
    else {
        _blackoutTexture->bind();
        _objectSize = _blackoutTexture->dimensions();
    }
}

} // namespace openspace
