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

#ifndef __OPENSPACE_MODULE_BASE___SCREENSPACEINSETBLACKOUT___H__
#define __OPENSPACE_MODULE_BASE___SCREENSPACEINSETBLACKOUT___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/properties/vector/vec2property.h>

#include <openspace/properties/misc/optionproperty.h>
#include <shared_mutex>
#include <deque>

namespace openspace {

namespace documentation { struct Documentation; }

class ScreenSpaceInsetBlackout : public ScreenSpaceRenderable {
public:
    ScreenSpaceInsetBlackout(const ghoul::Dictionary& dictionary);

    bool deinitializeGL() override;
    void update() override;

    static documentation::Documentation Documentation();

private:
    struct BlackoutShape;

    // Struct that defines a spline segment
    struct Spline : properties::PropertyOwner {
        enum class Side {
            Top = 0,
            Right,
            Bottom,
            Left
        };

        explicit Spline(const ghoul::Dictionary& dictionary, BlackoutShape& shape,
            const std::string identifier, const std::string guiName,
            const std::string basePointIdentifier, const std::string basePointName,
            const Spline::Side side);

        // Struct to make sure that we always use same size of char array
        struct char16 {
            char cstr[16];
        };

        // Struct that holds information about each point on the spline
        struct Point {
            explicit Point(glm::vec2& dataRef, const std::string& id, const std::string& name);
            ~Point();
            char16 guiName;
            char16 propIdentifier;
            properties::Property::PropertyInfo* propInfo = nullptr;
            properties::Vec2Property* prop = nullptr;
            glm::vec2* rawDataPointer;

            void updateRawDataPointerValue();
        };

        const std::string baseIdentifier;
        const std::string baseGuiName;

        BlackoutShape& parentShape;
        std::vector<glm::vec2> rawData;
        std::vector<std::unique_ptr<Point>> points;

        bool isTextureDirty = false;
        bool addPointFlag = false;
        bool removePointFlag = false;

        Spline::Side shapeSide;
        properties::TriggerProperty addControlPoint;
        properties::TriggerProperty removeControlPoint;
        properties::OptionProperty removeSelector;
        properties::OptionProperty addSelector;
        properties::Vec2Property newPointPosition;

        void generateProperties();
        void addSelectionOptions();
        void createPropertyTree();
        void updatePropertyTree();
        void cleanPropertyTree();

        void addPoint();
        void removePoint();
        void updateCornerData();

        /**
        * Synchronizes the shared data for corner0 and corner1 to corresponding Point
        * in points vector.
        **/
        void syncCornerData();
    };

    // Main struct that contains the splines and controls the blackout shape
    struct BlackoutShape : properties::PropertyOwner {
        explicit BlackoutShape(const ghoul::Dictionary& dictionary);
        Spline top;
        Spline right;
        Spline bottom;
        Spline left;

        properties::TriggerProperty copyToClipboardTrigger;
        properties::BoolProperty enableCalibrationPattern;
        properties::BoolProperty enableCalibrationColor;
        properties::StringProperty calibrationTexturePath;

        bool isTextureDirty = false;

        // Holds corner points as they need to be shared between adjacent splines
        glm::vec2 topLeftCorner;
        glm::vec2 topRightCorner;
        glm::vec2 bottomRightCorner;
        glm::vec2 bottomLeftCorner;
        std::shared_mutex cornerMutex;

        void copyToClipboard();
        std::string formatLine(std::string id, const std::vector<glm::vec2>& data,
			const bool isCorner = false);
        bool checkTextureStatus();
        void updateSplineAndGui();
        void resetDirtyTextureFlag();
        void setCornerData(const Spline::Side& side, const glm::vec2 corner0,
            const glm::vec2 corner1);
        std::pair<glm::vec2, glm::vec2> readCornerData(const Spline::Side& side);
    };
    BlackoutShape _shape;

    std::vector<glm::vec2> _vboData;
    GLuint _vao = 0;
    GLuint _vbo = 0;
    GLuint _fbo = 0;
    std::unique_ptr<ghoul::opengl::Texture> _blackoutTexture;
    std::unique_ptr<ghoul::opengl::Texture> _calibrationTexture;

    void bindTexture() override;

    void checkCornerSpecification(std::vector<glm::vec2> corners);
    void initializeShadersAndFBO();
    void generateTexture();
    void generateVertexArray();
    void copyToPointsVector(const std::vector<std::unique_ptr<Spline::Point>>& points,
        std::vector<glm::vec2>& vertexData);
    glm::vec2 calculateCatmullRom(const glm::vec2 &p0, const glm::vec2 &p1,
        const glm::vec2 &p2, const glm::vec2 &p3, const float t);
    std::vector<glm::vec2> sampleSpline(const std::vector<glm::vec2> &controlPoints);
    void offsetCoordinates(std::vector<glm::vec2> &vec);
    std::pair<glm::vec2, glm::vec2> calculatePadding(
	    const std::vector<std::unique_ptr<ScreenSpaceInsetBlackout::Spline::Point>>& pVec);

    // Program
    ghoul::opengl::ProgramObject* _fboProgram = nullptr;

    // Uniform Cache
    UniformCache(color) _uniformCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___SCREENSPACEINSETBLACKOUT___H__
