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

#ifndef __OPENSPACE_MODULE_BASE___SCREENSPACEINSETBLACKOUT___H__
#define __OPENSPACE_MODULE_BASE___SCREENSPACEINSETBLACKOUT___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/vector/vec2property.h>

namespace openspace {

namespace documentation { struct Documentation; }

class ScreenSpaceInsetBlackout : public ScreenSpaceRenderable {
public:
    explicit ScreenSpaceInsetBlackout(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;
    void update() override;

    static documentation::Documentation Documentation();

private:
    class BlackoutShape : public properties::PropertyOwner {
    public:
        struct Point {
            Point(glm::vec2& inData, std::string identifier, std::string guiName);

            void updateData();

            std::unique_ptr<properties::Property::PropertyInfo> propInfo = nullptr;
            std::unique_ptr<properties::Vec2Property> prop = nullptr;
            /// Pointer to data used when user modifies the point position
            glm::vec2* dataptr;
        };

        class Spline : public properties::PropertyOwner {
        public:
            Spline(std::vector<glm::vec2>& inData, std::string baseString);

            void addPoint();
            void removePoint();

            /// Vector of references to the original data
            std::vector<glm::vec2>& data;
            std::string base;
            bool pointAdded = false;
            bool pointRemoved = false;
            bool dataHasChanged = false;

        private:
            /// Control points for the spline
            std::vector<std::unique_ptr<Point>> points;
            /// Position for the new point used when adding a new point to a spline
            properties::Vec2Property newPointPosition;
            /// Selects which place in the list where the new point should be inserted
            properties::OptionProperty addSelector;
            /// Adds a new point based on given position and place in the list
            properties::TriggerProperty addButton;
            /// Selects which point to remove
            properties::OptionProperty removeSelector;
            /// Removes a point
            properties::TriggerProperty removeButton;
        };

        class Corners : public properties::PropertyOwner {
        public:
            explicit Corners(std::vector<glm::vec2>& inData);

            /// Vector of references to the original data
            std::vector<glm::vec2>& data;
            bool dataHasChanged = false;

        private:
            /// Control points for the spline
            std::vector<std::unique_ptr<Point>> points;
        };

        explicit BlackoutShape(const ghoul::Dictionary& dictionary);

        bool checkHasChanged();
        void resetHasChanged();
        void checkAndUpdateGUI();

        std::vector<glm::vec2> cornerData;
        std::vector<glm::vec2> topSplineData;
        std::vector<glm::vec2> rightSplineData;
        std::vector<glm::vec2> bottomSplineData;
        std::vector<glm::vec2> leftSplineData;

        /**
         * Enables a brighted color for the shape which makes it easier to see the
         * boundaries of the shape during setup
         **/
        properties::BoolProperty enableCalibrationColor;

        /**
         * Enables a calibration texture instead of the shape which can be used to check
         * which position values to use during shape setup
         **/
        properties::BoolProperty enableCalibrationPattern;

    private:
        std::unique_ptr<Corners> corners;
        std::unique_ptr<Spline> topSpline;
        std::unique_ptr<Spline> rightSpline;
        std::unique_ptr<Spline> bottomSpline;
        std::unique_ptr<Spline> leftSpline;
        bool textureTypeHasChanged = false;
        properties::StringProperty calibrationTexturePath;
        properties::TriggerProperty copyToClipboardTrigger;
    };

    void bindTexture() override;

    void generateTexture();
    void generateVertexArrayData();

    BlackoutShape _blackoutShape;
    std::vector<glm::vec2> _vboData;
    std::unique_ptr<ghoul::opengl::Texture> _blackoutTexture;
    std::unique_ptr<ghoul::opengl::Texture> _calibrationTexture;
    GLuint _vao = 0;
    GLuint _vbo = 0;
    GLuint _fbo = 0;
    ghoul::opengl::ProgramObject* _fboProgram = nullptr;
    UniformCache(color) _uniformCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___SCREENSPACEINSETBLACKOUT___H__
