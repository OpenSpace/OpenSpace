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

namespace openspace {

namespace documentation { struct Documentation; }

class ScreenSpaceInsetBlackout : public ScreenSpaceRenderable {
public:
    ScreenSpaceInsetBlackout(const ghoul::Dictionary& dictionary);

    bool deinitializeGL() override;
    void update() override;

    static documentation::Documentation Documentation();

private:
    class BlackoutShape : public properties::PropertyOwner {
    public:
        class PointOwner : public properties::PropertyOwner {
        public:
            class Point {
            public:
                Point(glm::vec2& inData, std::string identifier, std::string guiName);

                void updateData();

                std::unique_ptr<properties::Property::PropertyInfo> propInfo = nullptr;
                std::unique_ptr<properties::Vec2Property> prop = nullptr;
                glm::vec2* dataptr;
            };

            PointOwner(std::vector<glm::vec2>& inData, std::string identifier,
                std::string guiName);

            std::vector<glm::vec2>& data;
            std::vector<std::unique_ptr<Point>> points;
            bool dataHasChanged = false;
        };

        class Spline : public PointOwner {
        public:
            Spline (std::vector<glm::vec2>& inData, std::string baseString);

            void addPoint();
            void removePoint();

            std::string base;
            bool pointAdded = false;
            bool pointRemoved = false;

        private:
            void buildTree();

            properties::Vec2Property newPointPosition;
            properties::OptionProperty addSelector;
            properties::TriggerProperty addButton;
            properties::OptionProperty removeSelector;
            properties::TriggerProperty removeButton;            
        };

        class Corners : public PointOwner {
        public:
            explicit Corners(std::vector<glm::vec2>& inData);
        };

        explicit BlackoutShape(const ghoul::Dictionary& dictionary);
        ~BlackoutShape();

        bool checkHasChanged();
        void resetHasChanged();
        void checkAndUpdateGUI();

        std::vector<glm::vec2> cornerData;
        std::vector<glm::vec2> topSplineData;
        std::vector<glm::vec2> rightSplineData;
        std::vector<glm::vec2> bottomSplineData;
        std::vector<glm::vec2> leftSplineData;
        properties::BoolProperty enableCalibrationColor;
        properties::BoolProperty enableCalibrationPattern;

    private:
        void copyToClipboard();
        std::string formatLine(std::string id, const std::vector<glm::vec2>& data);

        Corners* corners;
        Spline* topSpline;
        Spline* rightSpline;
        Spline* bottomSpline;
        Spline* leftSpline;
        bool textureTypeHasChanged = false;
        properties::StringProperty calibrationTexturePath;
        properties::TriggerProperty copyToClipboardTrigger;
    };

    void bindTexture() override;

    void checkCornerSpecification(std::vector<glm::vec2> corners);
    void initializeShadersAndFBO();
    void generateTexture();
    void generateVertexArrayData();

    std::pair<glm::vec2, glm::vec2> calculatePadding(const std::vector<glm::vec2>& pVec);
    std::vector<glm::vec2> sampleSpline(const std::vector<glm::vec2>& controlPoints);
    glm::vec2 calculateCatmullRom(float t, const glm::vec2& p0, const glm::vec2& p1,
        const glm::vec2& p2, const glm::vec2& p3);
    void offsetCoordinates(std::vector<glm::vec2>& vec);

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
