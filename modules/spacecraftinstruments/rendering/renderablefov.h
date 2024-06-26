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

#ifndef __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLEFOV___H__
#define __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLEFOV___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableFov : public Renderable {
public:
    RenderableFov(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    /**
     * Checks the field of view of the instrument for the current \p time against all of
     * the potential targets are returns the first name of the target that is in field of
     * view, the previous target, or the closest target to the space craft. The second
     * return value is whether the target is currently in the field of view.
     */
    std::pair<std::string, bool> determineTarget(double time);

    void updateGPU();

    void computeIntercepts(double time, const std::string& target,
        bool isInFov);

    glm::dvec3 orthogonalProjection(const glm::dvec3& vecFov, double time,
        const std::string& target) const;

    // properties
    properties::FloatProperty _lineWidth;
    properties::DoubleProperty _standOffDistance;
    properties::BoolProperty _alwaysDrawFov;
    ghoul::opengl::ProgramObject* _program = nullptr;
    UniformCache(modelViewProjectionTransform, colorStart, colorEnd,
        activeColor, targetInFieldOfViewColor, intersectionStartColor,
        intersectionEndColor, squareColor, interpolation) _uniformCache;

    bool _simplifyBounds = false;

    std::string _previousTarget;
    bool _drawFOV = false;

    struct {
        std::string spacecraft;
        std::string name;
        std::string referenceFrame;
        SpiceManager::AberrationCorrection aberrationCorrection;

        std::vector<glm::dvec3> bounds;
        glm::dvec3 boresight = glm::dvec3(0.0);
        std::vector<std::string> potentialTargets;
    } _instrument;

    float _interpolationTime = 0.f;

    struct RenderInformation {
        // Differentiating different vertex types
        using VertexColorType = int32_t;
        // This needs to be synced with the fov_vs.glsl shader
        static constexpr VertexColorType VertexColorTypeDefaultStart = 0;
        static constexpr VertexColorType VertexColorTypeDefaultEnd = 1;
        static constexpr VertexColorType VertexColorTypeInFieldOfView = 2;
        static constexpr VertexColorType VertexColorTypeActive = 3;
        static constexpr VertexColorType VertexColorTypeIntersectionStart = 4;
        static constexpr VertexColorType VertexColorTypeIntersectionEnd = 5;
        static constexpr VertexColorType VertexColorTypeSquare = 6;

        struct VBOData {
            GLfloat position[3];
            VertexColorType color;
        };

        GLuint vao = 0;
        GLuint vbo = 0;
        // @SPEEDUP: Add an ibo to reduce the number of vertices drawn
        std::vector<VBOData> data;
        bool isDirty = true;
    };

    RenderInformation _orthogonalPlane;
    RenderInformation _fieldOfViewBounds;

    struct {
        properties::PropertyOwner container;

        /// Start color for uninteresting times
        properties::Vec3Property defaultStart;
        /// End color for uninteresting times
        properties::Vec3Property defaultEnd;
        /// Color use when a field-of-view is projecting
        properties::Vec3Property active;
        /// Color to use for target in fov
        properties::Vec3Property targetInFieldOfView;
        /// Color at the start of intersection
        properties::Vec3Property intersectionStart;
        /// Color at the end of intersection
        properties::Vec3Property intersectionEnd;
        /// Color for the orthogonal square
        properties::Vec3Property square;
    } _colors;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACECRAFTINSTRUMENTS___RENDERABLEFOV___H__
