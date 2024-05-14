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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLEORBITALKEPLER___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLEORBITALKEPLER___H__

#include <openspace/rendering/renderable.h>

#include <modules/base/rendering/renderabletrail.h>
#include <modules/space/kepler.h>
#include <modules/space/translation/keplertranslation.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <ghoul/glm.h>
#include <ghoul/misc/objectmanager.h>
#include <ghoul/opengl/programobject.h>

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableOrbitalKepler : public Renderable {
public:
    RenderableOrbitalKepler(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;
    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();

private:
    struct Appearance : properties::PropertyOwner {
        Appearance();
        /// Specifies the base color of the line/point
        properties::Vec3Property color;
        /// Line width for the line rendering part
        properties::FloatProperty trailWidth;
        /// Point size exponent for the point rendering part
        properties::FloatProperty pointSizeExponent;
        /// The option determining which rendering method to use
        properties::BoolProperty enableMaxSize;
        /// The option enables or disables Max Angular Size limit
        properties::FloatProperty maxSize;
        /// Max angular size between vector cameraToPoint and edge of the point
        properties::OptionProperty renderingModes;
        /// Specifies a multiplicative factor that fades out the trail line
        properties::FloatProperty trailFade;
        /// Specifies if the point outline should be enabled
        properties::BoolProperty enableOutline;
        /// Specifies the color of the point outline
        properties::Vec3Property outlineColor;
        /// Specifies how much if the point should be covered by the outline
        properties::FloatProperty outlineWidth;
    };

    void updateBuffers();

    bool _updateDataBuffersAtNextRender = false;
    std::streamoff _numObjects;
    std::vector<GLint> _segmentSize;
    std::vector<GLint> _startIndex;
    properties::UIntProperty _segmentQuality;
    properties::UIntProperty _startRenderIdx;
    properties::UIntProperty _sizeRender;

    /// The layout of the VBOs
    struct TrailVBOLayout {
        float x = 0.f;
        float y = 0.f;
        float z = 0.f;
        float time = 0.f;
        double epoch = 0.0;
        double period = 0.0;
    };

    /// The backend storage for the vertex buffer object containing all points
    std::vector<TrailVBOLayout> _vertexBufferData;

    GLuint _vertexArray;
    GLuint _vertexBuffer;

    ghoul::opengl::ProgramObject* _trailProgram;
    ghoul::opengl::ProgramObject* _pointProgram;
    properties::StringProperty _path;
    properties::BoolProperty _contiguousMode;
    kepler::Format _format;
    RenderableOrbitalKepler::Appearance _appearance;

    // Line cache
    UniformCache(modelView, projection, trailFade, inGameTime, color, opacity)
        _uniformTrailCache;
    
    // Point cache
    UniformCache(modelTransform, viewTransform, projectionTransform,
        cameraPositionWorld, cameraUpWorld,  inGameTime, color,
        pointSizeExponent, enableMaxSize, maxSize, enableOutline,
        outlineColor, outlineWeight, opacity)
        _uniformPointCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLEORBITALKEPLER___H__

