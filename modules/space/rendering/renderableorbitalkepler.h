/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/space/kepler.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/programobject.h>
#include <limits>

namespace openspace {

class RenderableOrbitalKepler : public Renderable {
public:
    explicit RenderableOrbitalKepler(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;
    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static openspace::Documentation Documentation();

private:
    struct Appearance : PropertyOwner {
        Appearance();

        /// Specifies the base color of the line/point
        Vec3Property color;
        /// Line width for the line rendering part
        FloatProperty trailWidth;
        /// Point size exponent for the point rendering part
        FloatProperty pointSizeExponent;
        /// The option determining which rendering method to use
        BoolProperty enableMaxSize;
        /// The option enables or disables Max Angular Size limit
        FloatProperty maxSize;
        /// Max angular size between vector cameraToPoint and edge of the point
        OptionProperty renderingModes;
        /// Specifies rendering orientation when rendering points
        OptionProperty pointRenderOption;
        /// Specifies a multiplicative factor that fades out the trail line
        FloatProperty trailFade;
        /// Specifies if the point outline should be enabled
        BoolProperty enableOutline;
        /// Specifies the color of the point outline
        Vec3Property outlineColor;
        /// Specifies how much if the point should be covered by the outline
        FloatProperty outlineWidth;

        bool isRenderTypeDirty = false;
    };

    void updateBuffers();
    void threadedSegmentCalculations(int threadId, const UpdateData& data);

    const int _nThreads = 0;
    std::vector<int> _threadIds;
    std::vector<int> _orbitsPerThread;
    std::vector<int> _vertexBufferOffset;

    bool _renderTrails = false;
    bool _renderPoints = false;
    bool _forceUpdate = false;
    bool _updateDataBuffersAtNextRender = false;

    unsigned int _nOrbits = 0;
    GLsizei _lineDrawCount = 0;
    std::vector<GLint> _segmentsPerOrbit;
    std::vector<GLint> _startIndexPoints;
    std::vector<GLint> _segmentSizePoints;
    std::vector<GLint> _startIndexTrails;
    std::vector<GLint> _segmentSizeTrails;
    std::vector<kepler::Parameters> _parameters;

    /// Extra data for more efficient updating of vectors
    struct UpdateInfo {
        double timestamp = std::numeric_limits<double>::min();
        double timePerStep = 0.0;
    };
    std::vector<UpdateInfo> _updateHelper;

    /// The layout of the VBOs
    struct TrailVBOLayout {
        float x = 0.f;
        float y = 0.f;
        float z = 0.f;
        double time = 0.0;
        double epoch = 0.0;
        double period = 0.0;
    };
    /// The backend storage for the vertex buffer object containing all points
    std::vector<TrailVBOLayout> _vertexBufferData;

    ghoul::opengl::ProgramObject* _trailProgram = nullptr;
    ghoul::opengl::ProgramObject* _pointProgram = nullptr;
    UIntProperty _segmentQuality;
    UIntProperty _startRenderIdx;
    UIntProperty _sizeRender;
    StringProperty _path;
    BoolProperty _contiguousMode;
    kepler::Format _format;
    RenderableOrbitalKepler::Appearance _appearance;

    GLuint _vertexArray = 0;
    GLuint _vertexBuffer = 0;

    // Line cache
    UniformCache(modelViewTransform, projectionTransform, trailFadeExponent,
        colorFadeCutoffValue, inGameTime, color, opacity) _uniformTrailCache;

    // Point cache
    UniformCache(modelTransform, viewTransform, projectionTransform, renderOption,
        cameraViewDirectionUp, cameraViewDirectionRight, cameraPositionWorld,
        cameraUpWorld, inGameTime, color, pointSizeExponent, enableMaxSize, maxSize,
        enableOutline, outlineColor, outlineWeight, opacity) _uniformPointCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLEORBITALKEPLER___H__
