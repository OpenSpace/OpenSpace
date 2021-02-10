/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <modules/space/translation/keplertranslation.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <ghoul/glm.h>
#include <ghoul/misc/objectmanager.h>
#include <ghoul/opengl/programobject.h>

namespace {
    static const openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the data file to read"
    };
    static const openspace::properties::Property::PropertyInfo SegmentQualityInfo = {
        "SegmentQuality",
        "Segment Quality",
        "A segment quality value for the orbital trail. A value from 1 (lowest) to "
        "10 (highest) that controls the number of line segments in the rendering of the "
        "orbital trail. This does not control the direct number of segments because "
        "these automatically increase according to the eccentricity of the orbit."
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the trail if the selected rendering "
        "method includes lines. If the rendering mode is set to Points, this value is "
        "ignored."
    };
    constexpr openspace::properties::Property::PropertyInfo LineColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB main color for the lines and points of the trail."
    };
    constexpr openspace::properties::Property::PropertyInfo TrailFadeInfo = {
        "TrailFade",
        "Trail Fade",
        "This value determines how fast the trail fades and is an appearance property. "
    };
    static const openspace::properties::Property::PropertyInfo UpperLimitInfo = {
        "UpperLimit",
        "Upper Limit",
        "Upper limit on the number of objects for this renderable, regardless of "
        "how many objects are contained in the data file. Produces an evenly-distributed"
        "sample from the data file."
    };
    static const openspace::properties::Property::PropertyInfo StartRenderIdxInfo = {
        "StartRenderIdx",
        "Contiguous Starting Index of Render",
        "Index of object in renderable group to start rendering (all prior objects will "
        "be ignored)."
    };
    static const openspace::properties::Property::PropertyInfo RenderSizeInfo = {
        "RenderSize",
        "Contiguous Size of Render Block",
        "Number of objects to render sequentially from StartRenderIdx"
    };
    constexpr openspace::properties::Property::PropertyInfo RenderBinModeInfo = {
        "RenderBinMode",
        "RenderBin Mode",
        "Determines if the trails will be rendered after all other elements, including"
        "atmospheres if needed."
    };
}

namespace openspace {

class RenderableOrbitalKepler : public Renderable {
public:
    RenderableOrbitalKepler(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;

    /**
        * Reads the provided data file and calls the KeplerTranslation::setKeplerElments
        * method with the correct values. If \p filename is a valid data file but contains
        * disallowed values (see KeplerTranslation::setKeplerElements), a
        * KeplerTranslation::RangeError is thrown.
        *
        * \param filename The path to the file that contains the data file.
        *
        * \throw ghoul::RuntimeError if the data file does not exist or there is a
        *        problem with its format.
        * \pre The \p filename must exist
        */
    virtual void readDataFile(const std::string& filename) = 0;

protected:
    double calculateSemiMajorAxis(double meanMotion) const;
    double epochFromSubstring(const std::string& epochString) const;
    double epochFromYMDdSubstring(const std::string& epochString);

    std::function<void()> _reinitializeTrailBuffers;
    std::function<void()> _updateStartRenderIdxSelect;
    std::function<void()> _updateRenderSizeSelect;
    std::function<void()> _updateRenderUpperLimitSelect;

    struct KeplerParameters {
        double inclination = 0.0;
        double semiMajorAxis = 0.0;
        double ascendingNode = 0.0;
        double eccentricity = 0.0;
        double argumentOfPeriapsis = 0.0;
        double meanAnomaly = 0.0;
        double meanMotion = 0.0;
        double epoch = 0.0;
        double period = 0.0;
    };
    struct PropsDependentOnFileData {
        bool startRenderIdx = false;
        bool sizeRender = false;
        bool upperLimit = false;
    } _propsDefinedInAssetFlag;

    bool _updateDataBuffersAtNextRender = false;
    std::streamoff _numObjects;
    bool _isFileReadinitialized = false;
    inline static constexpr double convertAuToKm = 1.496e8;
    inline static constexpr double convertDaysToSecs = 86400.0;
    std::vector<KeplerParameters> _data;
    std::vector<size_t> _segmentSize;
    properties::UIntProperty _upperLimit;
    properties::UIntProperty _segmentQuality;
    properties::Property::OnChangeHandle _upperLimitCallbackHandle;
    properties::UIntProperty _startRenderIdx;
    properties::UIntProperty _sizeRender;
    properties::Property::OnChangeHandle _startRenderIdxCallbackHandle;
    properties::Property::OnChangeHandle _sizeRenderCallbackHandle;

private:
    struct Vertex {
        glm::vec3 position = glm::vec3(0.f);
        glm::vec3 color = glm::vec3(0.f);
        glm::vec2 texcoord = glm::vec2(0.f);
    };

    /// The layout of the VBOs
    struct TrailVBOLayout {
        float x = 0.f;
        float y = 0.f;
        float z = 0.f;
        float time = 0.f;
        double epoch = 0.0;
        double period = 0.0;
    };

    KeplerTranslation _keplerTranslator;

    /// The backend storage for the vertex buffer object containing all points for this
    /// trail.
    std::vector<TrailVBOLayout> _vertexBufferData;

    GLuint _vertexArray;
    GLuint _vertexBuffer;

    void updateBuffers();

    ghoul::opengl::ProgramObject* _programObject;
    properties::StringProperty _path;
    RenderableTrail::Appearance _appearance;
    glm::vec3 _position = glm::vec3(0.f);

    UniformCache(modelView, projection, lineFade, inGameTime, color, opacity,
        numberOfSegments) _uniformCache;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLEORBITALKEPLER___H__

