/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__

#include <openspace/rendering/renderable.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/vector/vec4property.h>


#include <modules/fieldlinessequence/util/fieldlinesstate.h>

namespace openspace {

class RenderableFieldlinesSequence : public Renderable {
public:
    RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary);
    // ~RenderableFieldlinesSequence();

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

    bool isWithinSequenceInterval();
    void updateActiveStateIndex();
private:
    ghoul::Dictionary _vectorVolumeInfo;
    ghoul::Dictionary _fieldlineInfo;
    ghoul::Dictionary _seedPointsInfo;

    // Properties
    properties::BoolProperty _isMorphing;

    properties::IntProperty _timeMultiplier;
    properties::IntProperty _fieldlineParticleSize;
    properties::IntProperty _modulusDivider;

    properties::Vec4Property _fieldlineColor;
    properties::Vec4Property _fieldlineParticleColor;

    std::vector<glm::vec3> _seedPoints;
    std::vector<FieldlinesState> _states;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;

    bool _shouldRender; // only temporary
    bool _needsUpdate;
    // bool _isMorphing;

    GLuint _vertexArrayObject;

    // TODO: Make an array instead?
    GLuint _vertexPositionBuffer;
    GLuint _vertexColorBuffer;
    GLuint _morphToPositionBuffer;
    GLuint _quickMorphBuffer;

    int _activeStateIndex;
    int _numberOfStates;
    double _seqStartTime; // redundant, but hey.. nice n clear
    double _seqEndTime;
    double _currentTime;
    float _stateProgress;

    std::vector<double> _startTimes;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
