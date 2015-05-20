/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <modules/base/rendering/renderabletrail.h>
#include <openspace/util/time.h>

#include <openspace/util/constants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/misc/highresclock.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>


/* TODO for this class:
*  In order to add geometry shader (for pretty-draw),
*  need to pack each consecutive point pair into a vec2
*  in order to draw quad between them. 
*/

namespace {
    const std::string _loggerCat = "RenderableTrail";
    //constants
	const std::string keyName                    = "Name";
        const std::string keyBody                = "Body";
        const std::string keyObserver            = "Observer";
        const std::string keyFrame               = "Frame";
        const std::string keyPathModule          = "ModulePath";
        const std::string keyColor               = "RGB";
        const std::string keyTropicalOrbitPeriod = "TropicalOrbitPeriod";
        const std::string keyEarthOrbitRatio     = "EarthOrbitRatio";
        const std::string keyDayLength           = "DayLength";
		const std::string keyStamps				 = "TimeStamps";
}

namespace openspace {

RenderableTrail::RenderableTrail(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineColor("lineColor", "Line Color")
    , _lineFade("lineFade", "Line Fade", 0.75f, 0.f, 5.f)
    , _lineWidth("lineWidth", "Line Width", 2.f, 1.f, 20.f)
    , _showTimestamps("timestamps", "Show Timestamps", false)
    , _programObject(nullptr)
    , _successfullDictionaryFetch(true)
    , _vaoID(0)
    , _vBufferID(0)
    , _needsSweep(true)
    , _oldTime(std::numeric_limits<float>::max())
{
    _successfullDictionaryFetch &= dictionary.getValue(keyBody, _target);
    _successfullDictionaryFetch &= dictionary.getValue(keyObserver, _observer);
    _successfullDictionaryFetch &= dictionary.getValue(keyFrame, _frame);
    _successfullDictionaryFetch &= dictionary.getValue(keyTropicalOrbitPeriod, _tropic);
    _successfullDictionaryFetch &= dictionary.getValue(keyEarthOrbitRatio, _ratio);
    _successfullDictionaryFetch &= dictionary.getValue(keyDayLength, _day);

    // values in modfiles set from here
    // http://nssdc.gsfc.nasa.gov/planetary/factsheet/marsfact.html

    glm::vec3 color(0.f);
    if (dictionary.hasKeyAndValue<glm::vec3>(keyColor))
        dictionary.getValue(keyColor, color);
    _lineColor = color;

	bool timeStamps = false;
	if (dictionary.hasKeyAndValue<bool>(keyStamps))
		dictionary.getValue(keyStamps, timeStamps);
	_showTimestamps = timeStamps;
	addProperty(_showTimestamps);

    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_lineColor);

    addProperty(_lineFade);

    addProperty(_lineWidth);
	_distanceFade = 1.0;
}

bool RenderableTrail::initialize() {
    if (!_successfullDictionaryFetch) {
        LERROR("The following keys need to be set in the Dictionary. Cannot initialize!");
        LERROR(keyBody << ": " << _target);
        LERROR(keyObserver << ": " << _observer);
        LERROR(keyFrame << ": " << _frame);
        LERROR(keyTropicalOrbitPeriod << ": " << _tropic);
        LERROR(keyEarthOrbitRatio << ": " << _ratio);
        LERROR(keyDayLength << ": " << _day);
        return false;
    }

    bool completeSuccess = true;
    _programObject = ghoul::opengl::ProgramObject::Build("EphemerisProgram",
        "${SHADERS}/modules/trails/ephemeris_vs.glsl",
        "${SHADERS}/modules/trails/ephemeris_fs.glsl");
    if (!_programObject)
        return false;

    return completeSuccess;
}

bool RenderableTrail::deinitialize() {
    glDeleteVertexArrays(1, &_vaoID);
    glDeleteBuffers(1, &_vBufferID);
    return true;
	deinitialize();
}

bool RenderableTrail::isReady() const {
    return (_programObject != nullptr) && _successfullDictionaryFetch;
}

void RenderableTrail::render(const RenderData& data) {
    _programObject->activate();
    psc currentPosition = data.position;
    psc campos = data.camera.position();
    glm::mat4 camrot = data.camera.viewRotationMatrix();

    glm::mat4 transform = glm::mat4(1);

    // setup the data to the shader
    _programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _programObject->setUniform("ModelTransform", transform);
    setPscUniforms(_programObject, &data.camera, data.position);

    _programObject->setUniform("color", _lineColor);
    _programObject->setUniform("nVertices", static_cast<unsigned int>(_vertexArray.size()));
    _programObject->setUniform("lineFade", _lineFade);
	_programObject->setUniform("forceFade", _distanceFade);

	//const psc& position = data.camera.position();
	//const psc& origin = openspace::OpenSpaceEngine::ref().interactionHandler()->focusNode()->worldPosition();
	//const PowerScaledScalar& pssl = (position - origin).length();
	//
	//if (pssl[0] < 0.000001){
	//	if (_distanceFade > 0.0f) _distanceFade -= 0.05f;
	//	_programObject->setUniform("forceFade", _distanceFade);
	//}
	//else{
	//	if (_distanceFade < 1.0f) _distanceFade += 0.05f;
	//	_programObject->setUniform("forceFade", _distanceFade);
	//}

    glLineWidth(_lineWidth);

    glBindVertexArray(_vaoID);
    glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(_vertexArray.size()));
    glBindVertexArray(0);

    glLineWidth(1.f);

	if (_showTimestamps){
		glPointSize(5.f);
		glBindVertexArray(_vaoID);
		glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_vertexArray.size()));
		glBindVertexArray(0);
	}

    _programObject->deactivate();
}

void RenderableTrail::update(const UpdateData& data) {
	if (data.isTimeJump)
        _needsSweep = true;

    if (_needsSweep) {
        fullYearSweep(data.time);
        sendToGPU();
        _needsSweep = false;
        return;
    }

    if (_programObject->isDirty())
        _programObject->rebuildFromFile();
    double lightTime = 0.0;
    psc pscPos;

	bool intervalSet = hasTimeInterval();
	double start = DBL_MIN;
	double end = DBL_MAX;
	if (intervalSet) {
		getInterval(start, end);
	}

    // Points in the vertex array should always have a fixed distance. For this reason we
    // keep the first entry in the array floating and always pointing to the current date
    // As soon as the time difference between the current time and the last time is bigger
    // than the fixed distance, we need to create a new fixed point
    double deltaTime = std::abs(data.time - _oldTime);
    int nValues = static_cast<int>(floor(deltaTime / _increment));

    // Update the floating current time
	if (start > data.time)
		SpiceManager::ref().getTargetPosition(_target, _observer, _frame, "NONE", start, pscPos, lightTime);
	else if (end < data.time)
		SpiceManager::ref().getTargetPosition(_target, _observer, _frame, "NONE", end, pscPos, lightTime);
	else
		SpiceManager::ref().getTargetPosition(_target, _observer, _frame, "NONE", data.time, pscPos, lightTime);

    pscPos[3] += 3; // KM to M
    _vertexArray[0] = { pscPos[0], pscPos[1], pscPos[2], pscPos[3] };

    if (nValues != 0) {
        // If we have new values to create, we do that here. nValues should always be
        // close to 1

        // But you never know
        nValues = std::min(nValues, int(_vertexArray.size() - 1));
        //LINFO(nValues);
        std::vector<TrailVBOLayout> tmp = _vertexArray;

        for (int i = nValues; i > 0; --i) {
            double et = _oldTime + i * _increment;
			if (start > et)
				et = start;
			else if (end < et)
				et = end;
			SpiceManager::ref().getTargetPosition(_target, _observer, _frame, "NONE", et, pscPos, lightTime);
			pscPos[3] += 3;
            _vertexArray[i] = { pscPos[0], pscPos[1], pscPos[2], pscPos[3] };
        }

        for (size_t i = 0; i < tmp.size() - (nValues + 1); ++i)
            _vertexArray[nValues + 1 + i] = tmp[i + 1];
        _oldTime += nValues * _increment;
    }

    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vertexArray.size() * sizeof(TrailVBOLayout), &_vertexArray[0]);
}

/* This algorithm estimates and precomputes the number of segments required for
*  any planetary object in space, given a tropical orbit period and earth-to-planet 
*  orbit ratio. In doing so, it finds the exact increment of time corresponding
*  to a planetary year. 
*  Therefore all planets need said constants, for other objects we need a different,
*  and most likely heuristic measure to easily estimate a nodal time-increment.
*  Trivial, yet - a TODO. 
*/
void RenderableTrail::fullYearSweep(double time) {
    const int SecondsPerEarthYear = 31540000;

    double lightTime = 0.0;
    float planetYear = SecondsPerEarthYear * _ratio;
    int segments = static_cast<int>(_tropic);

	bool intervalSet = hasTimeInterval();
	double start = DBL_MIN;
	double end = DBL_MAX;
	if (intervalSet) {
		getInterval(start, end);
	}

    _increment = planetYear / _tropic;
    
    _oldTime = time;

    psc pscPos;
    _vertexArray.resize(segments+2);
    for (int i = 0; i < segments+2; i++) {
		//if (start > time)
		//	time = start;
		//else if (end < time)
		//	time = end;

        SpiceManager::ref().getTargetPosition(_target, _observer, _frame, "NONE", time, pscPos, lightTime);
		pscPos[3] += 3;

        _vertexArray[i] = {pscPos[0], pscPos[1], pscPos[2], pscPos[3]};
        time -= _increment;
    }

}

void RenderableTrail::sendToGPU() {
    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vBufferID);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBufferData(GL_ARRAY_BUFFER, _vertexArray.size() * sizeof(TrailVBOLayout), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vertexArray.size() * sizeof(TrailVBOLayout), &_vertexArray[0]);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
    glBindVertexArray(0);
}

} // namespace openspace
