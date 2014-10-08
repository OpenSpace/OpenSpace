/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <openspace/scenegraph/spiceephemeris.h>

#include <openspace/util/constants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace {
    const std::string _loggerCat = "SpiceEphemeris";
}

namespace openspace {
    
using namespace constants::spiceephemeris;
    
SpiceEphemeris::SpiceEphemeris(const ghoul::Dictionary& dictionary)
    : _targetName("")
    , _originName("")
    , _position()
{
    const bool hasBody = dictionary.getValue(keyBody, _targetName);
    if (!hasBody)
        LERROR("SpiceEphemeris does not contain the key '" << keyBody << "'");

    const bool hasObserver = dictionary.getValue(keyOrigin, _originName);
    if (!hasObserver)
        LERROR("SpiceEphemeris does not contain the key '" << keyOrigin << "'");

	ghoul::Dictionary kernels;
	dictionary.getValue(keyKernels, kernels);
	for (size_t i = 1; i <= kernels.size(); ++i) {
		std::string kernel;
		bool success = kernels.getValue(std::to_string(i), kernel);
		if (!success)
			LERROR("'" << keyKernels << "' has to be an array-style table");

		SpiceManager::ref().loadKernel(kernel);
	}
}
    
SpiceEphemeris::~SpiceEphemeris() {}

bool SpiceEphemeris::initialize()
{
    //if (!_targetName.empty() && !_originName.empty()) {
    //    int bsuccess = 0;
    //    int osuccess = 0;
    //    Spice::ref().bod_NameToInt(_targetName, &_target, &bsuccess);
    //    Spice::ref().bod_NameToInt(_originName, &_origin, &osuccess);
    //    
    //    if (bsuccess && osuccess)
    //        return true;
    //}
    //
    return true;
}

const psc& SpiceEphemeris::position() const {
    return _position;
}

void SpiceEphemeris::update(const UpdateData& data) {
    double state[3];
   
	glm::dvec3 position(0,0,0);

	double lightTime = 0.0;
	SpiceManager::ref().getTargetPosition(_targetName, _originName, "GALACTIC", "LT+S", data.time, position, lightTime);

	/*
	std::cout << _targetName  << " (";
	std::cout << position[0] << ", ";
	std::cout << position[1] << ", ";
	std::cout << position[2] << ")";
	std::cout << std::endl;
	assert(_targetName != "JUPITER");
	*/
	_position = psc::CreatePowerScaledCoordinate(position.x, position.y, position.z);
	//_position[3] += 1;
	//_position[3] += 3;

}

} // namespace openspace