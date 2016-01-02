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

#include <openspace/util/spicemanager.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>

#include <glm/gtc/type_ptr.hpp>

#include <algorithm>
#include <format.h>

namespace {
	const std::string _loggerCat = "SpiceManager";
    
    // The value comes from
    // http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getmsg_c.html
    // as the maximum message length
    const unsigned SpiceErrorBufferSize = 1841;
    
    // This method checks if one of the previous SPICE methods has failed. If it has, an
    // exception with the SPICE error message is thrown
    void throwOnSpiceError(std::string errorMessage) {
        SpiceBoolean failed = failed_c();
        if (failed) {
            char buffer[SpiceErrorBufferSize];
            getmsg_c("LONG", SpiceErrorBufferSize, buffer);
            reset_c();
            throw openspace::SpiceManager::SpiceException(
                errorMessage + ": " + buffer
            );
        }
    }
    
    const char* toString(openspace::SpiceManager::FieldOfViewMethod m) {
        switch (m) {
            case openspace::SpiceManager::FieldOfViewMethod::Ellipsoid:
                return "ELLIPSOID";
            case openspace::SpiceManager::FieldOfViewMethod::Point:
                return "POINT";
        }
    }
    
    const char* toString(openspace::SpiceManager::TerminatorType t) {
        switch (t) {
            case openspace::SpiceManager::TerminatorType::Umbral:
                return "UMBRAL";
            case openspace::SpiceManager::TerminatorType::Penumbral:
                return "PENUMBRAL";
        }
    }
}

using fmt::format;
using std::string;

namespace openspace {
    
    
SpiceManager::SpiceException::SpiceException(const string& msg)
    : ghoul::RuntimeError(msg, "Spice")
{}

    
    
SpiceManager::AberrationCorrection::AberrationCorrection(Type t, Direction d)
    : type(t)
    , direction(d)
{}
    
SpiceManager::AberrationCorrection::AberrationCorrection(const std::string& identifier) {
    const static std::map<std::string, std::pair<Type, Direction>> Mapping =  {
        { "NONE"  , { Type::None, Direction::Reception } },
        { "LT"    , { Type::LightTime, Direction::Reception } },
        { "LT+S"  , { Type::LightTimeStellar, Direction::Reception } },
        { "CN"    , { Type::ConvergedNewtonian, Direction::Reception } },
        { "CN+S"  , { Type::ConvergedNewtonianStellar, Direction::Reception } },
        { "XLT"   , { Type::LightTime, Direction::Transmission } },
        { "XLT+S" , { Type::LightTimeStellar, Direction::Transmission } },
        { "XCN"   , { Type::ConvergedNewtonian, Direction::Transmission } },
        { "XCN+S" , { Type::ConvergedNewtonianStellar, Direction::Transmission } }
    };

    auto it = Mapping.find(identifier);
    
    ghoul_assert(!identifier.empty(), "Identifier may not be empty");
    ghoul_assert(it != Mapping.end(), format("Invalid identifer '{}'", identifier));
    
    type = it->second.first;
    direction = it->second.second;
}
    
SpiceManager::AberrationCorrection::operator const char*() const {
    switch (type) {
        case Type::None:
            return "NONE";
        case Type::LightTime:
            return (direction == Direction::Reception) ? "LT" : "XLT";
        case Type::LightTimeStellar:
            return (direction == Direction::Reception) ? "LT+S" : "XLT+S";
        case Type::ConvergedNewtonian:
            return (direction == Direction::Reception) ? "CN" : "XCN";
        case Type::ConvergedNewtonianStellar:
            return (direction == Direction::Reception) ? "CN+S" : "XCN+S";
    }
}
    
SpiceManager::FieldOfViewMethod SpiceManager::fieldOfViewMethodFromString(
    const string& method)
{
    const static std::map<string, FieldOfViewMethod> Mapping = {
        { "ELLIPSOID", FieldOfViewMethod::Ellipsoid },
        { "POINT", FieldOfViewMethod::Point }
    };
    
    ghoul_assert(!method.empty(), "Method must not be empty");
    
    return Mapping.at(method);
}
    
SpiceManager::TerminatorType SpiceManager::terminatorTypeFromString( const string& type) {
    const static std::map<string, TerminatorType> Mapping = {
        { "UMBRAL", TerminatorType::Umbral },
        { "PENUMBRAL", TerminatorType::Penumbral }
    };
    
    ghoul_assert(!type.empty(), "Type must not be empty");
    
    return Mapping.at(type);
}

SpiceManager::SpiceManager() {
    // Set the SPICE library to not exit the program if an error occurs
    erract_c("SET", 0, const_cast<char*>("REPORT"));
    // But we do not want SPICE to print the errors, we will fetch them ourselves
    errprt_c("SET", 0, const_cast<char*>("NONE"));
}

SpiceManager::~SpiceManager() {
    for (const KernelInformation& i : _loadedKernels)
        unload_c(i.path.c_str());

    // Set values back to default
    erract_c("SET", 0, const_cast<char*>("DEFAULT"));
    errprt_c("SET", 0, const_cast<char*>("DEFAULT"));
}


SpiceManager::KernelHandle SpiceManager::loadKernel(string filePath) {
    ghoul_assert(!filePath.empty(), "Empty file path");
    ghoul_assert(
        FileSys.fileExists(filePath),
        format(
            "File '{}' ('{}') does not exist",
            filePath,
            absPath(filePath)
        )
    );
    ghoul_assert(
        FileSys.directoryExists(ghoul::filesystem::File(filePath).directoryName()),
        format(
            "File '{}' exists, but directory '{}' doesn't",
            absPath(filePath),
            ghoul::filesystem::File(filePath).directoryName()
        )
    );
    
	string path = absPath(filePath);
    auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [path](const KernelInformation& info) { return info.path == path; });

    if (it != _loadedKernels.end()) {
        it->refCount++;
        LDEBUG(
            format(
                "Kernel '{}' was already loaded. New reference count: {}",
                path,
                it->refCount
            )
        );
        return it->id;
    }

    // We need to set the current directory as meta-kernels are usually defined relative
    // to the directory they reside in. The directory change is not necessary for regular
    // kernels
    ghoul::filesystem::Directory currentDirectory = FileSys.currentDirectory();
    string fileDirectory = ghoul::filesystem::File(path, true).directoryName();
	FileSys.setCurrentDirectory(fileDirectory);

    LINFO("Loading SPICE kernel '" << path << "'");
    // Load the kernel
	furnsh_c(path.c_str());

    // Reset the current directory to the previous one
    FileSys.setCurrentDirectory(currentDirectory);

    throwOnSpiceError("Kernel loading");
    
    string fileExtension = ghoul::filesystem::File(path, true).fileExtension();
    if (fileExtension == ".bc" || fileExtension == ".BC")
		findCkCoverage(path); // binary ck kernel
	else if (fileExtension == "bsp" || fileExtension == "BSP")
		findSpkCoverage(path); // binary spk kernel

    KernelHandle kernelId = ++_lastAssignedKernel;
    ghoul_assert(kernelId != 0, fmt::format("Kernel Handle wrapped around to 0"));
    _loadedKernels.push_back({std::move(path), kernelId, 1});
    return kernelId;
}

void SpiceManager::unloadKernel(KernelHandle kernelId) {
    ghoul_assert(kernelId <= _lastAssignedKernel, "Invalid unassigned kernel");
    ghoul_assert(kernelId != KernelHandle(0), "Invalid zero handle");
    
	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&kernelId](const KernelInformation& info) { return info.id == kernelId; });

	if (it != _loadedKernels.end()) {
        // If there was only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
		    // No need to check for errors as we do not allow empty path names
            LINFO(format("Unloading SPICE kernel '{}'", it->path));
		    unload_c(it->path.c_str());
		    _loadedKernels.erase(it);
        }
        // Otherwise, we hold on to it, but reduce the reference counter by 1
        else {
            it->refCount--;
            LDEBUG(format("Reducing reference counter to: {}", it->refCount));
        }
	}
}

void SpiceManager::unloadKernel(string filePath) {
    ghoul_assert(!filePath.empty(), "Empty filename");

    string path = absPath(filePath);

	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&path](const KernelInformation& info) { return info.path == path; });
    
    if (it == _loadedKernels.end()) {
        throw SpiceException(
            format("'{}' did not correspond to a loaded kernel", path)
        );
    }
	else {
        // If there was only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
            LINFO(format("Unloading SPICE kernel '{}'", path));
            unload_c(path.c_str());
            _loadedKernels.erase(it);
        }
        else {
            // Otherwise, we hold on to it, but reduce the reference counter by 1
            it->refCount--;
            LDEBUG(format("Reducing reference counter to: {}", it->refCount));
        }
	}
}

bool SpiceManager::hasSpkCoverage(const string& target, double et) const {
    ghoul_assert(!target.empty(), "Empty target");
    
    int id = naifId(target);
    auto it = _spkIntervals.find(id);
    if (it != _spkIntervals.end()) {
        std::vector<std::pair<double, double>> intervalVector = it->second;
        for (const auto& vecElement : intervalVector) {
            if (vecElement.first < et && vecElement.second > et)
                return true;
        }
    }
    return false;
}

bool SpiceManager::hasCkCoverage(const string& frame, double et) const {
    ghoul_assert(!frame.empty(), "Empty target");
    
    int id = frameId(frame);
    
    auto it = _ckIntervals.find(id);
    if (it != _ckIntervals.end()) {
        std::vector<std::pair<double, double>> intervalVector = it->second;
        for (const auto& i : intervalVector) {
            if (i.first < et && i.second > et)
                return true;
        }
    }
    return false;
}

bool SpiceManager::hasValue(int naifId, const string& item) const {
	return bodfnd_c(naifId, item.c_str());
}

bool SpiceManager::hasValue(const string& body, const string& item) const {
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!item.empty(), "Empty item");
    
	int id = naifId(body);
    return hasValue(id, item);
}

int SpiceManager::naifId(const string& body) const {
    ghoul_assert(!body.empty(), "Empty body");
    
    SpiceBoolean success;
    SpiceInt id;
    bods2c_c(body.c_str(), &id, &success);
    if (!success)
        throw SpiceException(format("Could not find NAIF ID of body '{}'", body));
    return id;
}
    
bool SpiceManager::hasNaifId(const string& body) const {
    ghoul_assert(!body.empty(), "Empty body");
    
    SpiceBoolean success;
    SpiceInt id;
    bods2c_c(body.c_str(), &id, &success);
    reset_c();
    return success;
}

int SpiceManager::frameId(const string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");
    
    SpiceInt id;
    namfrm_c(frame.c_str(), &id);
    if (id == 0)
        throw SpiceException(format("Could not find NAIF ID of frame '{}'", frame));
    return id;
}

bool SpiceManager::hasFrameId(const string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");
    
    SpiceInt id;
    namfrm_c(frame.c_str(), &id);
    return id != 0;
}
    
void getValueInternal(const string& body, const string& value, int size,
	double* v)
{
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!value.empty(), "Empty value");
    ghoul_assert(v != nullptr, "Empty value pointer");

	SpiceInt n;
	bodvrd_c(body.c_str(), value.c_str(), size, &n, v);

    throwOnSpiceError(
        format("Error getting value '{}' for body '{}'",
               value,
               body
        )
    );
}

void SpiceManager::getValue(const string& body, const string& value, double& v) const {
	getValueInternal(body, value, 1, &v);
}

void SpiceManager::getValue(const string& body, const string& value,
                            glm::dvec2& v) const
{
    getValueInternal(body, value, 2, glm::value_ptr(v));
}
    
void SpiceManager::getValue(const string& body, const string& value,
                            glm::dvec3& v) const
{
	getValueInternal(body, value, 3, glm::value_ptr(v));
}

void SpiceManager::getValue(const string& body, const string& value,
	glm::dvec4& v) const
{
	getValueInternal(body, value, 4, glm::value_ptr(v));
}

void SpiceManager::getValue(const string& body, const string& value,
	std::vector<double>& v) const 
{
    ghoul_assert(!v.empty(), "Array for values has to be preallocaed");

    getValueInternal(body, value, v.size(), v.data());
}

double SpiceManager::spacecraftClockToET(const string& craft, double craftTicks) {
    ghoul_assert(!craft.empty(), "Empty craft");

    int craftId = naifId(craft);
    double et;
	sct2e_c(craftId, craftTicks, &et);
    throwOnSpiceError(format(
        "Error transforming spacecraft clock of '{}' at time {}",
        craft, craftTicks)
    );
	return et;
}

double SpiceManager::ephemerisTimeFromDate(const std::string& timeString) const {
    ghoul_assert(!timeString.empty(), "Empty timeString");

    double et;
	str2et_c(timeString.c_str(), &et);
    throwOnSpiceError(format("Error converting date '{}'", timeString));
    return et;
}

string SpiceManager::dateFromEphemerisTime(double ephemerisTime,
    const string& formatString) const
{
    ghoul_assert(!formatString.empty(), "Format is empty");
    
    static const int BufferSize = 256;
    SpiceChar buffer[BufferSize];
    timout_c(ephemerisTime, formatString.c_str(), BufferSize - 1, buffer);
    throwOnSpiceError(
        format("Error converting ephemeris time '{}' to date with format '{}'",
               ephemerisTime,
               formatString
        )
    );
    
    return std::string(buffer);
    
}

glm::dvec3 SpiceManager::targetPosition(const string& target, const string& observer,
    const string& referenceFrame, AberrationCorrection aberrationCorrection,
    double ephemerisTime, double& lightTime) const
{
    ghoul_assert(!target.empty(), "Target is not empty");
    ghoul_assert(!observer.empty(), "Observer is not empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame is not empty");
    
	bool targetHasCoverage = hasSpkCoverage(target, ephemerisTime);
	bool observerHasCoverage = hasSpkCoverage(observer, ephemerisTime);
	if (!targetHasCoverage && !observerHasCoverage){
        throw SpiceException(
            format("Neither the target '{}' nor observer '{}' has SPK coverage",
                   target,
                   observer
            )
        );
	}
	else if (targetHasCoverage && observerHasCoverage) {
        glm::dvec3 position;
		spkpos_c(
            target.c_str(),
            ephemerisTime,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(position),
			&lightTime
        );
        throwOnSpiceError(format(
            "Error getting target position from '{}' to '{}' in reference frame '{}",
            target,
            observer,
            referenceFrame
        ));
        return position;
	}
        else if (targetHasCoverage) {
            // observer has no coverage
            return getEstimatedPosition(
                observer,
                target,
                referenceFrame,
                aberrationCorrection,
                ephemerisTime,
                lightTime
            ) * -1.0;
        }
        else {
            // target has no coverage
            return getEstimatedPosition(
                target,
                observer,
                referenceFrame,
                aberrationCorrection,
                ephemerisTime,
                lightTime
            );
        }
}

glm::dmat3 SpiceManager::frameTransformationMatrix(const string& from, const string& to,
    double ephemerisTime) const
{
    ghoul_assert(!from.empty(), "From must not be empty");
    ghoul_assert(!to.empty(), "To must not be empty");
    
    // get rotation matrix from frame A - frame B
    glm::dmat3 transform;
	pxform_c(
        from.c_str(),
        to.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(transform))
    );
	
    throwOnSpiceError(
        format("Error converting from frame '{}' to frame '{}' at time '{}'",
               from, to, ephemerisTime
        )
    );

    // The rox-major, column-major order are switched in GLM and SPICE, so we have to
    // transpose the matrix before we can return it
    return glm::transpose(transform);
}

SpiceManager::SurfaceInterceptResult SpiceManager::surfaceIntercept(
    const string& target, const string& observer, const string& fovFrame,
    const string& referenceFrame, AberrationCorrection aberrationCorrection,
    double ephemerisTime, const glm::dvec3& directionVector) const
{
    ghoul_assert(!target.empty(), "Target must not be empty");
    ghoul_assert(!observer.empty(), "Observer must not be empty");
    ghoul_assert(target != observer, "Target and observer must be different");
    ghoul_assert(!fovFrame.empty(), "FOV frame must not be empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame must not be empty");
    ghoul_assert(directionVector != glm::dvec3(0.0), "Direction vector must not be zero");
    
    const std::string ComputationMethod = "ELLIPSOID";
    
    SurfaceInterceptResult result;
    
    SpiceBoolean found;
    sincpt_c(ComputationMethod.c_str(),
        target.c_str(),
        ephemerisTime,
        referenceFrame.c_str(),
        aberrationCorrection,
        observer.c_str(),
        fovFrame.c_str(),
        glm::value_ptr(directionVector),
        glm::value_ptr(result.surfaceIntercept),
        &result.interceptEpoch,
        glm::value_ptr(result.surfaceVector),
        &found
    );
    result.interceptFound = (found == SPICETRUE);
    
    throwOnSpiceError(format(
        "Error retrieving surface intercept on target '{}' viewed from observer '{}' in "
        "reference frame '{}' at time '{}'",
        target, observer, referenceFrame, ephemerisTime
    ));

    return result;
}

bool SpiceManager::isTargetInFieldOfView(const string& target, const string& observer,
    const string& referenceFrame, const string& instrument, FieldOfViewMethod method,
    AberrationCorrection aberrationCorrection, double& ephemerisTime) const
{
    ghoul_assert(!target.empty(), "Target must not be empty");
    ghoul_assert(!observer.empty(), "Observer must not be empty");
    ghoul_assert(target != observer, "Target and observer must be different");
    ghoul_assert(!referenceFrame.empty(), "Reference frame must not be empty");
    ghoul_assert(!instrument.empty(), "Instrument must not be empty");
    
	int visible;
	fovtrg_c(instrument.c_str(),
        target.c_str(),
        toString(method),
        referenceFrame.c_str(),
        aberrationCorrection,
        observer.c_str(),
        &ephemerisTime,
        &visible
    );
    
    throwOnSpiceError(format(
        "Checking if target '{}' is in view of instrument '{}' failed",
        target, instrument
    ));

	return visible == SPICETRUE;
}

bool SpiceManager::isTargetInFieldOfView(const string& target, const string& observer,
    const string& instrument, FieldOfViewMethod method,
    AberrationCorrection aberrationCorrection, double& ephemerisTime) const
{
    return isTargetInFieldOfView(
        target,
        observer,
        frameFromBody(target),
        instrument,
        method,
        aberrationCorrection,
        ephemerisTime
    );
}

SpiceManager::TargetStateResult SpiceManager::targetState(const string& target,
    const string& observer, const string& referenceFrame,
    AberrationCorrection aberrationCorrection, double ephemerisTime) const
{
    ghoul_assert(!target.empty(), "Target must not be empty");
    ghoul_assert(!observer.empty(), "Observer must not be empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame must not be empty");
    
    TargetStateResult result;
    result.lightTime = 0.0;

    double buffer[6];

	spkezr_c(
        target.c_str(),
        ephemerisTime,
        referenceFrame.c_str(),
        aberrationCorrection,
        observer.c_str(),
        buffer,
        &result.lightTime
    );

    throwOnSpiceError(format(
        "Error retrieving state of target '{}' viewed from observer '{}' in reference "
        "frame '{}' at time '{}'",
        target, observer, referenceFrame, ephemerisTime
    ));

    memmove(glm::value_ptr(result.position), buffer, sizeof(double) * 3);
    memmove(glm::value_ptr(result.velocity), buffer + 3, sizeof(double) * 3);
    return result;
}

SpiceManager::TransformMatrix SpiceManager::stateTransformMatrix(const string& fromFrame,
    const string& toFrame, double ephemerisTime) const
{
    ghoul_assert(!fromFrame.empty(), "fromFrame must not be empty");
    ghoul_assert(!toFrame.empty(), "toFrame must not be empty");
    
    TransformMatrix m;
	sxform_c(
        fromFrame.c_str(),
        toFrame.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[6]>(m.data())
    );
    throwOnSpiceError(format(
        "Error retrieved state transform matrix from frame '{}' to frame '{}' at time "
        "'{}'",
        fromFrame, toFrame, ephemerisTime
    ));
    return m;
}

glm::dmat3 SpiceManager::positionTransformMatrix(const string& fromFrame,
    const string& toFrame, double ephemerisTime) const
{
    ghoul_assert(!fromFrame.empty(), "fromFrame must not be empty");
    ghoul_assert(!toFrame.empty(), "toFrame must not be empty");
    
    glm::dmat3 result;
	pxform_c(
        fromFrame.c_str(),
        toFrame.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
    );

	SpiceBoolean success = !(failed_c());
    reset_c();
    bool estimated = false;
	if (!success)
		result = getEstimatedTransformMatrix(fromFrame, toFrame, ephemerisTime);

    return glm::transpose(result);
}

glm::dmat3 SpiceManager::positionTransformMatrix(const string& fromFrame,
    const string& toFrame, double ephemerisTimeFrom, double ephemerisTimeTo) const
{
    ghoul_assert(!fromFrame.empty(), "fromFrame must not be empty");
    ghoul_assert(!toFrame.empty(), "toFrame must not be empty");
    
    glm::dmat3 result;

	pxfrm2_c(
        fromFrame.c_str(),
        toFrame.c_str(),
        ephemerisTimeFrom,
        ephemerisTimeTo,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
    );
    throwOnSpiceError(format(
        "Error retrieving position transform matrix from '{}' at time '{}' to frame '{}' "
        "at time '{}'",
        fromFrame, ephemerisTimeFrom, toFrame, ephemerisTimeTo
    ));
    return glm::transpose(result);
}

SpiceManager::FieldOfViewResult SpiceManager::fieldOfView(const string& instrument) const
{
    ghoul_assert(!instrument.empty(), "Instrument must not be empty");
    return fieldOfView(naifId(instrument));
}

SpiceManager::FieldOfViewResult SpiceManager::fieldOfView(int instrument) const {
	static const int MaxBoundsSize = 20;
	static const int BufferSize = 128;

    FieldOfViewResult res;

	SpiceInt nrReturned;
	double boundsArr[MaxBoundsSize][3];
    char fovShapeBuffer[BufferSize];
    char frameNameBuffer[BufferSize];
	getfov_c(instrument,                        // instrument id
        MaxBoundsSize,                          // maximum size for the bounds vector
        BufferSize,                             // maximum size for the fov shape buffer
		BufferSize,                             // maximum size for the frame name buffer
		fovShapeBuffer,                         // the fov shape buffer
		frameNameBuffer,                        // the frame name buffer
		glm::value_ptr(res.boresightVector),    // the boresight vector
		&nrReturned,                            // the number of returned array values
		boundsArr                               // the bounds
    );
    
    throwOnSpiceError(format(
        "Error getting field-of-view parameters for instrument '{}'", instrument
    ));

    res.bounds.reserve(nrReturned);
	for (int i = 0; i < nrReturned; ++i)
        res.bounds.emplace_back(boundsArr[i][0], boundsArr[i][1], boundsArr[i][2]);

    string shape = string(fovShapeBuffer);
    static const std::map<string, FieldOfViewResult::Shape> Map = {
        { "POLYGON", FieldOfViewResult::Shape::Polygon },
        { "RECTANGLE" , FieldOfViewResult::Shape::Rectangle },
        { "CIRCLE", FieldOfViewResult::Shape::Circle },
        { "ELLIPSE", FieldOfViewResult::Shape::Ellipse }
    };
    res.shape = Map.at(shape);
    res.frameName = string(frameNameBuffer);

	return res;
}
    
SpiceManager::TerminatorEllipseResult SpiceManager::terminatorEllipse(
    const string& target, const string& observer, const string& frame,
    const string& lightSource, TerminatorType terminatorType,
    AberrationCorrection aberrationCorrection, double ephemerisTime,
    int numberOfTerminatorPoints)
{
    ghoul_assert(!target.empty(), "Target must not be empty");
    ghoul_assert(!observer.empty(), "Observer must not be empty");
    ghoul_assert(!frame.empty(), "Frame must not be empty");
    ghoul_assert(!lightSource.empty(), "Light source must not be empty");
    ghoul_assert(numberOfTerminatorPoints >= 1, "Terminator points must be >= 1");
    
    TerminatorEllipseResult res;
    
    // Warning: This assumes std::vector<glm::dvec3> to have all values memory contiguous
    res.terminatorPoints.resize(numberOfTerminatorPoints);

	edterm_c(toString(terminatorType),
		     lightSource.c_str(), 
			 target.c_str(), 
			 ephemerisTime, 
			 frame.c_str(), 
			 aberrationCorrection,
			 observer.c_str(),
		     numberOfTerminatorPoints,
			 &res.targetEphemerisTime,
			 glm::value_ptr(res.observerPosition),
			 (double(*)[3])res.terminatorPoints.data()
    );
    throwOnSpiceError(format(
        "Error getting terminator ellipse for target '{}' from observer '{}' in frame "
        "'{}' with light source '{}' at time '{}'",
        target, observer, frame, lightSource, ephemerisTime
    ));
	return res;
}

bool SpiceManager::addFrame(std::string body, std::string frame) {
    if (body == "" || frame == "")
        return false;
    else {
        _frameByBody.push_back(std::make_pair(body, frame));
        return true;
    }
}

std::string SpiceManager::frameFromBody(const std::string body) const {
	for (auto pair : _frameByBody) {
		if (pair.first == body) {
			return pair.second;
		}
	}

	std::string unionPrefix = "IAU_";
	std::string frame = "";

	if (body.find(unionPrefix) == std::string::npos)
		frame = unionPrefix + body;
	else
		frame = body;

	return frame;
}

void SpiceManager::findCkCoverage(const std::string& path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(FileSys.fileExists(path), format("File '{}' does not exist", path));
    
    const unsigned int MaxObj = 64;
    const unsigned int WinSiz = 10000;
    
    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);
    
    ckobj_c(path.c_str(), &ids);
    throwOnSpiceError("Error finding Ck Converage");
    
    for (SpiceInt i = 0; i < card_c(&ids); ++i) {
        SpiceInt frame = SPICE_CELL_ELEM_I(&ids, i);
        
        scard_c(0, &cover);
        ckcov_c(path.c_str(), frame, SPICEFALSE, "SEGMENT", 0.0, "TDB", &cover);
        throwOnSpiceError("Error finding Ck Converage");
        
        //Get the number of intervals in the coverage window.
        SpiceInt numberOfIntervals = wncard_c(&cover);
        
        for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
            //Get the endpoints of the jth interval.
            SpiceDouble b, e;
            wnfetd_c(&cover, j, &b, &e);
            throwOnSpiceError("Error finding Ck Converage");
            
            _ckCoverageTimes[frame].insert(e);
            _ckCoverageTimes[frame].insert(b);
            _ckIntervals[frame].emplace_back(b, e);
        }
    }
}

void SpiceManager::findSpkCoverage(const std::string& path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(FileSys.fileExists(path), format("File '{}' does not exist", path));
    
    const unsigned int MaxObj = 64;
    const unsigned int WinSiz = 10000;
    
    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);
    
    spkobj_c(path.c_str(), &ids);
    throwOnSpiceError("Error finding Spk Converage");
    
    for (SpiceInt i = 0; i < card_c(&ids); ++i) {
        SpiceInt obj = SPICE_CELL_ELEM_I(&ids, i);
        
        scard_c(0, &cover);
        spkcov_c(path.c_str(), obj, &cover);
        throwOnSpiceError("Error finding Spk Converage");
        
        //Get the number of intervals in the coverage window.
        SpiceInt numberOfIntervals = wncard_c(&cover);
        
        for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
            //Get the endpoints of the jth interval.
            SpiceDouble b, e;
            wnfetd_c(&cover, j, &b, &e);
            throwOnSpiceError("Error finding Spk Converage");
            
            //insert all into coverage time set, the windows could be merged @AA
            _spkCoverageTimes[obj].insert(e);
            _spkCoverageTimes[obj].insert(b);
            _spkIntervals[obj].emplace_back(b, e);
        }		
    }
}
    
glm::dvec3 SpiceManager::getEstimatedPosition(const std::string& target,
                                              const std::string& observer,
                                              const std::string& referenceFrame,
                                              AberrationCorrection aberrationCorrection,
                                              double ephemerisTime,
                                              double& lightTime) const
{
    ghoul_assert(!target.empty(), "Target must not be empty");
    ghoul_assert(!observer.empty(), "Observer must not be empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame must not be empty");
    ghoul_assert(target != observer, "Target and observer must be different");
    
    int targetId = naifId(target);
    
    if (targetId == 0) {
        // SOLAR SYSTEM BARYCENTER special case, no definition in kernels
        return glm::dvec3(0.0);
    }
    
    if (_spkCoverageTimes.find(targetId) == _spkCoverageTimes.end()) {
        // no coverage
        throw SpiceException(format("No position for '{}' at any time", target));
    }
    
    
    int observerId = naifId(observer);

    std::set<double> coveredTimes = _spkCoverageTimes.find(targetId)->second;
    
    glm::dvec3 pos;
    if (coveredTimes.lower_bound(ephemerisTime) == coveredTimes.begin()) {
        // coverage later, fetch first position
        spkpos_c(
            target.c_str(),
            *(coveredTimes.begin()),
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(pos),
            &lightTime
        );
        throwOnSpiceError(format(
            "Error estimating position for target '{}' with observer '{}' in frame '{}'",
            target, observer, referenceFrame
        ));
    }
    else if (coveredTimes.upper_bound(ephemerisTime) == coveredTimes.end()) {
        // coverage earlier, fetch last position
        spkpos_c(
            target.c_str(),
            *(coveredTimes.rbegin()),
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(pos),
            &lightTime
        );
        throwOnSpiceError(format(
            "Error estimating position for target '{}' with observer '{}' in frame '{}'",
            target, observer, referenceFrame
        ));
    }
    else {
        // coverage both earlier and later, interpolate these positions
        glm::dvec3 posEarlier;
        double ltEarlier;
        double timeEarlier = *std::prev((coveredTimes.lower_bound(ephemerisTime)));
        spkpos_c(
            target.c_str(),
            timeEarlier,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(posEarlier),
            &ltEarlier
        );
        
        glm::dvec3 posLater;
        double ltLater;
        double timeLater = *(coveredTimes.upper_bound(ephemerisTime));
        spkpos_c(
            target.c_str(),
            timeLater,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(posLater),
            &ltLater
        );

        throwOnSpiceError(format(
            "Error estimating position for target '{}' with observer '{}' in frame '{}'",
            target, observer, referenceFrame
        ));

        // linear interpolation
        double t = (ephemerisTime - timeEarlier) / (timeLater - timeEarlier);
        pos = posEarlier * (1.0 - t) + posLater * t;
        lightTime = ltEarlier * (1.0 - t) + ltLater * t;
    }

    return pos;
}
    
glm::dmat3 SpiceManager::getEstimatedTransformMatrix(const std::string& fromFrame,
                                                     const std::string& toFrame,
                                                     double time) const
{
    glm::dmat3 result;
    int idFrame = frameId(fromFrame);
    
    if (_ckCoverageTimes.find(idFrame) == _ckCoverageTimes.end()) {
        // no coverage
        throw SpiceException(format(
            "No data available for the transform matrix from '{}' to '{}' at any time",
            fromFrame, toFrame
        ));
    }
    
    std::set<double> coveredTimes = _ckCoverageTimes.find(idFrame)->second;
    
    if (coveredTimes.lower_bound(time) == coveredTimes.begin()) {
        // coverage later, fetch first transform
        pxform_c(
            fromFrame.c_str(),
            toFrame.c_str(),
            *(coveredTimes.begin()),
            reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
        );
        throwOnSpiceError(format(
            "Error estimating transform matrix from frame '{}' to from '{}' at time '{}'",
            fromFrame, toFrame, time
        ));
        
    }
    else if (coveredTimes.upper_bound(time) == coveredTimes.end()) {
        // coverage earlier, fetch last transform
        pxform_c(
            fromFrame.c_str(),
            toFrame.c_str(),
            *(coveredTimes.rbegin()),
            reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
        );
        throwOnSpiceError(format(
            "Error estimating transform matrix from frame '{}' to from '{}' at time '{}'",
            fromFrame, toFrame, time
        ));
    }
    else {
        // coverage both earlier and later, interpolate these transformations
        double earlier = *std::prev((coveredTimes.lower_bound(time)));
        double later = *(coveredTimes.upper_bound(time));
        
        glm::dmat3 earlierTransform;
        pxform_c(
            fromFrame.c_str(),
            toFrame.c_str(),
            earlier,
            reinterpret_cast<double(*)[3]>(glm::value_ptr(earlierTransform))
        );
        throwOnSpiceError(format(
            "Error estimating transform matrix from frame '{}' to from '{}' at time '{}'",
            fromFrame, toFrame, time
        ));
        
        glm::dmat3 laterTransform;
        pxform_c(
            fromFrame.c_str(),
            toFrame.c_str(),
            later,
            reinterpret_cast<double(*)[3]>(glm::value_ptr(laterTransform))
        );
        throwOnSpiceError(format(
            "Error estimating transform matrix from frame '{}' to from '{}' at time '{}'",
            fromFrame, toFrame, time
        ));
        
        double t = (time - earlier) / (later - earlier);
        result = earlierTransform * (1.0 - t) + laterTransform * t;
    }
    
    return result;
}



} // namespace openspace
