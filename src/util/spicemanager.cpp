/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/scripting/lualibrary.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

namespace {
    constexpr const char* _loggerCat = "SpiceManager";

    // The value comes from
    // http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getmsg_c.html
    // as the maximum message length
    constexpr const unsigned SpiceErrorBufferSize = 1841;

    // This method checks if one of the previous SPICE methods has failed. If it has, an
    // exception with the SPICE error message is thrown
    // If an error occurred, true is returned, otherwise, false
    bool throwOnSpiceError(const std::string& errorMessage) {
        SpiceBoolean failed = failed_c();
        if (openspace::SpiceManager::ref().exceptionHandling()) {
            if (failed) {
                char buffer[SpiceErrorBufferSize];
                getmsg_c("LONG", SpiceErrorBufferSize, buffer);
                reset_c();
                throw openspace::SpiceManager::SpiceException(
                    errorMessage + ": " + buffer
                );
            }
            else {
                return false;
            }
        }
        else {
            return failed;
        }
    }

    const char* toString(openspace::SpiceManager::FieldOfViewMethod m) {
        using SM = openspace::SpiceManager;
        switch (m) {
            case SM::FieldOfViewMethod::Ellipsoid: return "ELLIPSOID";
            case SM::FieldOfViewMethod::Point:     return "POINT";
            default:                               throw ghoul::MissingCaseException();
        }
    }

    const char* toString(openspace::SpiceManager::TerminatorType t) {
        using SM = openspace::SpiceManager;
        switch (t) {
            case SM::TerminatorType::Umbral:    return "UMBRAL";
            case SM::TerminatorType::Penumbral: return "PENUMBRAL";
            default:                            throw ghoul::MissingCaseException();
        }
    }
}

#include "spicemanager_lua.inl"

namespace openspace {

SpiceManager::SpiceException::SpiceException(const std::string& msg)
    : ghoul::RuntimeError(msg, "Spice")
{
    ghoul_assert(
        SpiceManager::ref().exceptionHandling() == SpiceManager::UseException::Yes,
        "No exceptions should be thrown when UseException is No"
    );
}

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
    ghoul_assert(it != Mapping.end(), fmt::format("Invalid identifer '{}'", identifier));

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
        default:
            throw ghoul::MissingCaseException();
    }
}

SpiceManager::FieldOfViewMethod SpiceManager::fieldOfViewMethodFromString(
                                                                const std::string& method)
{
    const static std::map<std::string, FieldOfViewMethod> Mapping = {
        { "ELLIPSOID", FieldOfViewMethod::Ellipsoid },
        { "POINT", FieldOfViewMethod::Point }
    };

    ghoul_assert(!method.empty(), "Method must not be empty");

    return Mapping.at(method);
}

SpiceManager::TerminatorType SpiceManager::terminatorTypeFromString(
                                                                  const std::string& type)
{
    const static std::map<std::string, TerminatorType> Mapping = {
        { "UMBRAL", TerminatorType::Umbral },
        { "PENUMBRAL", TerminatorType::Penumbral }
    };

    ghoul_assert(!type.empty(), "Type must not be empty");

    return Mapping.at(type);
}

SpiceManager::SpiceManager() {
    // Set the SPICE library to not exit the program if an error occurs
    erract_c("SET", 0, const_cast<char*>("REPORT")); // NOLINT
    // But we do not want SPICE to print the errors, we will fetch them ourselves
    errprt_c("SET", 0, const_cast<char*>("NONE")); // NOLINT
}

SpiceManager::~SpiceManager() {
    for (const KernelInformation& i : _loadedKernels) {
        unload_c(i.path.c_str());
    }

    // Set values back to default
    erract_c("SET", 0, const_cast<char*>("DEFAULT")); // NOLINT
    errprt_c("SET", 0, const_cast<char*>("DEFAULT")); // NOLINT
}

SpiceManager::KernelHandle SpiceManager::loadKernel(std::string filePath) {
    ghoul_assert(!filePath.empty(), "Empty file path");
    ghoul_assert(
        FileSys.fileExists(filePath),
        fmt::format("File '{}' ('{}') does not exist", filePath, absPath(filePath))
    );
    ghoul_assert(
        FileSys.directoryExists(ghoul::filesystem::File(filePath).directoryName()),
        fmt::format(
            "File '{}' exists, but directory '{}' doesn't",
            absPath(filePath), ghoul::filesystem::File(filePath).directoryName()
        )
    );

    std::string path = absPath(std::move(filePath));
    const auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [path](const KernelInformation& info) { return info.path == path; }
    );

    if (it != _loadedKernels.end()) {
        it->refCount++;
        return it->id;
    }

    // We need to set the current directory as meta-kernels are usually defined relative
    // to the directory they reside in. The directory change is not necessary for regular
    // kernels
    ghoul::filesystem::Directory currentDirectory = FileSys.currentDirectory();
    using RawPath = ghoul::filesystem::File::RawPath;
    std::string fileDirectory = ghoul::filesystem::File(
        path,
        RawPath::Yes
    ).directoryName();
    FileSys.setCurrentDirectory(fileDirectory);

    LINFO(fmt::format("Loading SPICE kernel '{}'", path));
    // Load the kernel
    furnsh_c(path.c_str());

    // Reset the current directory to the previous one
    FileSys.setCurrentDirectory(currentDirectory);

    throwOnSpiceError("Kernel loading");

    std::string fileExtension = ghoul::filesystem::File(
        path,
        RawPath::Yes
    ).fileExtension();
    if (fileExtension == "bc" || fileExtension == "BC") {
        findCkCoverage(path); // binary ck kernel
    }
    else if (fileExtension == "bsp" || fileExtension == "BSP") {
        findSpkCoverage(path); // binary spk kernel
    }

    KernelHandle kernelId = ++_lastAssignedKernel;
    ghoul_assert(kernelId != 0, fmt::format("Kernel Handle wrapped around to 0"));
    _loadedKernels.push_back({std::move(path), kernelId, 1});
    return kernelId;
}

void SpiceManager::unloadKernel(KernelHandle kernelId) {
    ghoul_assert(kernelId <= _lastAssignedKernel, "Invalid unassigned kernel");
    ghoul_assert(kernelId != KernelHandle(0), "Invalid zero handle");

    const auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [&kernelId](const KernelInformation& info) { return info.id == kernelId; }
    );

    if (it != _loadedKernels.end()) {
        // If there was only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
            // No need to check for errors as we do not allow empty path names
            LINFO(fmt::format("Unloading SPICE kernel '{}'", it->path));
            unload_c(it->path.c_str());
            _loadedKernels.erase(it);
        }
        // Otherwise, we hold on to it, but reduce the reference counter by 1
        else {
            it->refCount--;
            LDEBUG(fmt::format("Reducing reference counter to: {}", it->refCount));
        }
    }
}

void SpiceManager::unloadKernel(std::string filePath) {
    ghoul_assert(!filePath.empty(), "Empty filename");

    std::string path = absPath(std::move(filePath));

    const auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [&path](const KernelInformation& info) { return info.path == path; }
    );

    if (it == _loadedKernels.end()) {
        if (_useExceptions) {
            throw SpiceException(
                fmt::format("'{}' did not correspond to a loaded kernel", path)
            );
        }
        else {
            return;
        }
    }
    else {
        // If there was only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
            LINFO(fmt::format("Unloading SPICE kernel '{}'", path));
            unload_c(path.c_str());
            _loadedKernels.erase(it);
        }
        else {
            // Otherwise, we hold on to it, but reduce the reference counter by 1
            it->refCount--;
            LDEBUG(fmt::format("Reducing reference counter to: {}", it->refCount));
        }
    }
}

bool SpiceManager::hasSpkCoverage(const std::string& target, double et) const {
    ghoul_assert(!target.empty(), "Empty target");

    const int id = naifId(target);
    const auto it = _spkIntervals.find(id);
    if (it != _spkIntervals.end()) {
        const std::vector<std::pair<double, double>>& intervalVector = it->second;
        for (const std::pair<double, double>& vecElement : intervalVector) {
            if ((vecElement.first < et) && (vecElement.second > et)) {
                return true;
            }
        }
    }
    return false;
}

bool SpiceManager::hasCkCoverage(const std::string& frame, double et) const {
    ghoul_assert(!frame.empty(), "Empty target");

    const int id = frameId(frame);
    const auto it = _ckIntervals.find(id);
    if (it != _ckIntervals.end()) {
        const std::vector<std::pair<double, double>>& intervalVector = it->second;
        for (const std::pair<double, double>& i : intervalVector) {
            if ((i.first < et) && (i.second > et)) {
                return true;
            }
        }
    }
    return false;
}

bool SpiceManager::hasValue(int naifId, const std::string& item) const {
    return bodfnd_c(naifId, item.c_str());
}

bool SpiceManager::hasValue(const std::string& body, const std::string& item) const {
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!item.empty(), "Empty item");

    int id = naifId(body);
    return hasValue(id, item);
}

int SpiceManager::naifId(const std::string& body) const {
    ghoul_assert(!body.empty(), "Empty body");

    SpiceBoolean success;
    SpiceInt id;
    bods2c_c(body.c_str(), &id, &success);
    if (!success && _useExceptions) {
        throw SpiceException(fmt::format("Could not find NAIF ID of body '{}'", body));
    }
    return id;
}

bool SpiceManager::hasNaifId(const std::string& body) const {
    ghoul_assert(!body.empty(), "Empty body");

    SpiceBoolean success;
    SpiceInt id;
    bods2c_c(body.c_str(), &id, &success);
    reset_c();
    return success;
}

int SpiceManager::frameId(const std::string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");

    SpiceInt id;
    namfrm_c(frame.c_str(), &id);
    if (id == 0 && _useExceptions) {
        throw SpiceException(fmt::format("Could not find NAIF ID of frame '{}'", frame));
    }
    return id;
}

bool SpiceManager::hasFrameId(const std::string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");

    SpiceInt id;
    namfrm_c(frame.c_str(), &id);
    return id != 0;
}

void getValueInternal(const std::string& body, const std::string& value, int size,
                      double* v)
{
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!value.empty(), "Empty value");
    ghoul_assert(v != nullptr, "Empty value pointer");

    SpiceInt n;
    bodvrd_c(body.c_str(), value.c_str(), size, &n, v);

    throwOnSpiceError(fmt::format("Error getting value '{}' for body '{}'", value, body));
}

void SpiceManager::getValue(const std::string& body, const std::string& value,
                            double& v) const
{
    getValueInternal(body, value, 1, &v);
}

void SpiceManager::getValue(const std::string& body, const std::string& value,
                            glm::dvec2& v) const
{
    getValueInternal(body, value, 2, glm::value_ptr(v));
}

void SpiceManager::getValue(const std::string& body, const std::string& value,
                            glm::dvec3& v) const
{
    getValueInternal(body, value, 3, glm::value_ptr(v));
}

void SpiceManager::getValue(const std::string& body, const std::string& value,
                            glm::dvec4& v) const
{
    getValueInternal(body, value, 4, glm::value_ptr(v));
}

void SpiceManager::getValue(const std::string& body, const std::string& value,
                            std::vector<double>& v) const
{
    ghoul_assert(!v.empty(), "Array for values has to be preallocaed");

    getValueInternal(body, value, static_cast<int>(v.size()), v.data());
}

double SpiceManager::spacecraftClockToET(const std::string& craft, double craftTicks) {
    ghoul_assert(!craft.empty(), "Empty craft");

    int craftId = naifId(craft);
    double et;
    sct2e_c(craftId, craftTicks, &et);
    throwOnSpiceError(fmt::format(
        "Error transforming spacecraft clock of '{}' at time {}", craft, craftTicks
    ));
    return et;
}

double SpiceManager::ephemerisTimeFromDate(const std::string& timeString) const {
    ghoul_assert(!timeString.empty(), "Empty timeString");

    double et;
    str2et_c(timeString.c_str(), &et);
    throwOnSpiceError(fmt::format("Error converting date '{}'", timeString));
    return et;
}

std::string SpiceManager::dateFromEphemerisTime(double ephemerisTime,
                                                    const std::string& formatString) const
{
    ghoul_assert(!formatString.empty(), "Format is empty");

    constexpr const int BufferSize = 256;
    SpiceChar buffer[BufferSize];
    timout_c(ephemerisTime, formatString.c_str(), BufferSize - 1, buffer);
    throwOnSpiceError(
        fmt::format("Error converting ephemeris time '{}' to date with format '{}'",
            ephemerisTime, formatString
        )
    );

    return std::string(buffer);
}

glm::dvec3 SpiceManager::targetPosition(const std::string& target,
                                        const std::string& observer,
                                        const std::string& referenceFrame,
                                        AberrationCorrection aberrationCorrection,
                                        double ephemerisTime, double& lightTime) const
{
    ghoul_assert(!target.empty(), "Target is not empty");
    ghoul_assert(!observer.empty(), "Observer is not empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame is not empty");

    bool targetHasCoverage = hasSpkCoverage(target, ephemerisTime);
    bool observerHasCoverage = hasSpkCoverage(observer, ephemerisTime);
    if (!targetHasCoverage && !observerHasCoverage) {
        if (_useExceptions) {
            throw SpiceException(
                fmt::format(
                    "Neither target '{}' nor observer '{}' has SPK coverage at time {}",
                    target, observer, ephemerisTime
                )
            );
        }
        else {
            return glm::dvec3();
        }
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
        throwOnSpiceError(fmt::format(
            "Error getting position from '{}' to '{}' in reference frame '{}' at time {}",
            target, observer, referenceFrame, ephemerisTime
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

glm::dvec3 SpiceManager::targetPosition(const std::string& target,
                                        const std::string& observer,
                                        const std::string& referenceFrame,
                                        AberrationCorrection aberrationCorrection,
                                        double ephemerisTime) const
{
    double unused = 0.0;
    return targetPosition(
        target,
        observer,
        referenceFrame,
        aberrationCorrection,
        ephemerisTime,
        unused
    );
}

glm::dmat3 SpiceManager::frameTransformationMatrix(const std::string& from,
                                                   const std::string& to,
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
        fmt::format("Error converting from frame '{}' to frame '{}' at time '{}'",
            from, to, ephemerisTime
        )
    );

    // The rox-major, column-major order are switched in GLM and SPICE, so we have to
    // transpose the matrix before we can return it
    return glm::transpose(transform);
}

SpiceManager::SurfaceInterceptResult SpiceManager::surfaceIntercept(
                                                                const std::string& target,
                                                              const std::string& observer,
                                                              const std::string& fovFrame,
                                                        const std::string& referenceFrame,
                                                AberrationCorrection aberrationCorrection,
                                                                     double ephemerisTime,
                                                  const glm::dvec3& directionVector) const
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

    throwOnSpiceError(fmt::format(
        "Error retrieving surface intercept on target '{}' viewed from observer '{}' in "
        "reference frame '{}' at time '{}'",
        target, observer, referenceFrame, ephemerisTime
    ));

    return result;
}

bool SpiceManager::isTargetInFieldOfView(const std::string& target,
                                         const std::string& observer,
                                         const std::string& referenceFrame,
                                         const std::string& instrument,
                                         FieldOfViewMethod method,
                                         AberrationCorrection aberrationCorrection,
                                         double& ephemerisTime) const
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

    throwOnSpiceError(fmt::format(
        "Checking if target '{}' is in view of instrument '{}' failed",
        target, instrument
    ));

    return visible == SPICETRUE;
}

bool SpiceManager::isTargetInFieldOfView(const std::string& target,
                                         const std::string& observer,
                                         const std::string& instrument,
                                         FieldOfViewMethod method,
                                         AberrationCorrection aberrationCorrection,
                                         double& ephemerisTime) const
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

SpiceManager::TargetStateResult SpiceManager::targetState(const std::string& target,
                                                          const std::string& observer,
                                                        const std::string& referenceFrame,
                                                AberrationCorrection aberrationCorrection,
                                                               double ephemerisTime) const
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

    throwOnSpiceError(fmt::format(
        "Error retrieving state of target '{}' viewed from observer '{}' in reference "
        "frame '{}' at time '{}'",
        target, observer, referenceFrame, ephemerisTime
    ));

    memmove(glm::value_ptr(result.position), buffer, sizeof(double) * 3);
    memmove(glm::value_ptr(result.velocity), buffer + 3, sizeof(double) * 3);
    return result;
}

SpiceManager::TransformMatrix SpiceManager::stateTransformMatrix(
                                                           const std::string& sourceFrame,
                                                      const std::string& destinationFrame,
                                                               double ephemerisTime) const
{
    ghoul_assert(!sourceFrame.empty(), "sourceFrame must not be empty");
    ghoul_assert(!destinationFrame.empty(), "toFrame must not be empty");

    TransformMatrix m;
    sxform_c(
        sourceFrame.c_str(),
        destinationFrame.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[6]>(m.data())
    );
    throwOnSpiceError(fmt::format(
        "Error retrieved state transform matrix from frame '{}' to frame '{}' at time "
        "'{}'",
        sourceFrame, destinationFrame, ephemerisTime
    ));
    return m;
}

glm::dmat3 SpiceManager::positionTransformMatrix(const std::string& sourceFrame,
                                                 const std::string& destinationFrame,
                                                 double ephemerisTime) const
{
    ghoul_assert(!sourceFrame.empty(), "sourceFrame must not be empty");
    ghoul_assert(!destinationFrame.empty(), "destinationFrame must not be empty");

    glm::dmat3 result;
    pxform_c(
        sourceFrame.c_str(),
        destinationFrame.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
    );

    throwOnSpiceError("");
    SpiceBoolean success = !(failed_c());
    reset_c();
    if (!success) {
        result = getEstimatedTransformMatrix(
            sourceFrame,
            destinationFrame,
            ephemerisTime
        );
    }

    return glm::transpose(result);
}

glm::dmat3 SpiceManager::positionTransformMatrix(const std::string& sourceFrame,
                                                 const std::string& destinationFrame,
                                                 double ephemerisTimeFrom,
                                                 double ephemerisTimeTo) const
{
    ghoul_assert(!sourceFrame.empty(), "sourceFrame must not be empty");
    ghoul_assert(!destinationFrame.empty(), "destinationFrame must not be empty");

    glm::dmat3 result;

    pxfrm2_c(
        sourceFrame.c_str(),
        destinationFrame.c_str(),
        ephemerisTimeFrom,
        ephemerisTimeTo,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
    );
    throwOnSpiceError(fmt::format(
        "Error retrieving position transform matrix from '{}' at time '{}' to frame '{}' "
        "at time '{}'",
        sourceFrame, ephemerisTimeFrom, destinationFrame, ephemerisTimeTo
    ));
    return glm::transpose(result);
}

SpiceManager::FieldOfViewResult
SpiceManager::fieldOfView(const std::string& instrument) const
{
    ghoul_assert(!instrument.empty(), "Instrument must not be empty");
    return fieldOfView(naifId(instrument));
}

SpiceManager::FieldOfViewResult SpiceManager::fieldOfView(int instrument) const {
    constexpr int MaxBoundsSize = 64;
    constexpr int BufferSize = 128;

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

    bool failed = throwOnSpiceError(fmt::format(
        "Error getting field-of-view parameters for instrument '{}'", instrument
    ));
    if (failed) {
        return res;
    }

    res.bounds.reserve(nrReturned);
    for (int i = 0; i < nrReturned; ++i) {
        res.bounds.emplace_back(boundsArr[i][0], boundsArr[i][1], boundsArr[i][2]);
    }

    std::string shape = std::string(fovShapeBuffer);
    static const std::map<std::string, FieldOfViewResult::Shape> Map = {
        { "POLYGON", FieldOfViewResult::Shape::Polygon },
        { "RECTANGLE" , FieldOfViewResult::Shape::Rectangle },
        { "CIRCLE", FieldOfViewResult::Shape::Circle },
        { "ELLIPSE", FieldOfViewResult::Shape::Ellipse }
    };
    res.shape = Map.at(shape);
    res.frameName = std::string(frameNameBuffer);

    return res;
}

SpiceManager::TerminatorEllipseResult SpiceManager::terminatorEllipse(
                                                                const std::string& target,
                                                              const std::string& observer,
                                                                 const std::string& frame,
                                                           const std::string& lightSource,
                                                            TerminatorType terminatorType,
                                                AberrationCorrection aberrationCorrection,
                                                                     double ephemerisTime,
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

    edterm_c(
        toString(terminatorType),
        lightSource.c_str(),
        target.c_str(),
        ephemerisTime,
        frame.c_str(),
        aberrationCorrection,
        observer.c_str(),
        numberOfTerminatorPoints,
        &res.targetEphemerisTime,
        glm::value_ptr(res.observerPosition),
        reinterpret_cast<double(*)[3]>(res.terminatorPoints.data())
    );
    throwOnSpiceError(fmt::format(
        "Error getting terminator ellipse for target '{}' from observer '{}' in frame "
        "'{}' with light source '{}' at time '{}'",
        target, observer, frame, lightSource, ephemerisTime
    ));
    return res;
}

bool SpiceManager::addFrame(std::string body, std::string frame) {
    if (body.empty() || frame.empty()) {
        return false;
    }
    else {
        _frameByBody.emplace_back(body, frame);
        return true;
    }
}

std::string SpiceManager::frameFromBody(const std::string& body) const {
    for (const std::pair<std::string, std::string>& pair : _frameByBody) {
        if (pair.first == body) {
            return pair.second;
        }
    }

    constexpr const char* unionPrefix = "IAU_";

    if (body.find(unionPrefix) == std::string::npos) {
        return unionPrefix + body;
    }
    else {
        return body;
    }
}

void SpiceManager::findCkCoverage(const std::string& path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(FileSys.fileExists(path), fmt::format("File '{}' does not exist", path));

    constexpr unsigned int MaxObj = 256;
    constexpr unsigned int WinSiz = 10000;

#if defined __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#elif defined __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);

    ckobj_c(path.c_str(), &ids);
    throwOnSpiceError("Error finding Ck Coverage");

    for (SpiceInt i = 0; i < card_c(&ids); ++i) {
        const SpiceInt frame = SPICE_CELL_ELEM_I(&ids, i); // NOLINT

#if defined __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif

        scard_c(0, &cover);
        ckcov_c(path.c_str(), frame, SPICEFALSE, "SEGMENT", 0.0, "TDB", &cover);
        throwOnSpiceError("Error finding Ck Coverage");

        // Get the number of intervals in the coverage window.
        const SpiceInt numberOfIntervals = wncard_c(&cover);

        for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
            // Get the endpoints of the jth interval.
            SpiceDouble b, e;
            wnfetd_c(&cover, j, &b, &e);
            throwOnSpiceError("Error finding Ck Coverage");

            _ckCoverageTimes[frame].insert(e);
            _ckCoverageTimes[frame].insert(b);
            _ckIntervals[frame].emplace_back(b, e);
        }
    }
}

void SpiceManager::findSpkCoverage(const std::string& path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(FileSys.fileExists(path), fmt::format("File '{}' does not exist", path));

    constexpr unsigned int MaxObj = 256;
    constexpr unsigned int WinSiz = 10000;

#if defined __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#elif defined __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);

    spkobj_c(path.c_str(), &ids);
    throwOnSpiceError("Error finding Spk ID for coverage");

    for (SpiceInt i = 0; i < card_c(&ids); ++i) {
        const SpiceInt obj = SPICE_CELL_ELEM_I(&ids, i); // NOLINT

#if defined __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif

        scard_c(0, &cover);
        spkcov_c(path.c_str(), obj, &cover);
        throwOnSpiceError("Error finding Spk coverage");

        // Get the number of intervals in the coverage window.
        const SpiceInt numberOfIntervals = wncard_c(&cover);

        for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
            //Get the endpoints of the jth interval.
            SpiceDouble b, e;
            wnfetd_c(&cover, j, &b, &e);
            throwOnSpiceError("Error finding Spk coverage");

            // insert all into coverage time set, the windows could be merged @AA
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
        if (_useExceptions) {
            // no coverage
            throw SpiceException(fmt::format("No position for '{}' at any time", target));
        }
        else {
            return glm::dvec3();
        }
    }

    const std::set<double>& coveredTimes = _spkCoverageTimes.find(targetId)->second;

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
        throwOnSpiceError(fmt::format(
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
        throwOnSpiceError(fmt::format(
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

        throwOnSpiceError(fmt::format(
            "Error estimating position for target '{}' with observer '{}' in frame '{}'",
            target, observer, referenceFrame
        ));

        // linear interpolation
        const double t = (ephemerisTime - timeEarlier) / (timeLater - timeEarlier);
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
    const int idFrame = frameId(fromFrame);

    if (_ckCoverageTimes.find(idFrame) == _ckCoverageTimes.end()) {
        if (_useExceptions) {
            // no coverage
            throw SpiceException(fmt::format(
                "No data available for transform matrix from '{}' to '{}' at any time",
                fromFrame, toFrame
            ));
        }
        else {
            return glm::dmat3();
        }
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
        throwOnSpiceError(fmt::format(
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
        throwOnSpiceError(fmt::format(
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
        throwOnSpiceError(fmt::format(
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
        throwOnSpiceError(fmt::format(
            "Error estimating transform matrix from frame '{}' to from '{}' at time '{}'",
            fromFrame, toFrame, time
        ));

        const double t = (time - earlier) / (later - earlier);
        result = earlierTransform * (1.0 - t) + laterTransform * t;
    }

    return result;
}

void SpiceManager::setExceptionHandling(UseException useException) {
    _useExceptions = useException;
}

SpiceManager::UseException SpiceManager::exceptionHandling() const {
    return _useExceptions;
}

scripting::LuaLibrary SpiceManager::luaLibrary() {
    return {
        "spice",
        {
            {
                "loadKernel",
                &luascriptfunctions::loadKernel,
                {},
                "string",
                "Loads the provided SPICE kernel by name. The name can contain path "
                "tokens, which are automatically resolved"
            },
            {
                "unloadKernel",
                &luascriptfunctions::unloadKernel,
                {},
                "{string, number}",
                "Unloads the provided SPICE kernel. The name can contain path tokens, "
                "which are automatically resolved"
            }
        }
    };
}

} // namespace openspace
