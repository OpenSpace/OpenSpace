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

#include <openspace/util/spicemanager.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <algorithm>
#include <filesystem>
#include <format>
#include "SpiceUsr.h"
#include "SpiceZpr.h"

namespace {
    constexpr std::string_view _loggerCat = "SpiceManager";

    // The value comes from
    // http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getmsg_c.html
    // as the maximum message length
    constexpr unsigned SpiceErrorBufferSize = 1841;

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
} // namespace

#include "spicemanager_lua.inl"

namespace openspace {

SpiceManager* SpiceManager::_instance = nullptr;

SpiceManager::SpiceException::SpiceException(std::string msg)
    : ghoul::RuntimeError(std::move(msg), "Spice")
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
    ghoul_assert(it != Mapping.end(), std::format("Invalid identifer '{}'", identifier));

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
    // The third parameter for the erract_c function is a SpiceChar*, not ConstSpiceChar*
    // so we have to do this weird memory copying trick
    std::array<char, 7> buffer;

    // Set the SPICE library to not exit the program if an error occurs
    std::memset(buffer.data(), 0, buffer.size());
    std::strcpy(buffer.data(), "REPORT");
    erract_c("SET", 0, buffer.data());

    // But we do not want SPICE to print the errors, we will fetch them ourselves
    std::memset(buffer.data(), 0, buffer.size());
    std::strcpy(buffer.data(), "NONE");
    errprt_c("SET", 0, buffer.data());

    loadLeapSecondsSpiceKernel();
    loadGeophysicalConstantsKernel();
}

SpiceManager::~SpiceManager() {
    for (const KernelInformation& i : _loadedKernels) {
        const std::string p = i.path.string();
        unload_c(p.c_str());
    }

    // Set values back to default
    erract_c("SET", 0, const_cast<char*>("DEFAULT"));
    errprt_c("SET", 0, const_cast<char*>("DEFAULT"));
}

void SpiceManager::initialize() {
    ghoul_assert(!isInitialized(), "SpiceManager is already initialized");
    _instance = new SpiceManager;
}

void SpiceManager::deinitialize() {
    ghoul_assert(isInitialized(), "SpiceManager is not initialized");
    delete _instance;
    _instance = nullptr;
}

bool SpiceManager::isInitialized() {
    return _instance != nullptr;
}

SpiceManager& SpiceManager::ref() {
    ghoul_assert(isInitialized(), "SpiceManager is not initialized");
    return *_instance;
}

// This method checks if one of the previous SPICE methods has failed. If it has, an
// exception with the SPICE error message is thrown
// If an error occurred, true is returned, otherwise, false
void throwSpiceError(const std::string& errorMessage) {
    if (openspace::SpiceManager::ref().exceptionHandling()) {
        std::string buffer;
        buffer.resize(SpiceErrorBufferSize);
        getmsg_c("LONG", SpiceErrorBufferSize, buffer.data());
        reset_c();
        throw openspace::SpiceManager::SpiceException(errorMessage + ": " + buffer);
    }
    else {
        reset_c();
    }
}

SpiceManager::KernelHandle SpiceManager::loadKernel(std::filesystem::path filePath) {
    ghoul_assert(!filePath.empty(), "Empty file path");
    ghoul_assert(
        std::filesystem::is_regular_file(filePath),
        std::format("File '{}' ({}) does not exist", filePath, absPath(filePath))
    );
    ghoul_assert(
        std::filesystem::is_directory(std::filesystem::path(filePath).parent_path()),
        std::format(
            "File '{}' exists, but directory '{}' does not",
            absPath(filePath), std::filesystem::path(filePath).parent_path()
        )
    );

    const auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [filePath](const KernelInformation& info) { return info.path == filePath; }
    );

    if (it != _loadedKernels.end()) {
        it->refCount++;
        return it->id;
    }

    // We need to set the current directory as meta-kernels are usually defined relative
    // to the directory they reside in. The directory change is not necessary for regular
    // kernels
    const std::filesystem::path currentDirectory = std::filesystem::current_path();

    const std::filesystem::path p = filePath.parent_path();
    std::filesystem::current_path(p);

    LINFO(std::format("Loading SPICE kernel '{}'", filePath));
    // Load the kernel
    const std::string k = filePath.string();
    furnsh_c(k.c_str());

    // Reset the current directory to the previous one
    std::filesystem::current_path(currentDirectory);

    if (failed_c()) {
        throwSpiceError("Kernel loading");
    }

    const std::filesystem::path fileExtension = filePath.extension();
    if (fileExtension == ".bc" ||
        fileExtension == ".BC" ||
        fileExtension == ".ck" ||
        fileExtension == ".CK")
    {
        findCkCoverage(filePath); // binary ck kernel
    }
    else if (fileExtension == ".bsp" ||
            fileExtension == ".BSP" ||
            fileExtension == ".spk" ||
            fileExtension == ".SPK")
        {
            findSpkCoverage(filePath); // spk kernel
    }

    const KernelHandle kernelId = ++_lastAssignedKernel;
    ghoul_assert(kernelId != 0, "Kernel Handle wrapped around to 0");
    _loadedKernels.push_back({ std::move(filePath), kernelId, 1 });
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
            LINFO(std::format("Unloading SPICE kernel '{}'", it->path));
            const std::string p = it->path.string();
            unload_c(p.c_str());
            _loadedKernels.erase(it);
        }
        // Otherwise, we hold on to it, but reduce the reference counter by 1
        else {
            it->refCount--;
            LDEBUG(std::format("Reducing reference counter to: {}", it->refCount));
        }
    }
}

void SpiceManager::unloadKernel(std::filesystem::path filePath) {
    ghoul_assert(!filePath.empty(), "Empty filename");

    const auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [&filePath](const KernelInformation& info) { return info.path == filePath; }
    );

    if (it == _loadedKernels.end()) {
        if (_useExceptions) {
            throw SpiceException(
                std::format("'{}' did not correspond to a loaded kernel", filePath)
            );
        }
        else {
            return;
        }
    }
    else {
        // If there was only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
            LINFO(std::format("Unloading SPICE kernel '{}'", filePath));
            const std::string p = filePath.string();
            unload_c(p.c_str());
            _loadedKernels.erase(it);
        }
        else {
            // Otherwise, we hold on to it, but reduce the reference counter by 1
            it->refCount--;
            LDEBUG(std::format("Reducing reference counter to: {}", it->refCount));
        }
    }
}

std::vector<std::filesystem::path> SpiceManager::loadedKernels() const {
    std::vector<std::filesystem::path> res;
    res.reserve(_loadedKernels.size());
    for (const KernelInformation& info : _loadedKernels) {
        res.push_back(info.path);
    }
    return res;
}

bool SpiceManager::hasSpkCoverage(const std::string& target, double et) const {
    ghoul_assert(!target.empty(), "Empty target");

    const int id = naifId(target);
    // SOLAR SYSTEM BARYCENTER special case, implicitly included by Spice
    if (id == 0) {
        return true;
    }

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

std::vector<std::pair<double, double>> SpiceManager::spkCoverage(
                                                          const std::string& target) const
{
    ghoul_assert(!target.empty(), "Empty target");

    const int id = naifId(target);
    const auto it = _spkIntervals.find(id);
    if (it != _spkIntervals.end()) {
        return it->second;
    }
    else {
        std::vector<std::pair<double, double>> emptyList;
        return emptyList;
    }
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

std::vector<std::pair<double, double>> SpiceManager::ckCoverage(
                                                          const std::string& target) const
{
    ghoul_assert(!target.empty(), "Empty target");

    int id = naifId(target);
    const auto it = _ckIntervals.find(id);
    if (it != _ckIntervals.end()) {
        return it->second;
    }
    else {
        id *= 1000;
        const auto it2 = _ckIntervals.find(id);
        if (it2 != _ckIntervals.end()) {
            return it2->second;
        }
        else {
            std::vector<std::pair<double, double>> emptyList;
            return emptyList;
        }
    }
}

std::vector<std::pair<int, std::string>> SpiceManager::spiceBodies(
                                                                 bool builtInFrames) const
{
    std::vector<std::pair<int, std::string>> bodies;

    static std::array<SpiceInt, SPICE_CELL_CTRLSZ + 8192> idsetBuffer;
    static SpiceCell idset = {
        SPICE_INT,
        0,
        8192,
        0,
        SPICETRUE,
        SPICEFALSE,
        SPICEFALSE,
        idsetBuffer.data(),
        idsetBuffer.data() + SPICE_CELL_CTRLSZ
    };

    constexpr int Frnmln = 33;
    std::array<SpiceChar, Frnmln> frname;

    for (SpiceInt i = 1; i <= 6; i++) {
        if (i < 6) {
            if (builtInFrames) {
                bltfrm_c(i, &idset);
            }
            else {
                kplfrm_c(i, &idset);
            }
        }
        else {
            if (builtInFrames) {
                bltfrm_c(SPICE_FRMTYP_ALL, &idset);
            }
            else {
                kplfrm_c(SPICE_FRMTYP_ALL, &idset);
            }
        }

        for (SpiceInt j = 0; j < card_c(&idset); j++) {
            frmnam_c(
                (reinterpret_cast<SpiceInt*>(idset.data))[j],
                Frnmln,
                frname.data()
            );
            bodies.emplace_back(
                static_cast<long>(reinterpret_cast<SpiceInt*>(idset.data)[j]),
                frname.data()
            );
        }
    }
    return bodies;
}

bool SpiceManager::hasValue(int naifId, const std::string& item) const {
    return bodfnd_c(naifId, item.c_str());
}

bool SpiceManager::hasValue(const std::string& body, const std::string& item) const {
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!item.empty(), "Empty item");

    const int id = naifId(body);
    return hasValue(id, item);
}

int SpiceManager::naifId(const std::string& body) const {
    ghoul_assert(!body.empty(), "Empty body");

    SpiceBoolean success = SPICEFALSE;
    SpiceInt id = 0;
    bods2c_c(body.c_str(), &id, &success);
    if (!success && _useExceptions) {
        throw SpiceException(std::format("Could not find NAIF ID of body '{}'", body));
    }
    return id;
}

bool SpiceManager::hasNaifId(const std::string& body) const {
    ghoul_assert(!body.empty(), "Empty body");

    SpiceBoolean success = SPICEFALSE;
    SpiceInt id = 0;
    bods2c_c(body.c_str(), &id, &success);
    reset_c();
    return success;
}

int SpiceManager::frameId(const std::string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");

    SpiceInt id = 0;
    namfrm_c(frame.c_str(), &id);
    if (id == 0 && _useExceptions) {
        throw SpiceException(std::format("Could not find NAIF ID of frame '{}'", frame));
    }
    return id;
}

bool SpiceManager::hasFrameId(const std::string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");

    SpiceInt id = 0;
    namfrm_c(frame.c_str(), &id);
    return id != 0;
}

void getValueInternal(const std::string& body, const std::string& value, int size,
                      double* v)
{
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!value.empty(), "Empty value");
    ghoul_assert(v != nullptr, "Empty value pointer");

    SpiceInt n = 0;
    bodvrd_c(body.c_str(), value.c_str(), size, &n, v);

    if (failed_c()) {
        throwSpiceError(
            std::format("Error getting value '{}' for body '{}'", value, body)
        );
    }
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

double SpiceManager::spacecraftClockToET(const std::string& craft,
                                         double craftTicks) const
{
    ghoul_assert(!craft.empty(), "Empty craft");

    const int craftId = naifId(craft);
    double et = 0.0;
    sct2e_c(craftId, craftTicks, &et);
    if (failed_c()) {
        throwSpiceError(std::format(
            "Error transforming spacecraft clock of '{}' at time {}", craft, craftTicks
        ));
    }
    return et;
}

double SpiceManager::ephemerisTimeFromDate(const std::string& timeString) const {
    ghoul_assert(!timeString.empty(), "Empty timeString");

    return ephemerisTimeFromDate(timeString.c_str());
}

double SpiceManager::ephemerisTimeFromDate(const char* timeString) const {
    double et = 0.0;
    str2et_c(timeString, &et);
    if (failed_c()) {
        throwSpiceError(std::format("Error converting date '{}'", timeString));
    }
    return et;
}

std::string SpiceManager::dateFromEphemerisTime(double ephemerisTime, const char* format)
{
    constexpr int BufferSize = 128;
    std::array<char, BufferSize> Buffer;
    std::memset(Buffer.data(), char(0), BufferSize);

    timout_c(ephemerisTime, format, BufferSize, Buffer.data());
    if (failed_c()) {
        throwSpiceError(std::format(
            "Error converting ephemeris time '{}' to date with format '{}'",
            ephemerisTime, format
        ));
    }
    if (Buffer[0] == '*') {
        // The conversion failed and we need to use et2utc
        constexpr int SecondsPrecision = 3;
        et2utc_c(ephemerisTime, "C", SecondsPrecision, BufferSize, Buffer.data());
    }


    return std::string(Buffer.data());
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

    const bool targetHasCoverage = hasSpkCoverage(target, ephemerisTime);
    const bool observerHasCoverage = hasSpkCoverage(observer, ephemerisTime);
    if (!targetHasCoverage && !observerHasCoverage) {
        if (_useExceptions) {
            throw SpiceException(
                std::format(
                    "Neither target '{}' nor observer '{}' has SPK coverage at time '{}'",
                    target, observer, ephemerisTime
                )
            );
        }
        else {
            return glm::dvec3(0.0);
        }
    }
    else if (targetHasCoverage && observerHasCoverage) {
        glm::dvec3 position = glm::dvec3(0.0);
        spkpos_c(
            target.c_str(),
            ephemerisTime,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(position),
            &lightTime
        );
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error getting position from '{}' to '{}' in frame '{}' at time '{}'",
                target, observer, referenceFrame, ephemerisTime
            ));
        }
        return position;
    }
    else if (targetHasCoverage) {
        // observer has no coverage, so we try getting position from the reverse
        const std::string& invObserver = target;
        const std::string& invTarget = observer;
        return getEstimatedPosition(
            invTarget,
            invObserver,
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
    glm::dmat3 transform = glm::dmat3(1.0);
    pxform_c(
        from.c_str(),
        to.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(transform))
    );

    if (failed_c()) {
        throwSpiceError(
            std::format("Error converting from frame '{}' to frame '{}' at time '{}'",
                from, to, ephemerisTime
            )
        );
    }

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

    SpiceBoolean found = SPICEFALSE;
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

    if (failed_c()) {
        throwSpiceError(std::format(
            "Error retrieving surface intercept on target '{}' viewed from observer '{}' "
            "in reference frame '{}' at time '{}'",
            target, observer, referenceFrame, ephemerisTime
        ));
    }

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

    int visible = 0;
    fovtrg_c(
        instrument.c_str(),
        target.c_str(),
        toString(method),
        referenceFrame.c_str(),
        aberrationCorrection,
        observer.c_str(),
        &ephemerisTime,
        &visible
    );

    if (failed_c()) {
        throwSpiceError(std::format(
            "Checking if target '{}' is in view of instrument '{}' failed",
            target, instrument
        ));
    }

    return visible == SPICETRUE;
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

    if (failed_c()) {
        throwSpiceError(std::format(
            "Error retrieving state of target '{}' viewed from observer '{}' in "
            "reference frame '{}' at time '{}'",
            target, observer, referenceFrame, ephemerisTime
        ));
    }

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
    if (failed_c()) {
        throwSpiceError(std::format(
            "Error retrieved state transform matrix from frame '{}' to frame '{}' at "
            "time '{}'",
            sourceFrame, destinationFrame, ephemerisTime
        ));
    }
    return m;
}

glm::dmat3 SpiceManager::positionTransformMatrix(const std::string& sourceFrame,
                                                 const std::string& destinationFrame,
                                                 double ephemerisTime) const
{
    ghoul_assert(!sourceFrame.empty(), "sourceFrame must not be empty");
    ghoul_assert(!destinationFrame.empty(), "destinationFrame must not be empty");

    glm::dmat3 result = glm::dmat3(1.0);
    pxform_c(
        sourceFrame.c_str(),
        destinationFrame.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
    );

    if (failed_c()) {
        throwSpiceError("");
    }
    const SpiceBoolean success = !(failed_c());
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

    glm::dmat3 result = glm::dmat3(1.0);

    pxfrm2_c(
        sourceFrame.c_str(),
        destinationFrame.c_str(),
        ephemerisTimeFrom,
        ephemerisTimeTo,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
    );
    if (failed_c()) {
        throwSpiceError(std::format(
            "Error retrieving position transform matrix from '{}' at time '{}' to frame "
            "'{}' at time '{}'",
            sourceFrame, ephemerisTimeFrom, destinationFrame, ephemerisTimeTo
        ));
    }
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

    SpiceInt nrReturned = 0;
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

    if (failed_c()) {
        throwSpiceError(std::format(
            "Error getting field-of-view parameters for instrument '{}'", instrument
        ));
        return res;
    }

    res.bounds.reserve(nrReturned);
    for (int i = 0; i < nrReturned; i++) {
        res.bounds.emplace_back(boundsArr[i][0], boundsArr[i][1], boundsArr[i][2]);
    }

    const std::string shape = std::string(fovShapeBuffer);
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
    if (failed_c()) {
        throwSpiceError(std::format(
            "Error getting terminator ellipse for target '{}' from observer '{}' in "
            "frame '{}' with light source '{}' at time '{}'",
            target, observer, frame, lightSource, ephemerisTime
        ));
    }
    return res;
}

void SpiceManager::findCkCoverage(const std::filesystem::path& path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(
        std::filesystem::is_regular_file(path),
        std::format("File '{}' does not exist", path)
    );

    constexpr unsigned int MaxObj = 1024;
    constexpr unsigned int WinSiz = 16384;

#if defined __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#elif defined __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);

    const std::string p = path.string();
    ckobj_c(p.c_str(), &ids);
    if (failed_c()) {
        throwSpiceError("Error finding Ck Coverage");
    }

    for (SpiceInt i = 0; i < card_c(&ids); i++) {
        const SpiceInt frame = SPICE_CELL_ELEM_I(&ids, i);

#if defined __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif

        scard_c(0, &cover);
        ckcov_c(p.c_str(), frame, SPICEFALSE, "SEGMENT", 0.0, "TDB", &cover);
        if (failed_c()) {
            throwSpiceError("Error finding Ck Coverage");
        }

        // Get the number of intervals in the coverage window.
        const SpiceInt numberOfIntervals = wncard_c(&cover);

        for (SpiceInt j = 0; j < numberOfIntervals; j++) {
            // Get the endpoints of the jth interval.
            SpiceDouble b = 0.0;
            SpiceDouble e = 0.0;
            wnfetd_c(&cover, j, &b, &e);
            if (failed_c()) {
                throwSpiceError("Error finding Ck Coverage");
            }

            _ckCoverageTimes[frame].insert(e);
            _ckCoverageTimes[frame].insert(b);
            _ckIntervals[frame].emplace_back(b, e);
        }
    }
}

void SpiceManager::findSpkCoverage(const std::filesystem::path &path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(
        std::filesystem::is_regular_file(path),
        std::format("File '{}' does not exist", path)
    );

    constexpr unsigned int MaxObj = 1024;
    constexpr unsigned int WinSiz = 16384;

#if defined __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#elif defined __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);

    const std::string p = path.string();
    spkobj_c(p.c_str(), &ids);
    if (failed_c()) {
        throwSpiceError("Error finding Spk ID for coverage");
    }

    for (SpiceInt i = 0; i < card_c(&ids); i++) {
        const SpiceInt obj = SPICE_CELL_ELEM_I(&ids, i);

#if defined __clang__
#pragma clang diagnostic pop
#elif defined __GNUC__
#pragma GCC diagnostic pop
#endif

        scard_c(0, &cover);
        spkcov_c(p.c_str(), obj, &cover);
        if (failed_c()) {
            throwSpiceError("Error finding Spk coverage");
        }

        // Get the number of intervals in the coverage window.
        const SpiceInt numberOfIntervals = wncard_c(&cover);

        for (SpiceInt j = 0; j < numberOfIntervals; j++) {
            //Get the endpoints of the jth interval.
            SpiceDouble b = 0.0;
            SpiceDouble e = 0.0;
            wnfetd_c(&cover, j, &b, &e);
            if (failed_c()) {
                throwSpiceError("Error finding Spk coverage");
            }

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
    ZoneScoped;

    ghoul_assert(!target.empty(), "Target must not be empty");
    ghoul_assert(!observer.empty(), "Observer must not be empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame must not be empty");
    ghoul_assert(target != observer, "Target and observer must be different");

    const int targetId = naifId(target);

    if (targetId == 0) {
        // SOLAR SYSTEM BARYCENTER special case, no definition in kernels
        return glm::dvec3(0.0);
    }

    if (_spkCoverageTimes.find(targetId) == _spkCoverageTimes.end()) {
        if (_useExceptions) {
            // no coverage
            throw SpiceException(std::format("No position for '{}' at any time", target));
        }
        else {
            return glm::dvec3(0.0);
        }
    }

    const std::set<double>& coveredTimes = _spkCoverageTimes.find(targetId)->second;

    glm::dvec3 pos = glm::dvec3(0.0);
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
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error estimating position for '{}' with observer '{}' in frame '{}'",
                target, observer, referenceFrame
            ));
        }

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
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error estimating position for '{}' with observer '{}' in frame '{}'",
                target, observer, referenceFrame
            ));
        }
    }
    else {
        // coverage both earlier and later, interpolate these positions
        glm::dvec3 posEarlier = glm::dvec3(0.0);
        double ltEarlier = 0.0;
        const double timeEarlier = *std::prev((coveredTimes.lower_bound(ephemerisTime)));
        spkpos_c(
            target.c_str(),
            timeEarlier,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(posEarlier),
            &ltEarlier
        );

        glm::dvec3 posLater = glm::dvec3(0.0);
        double ltLater = 0.0;
        const double timeLater = *(coveredTimes.upper_bound(ephemerisTime));
        spkpos_c(
            target.c_str(),
            timeLater,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(posLater),
            &ltLater
        );

        if (failed_c()) {
            throwSpiceError(std::format(
                "Error estimating position for '{}' with observer '{}' in frame '{}'",
                target, observer, referenceFrame
            ));
        }

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
    glm::dmat3 result = glm::dmat3(1.0);
    const int idFrame = frameId(fromFrame);

    if (_ckCoverageTimes.find(idFrame) == _ckCoverageTimes.end()) {
        if (_useExceptions) {
            // no coverage
            throw SpiceException(std::format(
                "No data available for transform matrix from '{}' to '{}' at any time",
                fromFrame, toFrame
            ));
        }
        else {
            return glm::dmat3(1.0);
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
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error estimating transform matrix from '{}' to from '{}' at time '{}'",
                fromFrame, toFrame, time
            ));
        }
    }
    else if (coveredTimes.upper_bound(time) == coveredTimes.end()) {
        // coverage earlier, fetch last transform
        pxform_c(
            fromFrame.c_str(),
            toFrame.c_str(),
            *(coveredTimes.rbegin()),
            reinterpret_cast<double(*)[3]>(glm::value_ptr(result))
        );
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error estimating transform matrix from frame '{}' to '{}' at time '{}'",
                fromFrame, toFrame, time
            ));
        }
    }
    else {
        // coverage both earlier and later, interpolate these transformations
        const double earlier = *std::prev((coveredTimes.lower_bound(time)));
        const double later = *(coveredTimes.upper_bound(time));

        glm::dmat3 earlierTransform = glm::dmat3(1.0);
        pxform_c(
            fromFrame.c_str(),
            toFrame.c_str(),
            earlier,
            reinterpret_cast<double(*)[3]>(glm::value_ptr(earlierTransform))
        );
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error estimating transform matrix from frame '{}' to '{}' at time '{}'",
                fromFrame, toFrame, time
            ));
        }

        glm::dmat3 laterTransform = glm::dmat3(1.0);
        pxform_c(
            fromFrame.c_str(),
            toFrame.c_str(),
            later,
            reinterpret_cast<double(*)[3]>(glm::value_ptr(laterTransform))
        );
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error estimating transform matrix from frame '{}' to '{}' at time '{}'",
                fromFrame, toFrame, time
            ));
        }

        const double t = (time - earlier) / (later - earlier);
        result = earlierTransform * (1.0 - t) + laterTransform * t;
    }

    return result;
}

void SpiceManager::loadLeapSecondsSpiceKernel() {
    constexpr std::string_view Naif00012tlsSource = R"(
KPL/LSK


LEAPSECONDS KERNEL FILE
===========================================================================

Modifications:
--------------

2016, Jul. 14   NJB  Modified file to account for the leapsecond that
                     will occur on December 31, 2016.

2015, Jan. 5    NJB  Modified file to account for the leapsecond that
                     will occur on June 30, 2015.

2012, Jan. 5    NJB  Modified file to account for the leapsecond that
                     will occur on June 30, 2012.

2008, Jul. 7    NJB  Modified file to account for the leapsecond that
                     will occur on December 31, 2008.

2005, Aug. 3    NJB  Modified file to account for the leapsecond that
                     will occur on December 31, 2005.

1998, Jul  17   WLT  Modified file to account for the leapsecond that
                     will occur on December 31, 1998.

1997, Feb  22   WLT  Modified file to account for the leapsecond that
                     will occur on June 30, 1997.

1995, Dec  14   KSZ  Corrected date of last leapsecond from 1-1-95
                     to 1-1-96.

1995, Oct  25   WLT  Modified file to account for the leapsecond that
                     will occur on Dec 31, 1995.

1994, Jun  16   WLT  Modified file to account for the leapsecond on
                     June 30, 1994.

1993, Feb. 22  CHA   Modified file to account for the leapsecond on
                     June 30, 1993.

1992, Mar. 6   HAN   Modified file to account for the leapsecond on
                     June 30, 1992.

1990, Oct. 8   HAN   Modified file to account for the leapsecond on
                     Dec. 31, 1990.


Explanation:
------------

The contents of this file are used by the routine DELTET to compute the
time difference

[1]       DELTA_ET  =  ET - UTC

the increment to be applied to UTC to give ET.

The difference between UTC and TAI,

[2]       DELTA_AT  =  TAI - UTC

is always an integral number of seconds. The value of DELTA_AT was 10
seconds in January 1972, and increases by one each time a leap second
is declared. Combining [1] and [2] gives

[3]       DELTA_ET  =  ET - (TAI - DELTA_AT)

                    =  (ET - TAI) + DELTA_AT

The difference (ET - TAI) is periodic, and is given by

[4]       ET - TAI  =  DELTA_T_A  + K sin E

where DELTA_T_A and K are constant, and E is the eccentric anomaly of the
heliocentric orbit of the Earth-Moon barycenter. Equation [4], which ignores
small-period fluctuations, is accurate to about 0.000030 seconds.

The eccentric anomaly E is given by

[5]       E = M + EB sin M

where M is the mean anomaly, which in turn is given by

[6]       M = M  +  M t
               0     1

where t is the number of ephemeris seconds past J2000.

Thus, in order to compute DELTA_ET, the following items are necessary.

          DELTA_TA
          K
          EB
          M0
          M1
          DELTA_AT      after each leap second.

The numbers, and the formulation, are taken from the following sources.

     1) Moyer, T.D., Transformation from Proper Time on Earth to
        Coordinate Time in Solar System Barycentric Space-Time Frame
        of Reference, Parts 1 and 2, Celestial Mechanics 23 (1981),
        33-56 and 57-68.

     2) Moyer, T.D., Effects of Conversion to the J2000 Astronomical
        Reference System on Algorithms for Computing Time Differences
        and Clock Rates, JPL IOM 314.5--942, 1 October 1985.

The variable names used above are consistent with those used in the
Astronomical Almanac.

\begindata

DELTET/DELTA_T_A       =   32.184
DELTET/K               =    1.657D-3
DELTET/EB              =    1.671D-2
DELTET/M               = (  6.239996D0   1.99096871D-7 )

DELTET/DELTA_AT        = ( 10,   @1972-JAN-1
                           11,   @1972-JUL-1
                           12,   @1973-JAN-1
                           13,   @1974-JAN-1
                           14,   @1975-JAN-1
                           15,   @1976-JAN-1
                           16,   @1977-JAN-1
                           17,   @1978-JAN-1
                           18,   @1979-JAN-1
                           19,   @1980-JAN-1
                           20,   @1981-JUL-1
                           21,   @1982-JUL-1
                           22,   @1983-JUL-1
                           23,   @1985-JUL-1
                           24,   @1988-JAN-1
                           25,   @1990-JAN-1
                           26,   @1991-JAN-1
                           27,   @1992-JUL-1
                           28,   @1993-JUL-1
                           29,   @1994-JUL-1
                           30,   @1996-JAN-1
                           31,   @1997-JUL-1
                           32,   @1999-JAN-1
                           33,   @2006-JAN-1
                           34,   @2009-JAN-1
                           35,   @2012-JUL-1
                           36,   @2015-JUL-1
                           37,   @2017-JAN-1 )

\begintext


)";
const std::filesystem::path path = std::filesystem::temp_directory_path();
    const std::filesystem::path file = path / "naif0012.tls";
    {
        std::ofstream f(file);
        f << Naif00012tlsSource;
    }
    loadKernel(file);
    std::filesystem::remove(file);
}

void SpiceManager::loadGeophysicalConstantsKernel() {
    constexpr std::string_view GeoPhysicalConstantsKernelSource = R"(
KPL/PCK

      The SPK creations applications (mkspk, mkspk_c) require the data in
      this kernel to produce Type 10 SPK segments based upon the Two-Line
      element sets available from NORAD/SPACETRACK. The data applies ONLY
      to the Two-Line Element sets and only to the SGP4 implementations
      included in the SPICE library [1][2]. The SPK application copies
      this data to the constants partition of the Type 10 segment, so the
      user has no need for the kernel after creation of the corresponding
      SPK.

      Bill Taber (JPL)
      Edward Wright (JPL)

      The assigned values are taken from the Spacetrack #3 report, referred
      to as WGS721 in Vallado [2]. It is possible to edit this file
      to use the high accuracy WGS-72 values (WGS72) or the WGS-84
      values (WGS84). The KE parameter value for WGS72 and WGS84
      is calculated from the MU and ER values. The lists include MU only
      for the calculation of KE.

      MU, the standard gravitational parameter, in cubic kilometer per
      second squared.

      WGS721 (STR#3 values) [1]

         ER =    6378.135

         J2   =    0.001082616
         J3   =   -0.00000253881
         J4   =   -0.00000165597

         KE   =    0.0743669161


      WGS72 [2]

         MU   =  398600.8
         ER   =  6378.135

         J2   =   0.001082616
         J3   =  -0.00000253881
         J4   =  -0.00000165597

         KE   =  60.0D0/DSQRT(ER**3/MU)
              =  0.074366916133173


      WGS84 [2]

         MU   =  398600.5
         ER   =  6378.137

         J2   =   0.00108262998905
         J3   =  -0.00000253215306
         J4   =  -0.00000161098761

         KE   =  60.0D0/DSQRT(ER**3/MU)
              =  0.074366853168714


      The BODY399_Jn values are un-normalized zonal harmonic values
      for the earth. These numbers are dimensionless.

\begindata

      BODY399_J2 =    1.082616D-3
      BODY399_J3 =   -2.53881D-6
      BODY399_J4 =   -1.65597D-6

\begintext

      The next item is the square root of GM for the earth given
      in units of earth-radii**1.5/Minute

\begindata

      BODY399_KE =    7.43669161D-2

\begintext

      The next two items define the top and bottom of the atmospheric
      drag model, distance above the surface in kilometers, used by
      the type 10 ephemeris type.

\begindata

      BODY399_QO =  120.0D0
      BODY399_SO =   78.0D0

\begintext

      The equatorial radius of the earth in kilometers as used by
      the TLE propagator.

\begindata

      BODY399_ER = 6378.135D0

\begintext

      The value of AE is the number of distance units per earth
      radii used by the NORAD state propagation software. Don't
      change this value.

\begindata

      BODY399_AE = 1.0D0

\begintext

References:

   [1] Hoots, F. R., and Roehrich, R., l. 1980. "Spacetrack Report #3, Models
       for Propagation of the NORAD Element Sets." U.S. Air Force, CO.

   [2] Vallado, David, Crawford, Paul, Hujsak, Richard,
       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
       Specialist Conference, August 21-24, 2006. Keystone, CO.
)";

      const std::filesystem::path path = std::filesystem::temp_directory_path();
      const std::filesystem::path file = path / "geophysical.ker";
      {
          std::ofstream f(file);
          f << GeoPhysicalConstantsKernelSource;
      }
      loadKernel(file);
      std::filesystem::remove(file);
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
            codegen::lua::LoadKernel,
            codegen::lua::UnloadKernel,
            codegen::lua::Kernels,
            codegen::lua::SpiceBodies,
            codegen::lua::RotationMatrix,
            codegen::lua::Position,
            codegen::lua::ConvertTLEtoSPK
        }
    };
}

} // namespace openspace
