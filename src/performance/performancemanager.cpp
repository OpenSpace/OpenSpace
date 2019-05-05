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

#include <openspace/performance/performancemanager.h>

#include <openspace/performance/performancelayout.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/sharedmemory.h>
#include <algorithm>
#include <cstring>
#include <iostream>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "PerformanceManager";

    constexpr const char* GlobalSharedMemoryName = "OpenSpacePerformanceMeasurementData";
    // Probably 255 performance blocks per node are enough, so we can get away with
    // 4 bytes (one uint8_t for the number, one uint8_t for the reference count to keep
    // the global memory alive, and 2 bytes to enforce alignment)
    constexpr const size_t GlobalSharedMemorySize = 4;

    struct GlobalMemory {
        uint8_t number;
        uint8_t referenceCount;

        std::array<uint8_t, 2> alignment;
    };

    constexpr const char* LocalSharedMemoryNameBase = "PerformanceMeasurement_";
} // namespace

namespace openspace::performance {

// The Performance Manager will use a level of indirection in order to support multiple
// PerformanceManagers running in parallel:
// The ghoul::SharedData block addressed by OpenSpacePerformanceMeasurementSharedData
// will only get allocated once and contains the total number of allocated shared memory
// blocks alongside a list of names of these blocks
//

void PerformanceManager::CreateGlobalSharedMemory() {
    static_assert(
        sizeof(GlobalMemory) == GlobalSharedMemorySize,
        "The global memory struct does not fit the allocated global memory space"
    );

    if (ghoul::SharedMemory::exists(GlobalSharedMemoryName)) {
        ghoul::SharedMemory sharedMemory(GlobalSharedMemoryName);
        sharedMemory.acquireLock();
        GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
        ++(m->referenceCount);
        LINFO(fmt::format(
            "Using global shared memory block for performance measurements. "
            "Reference count: {}",
            int(m->referenceCount)
        ));
        sharedMemory.releaseLock();
    }
    else {
        LINFO("Creating global shared memory block for performance measurements");
        ghoul::SharedMemory::create(GlobalSharedMemoryName, GlobalSharedMemorySize);

        // Initialize the data
        ghoul::SharedMemory sharedMemory(GlobalSharedMemoryName);
        sharedMemory.acquireLock();
        new (sharedMemory.memory()) GlobalMemory;
        GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
        m->number = 0;
        m->referenceCount = 1;
        sharedMemory.releaseLock();
    }
}

void PerformanceManager::DestroyGlobalSharedMemory() {
    if (!ghoul::SharedMemory::exists(GlobalSharedMemoryName)) {
        LWARNING("Global shared memory for Performance measurements did not exist");
        return;
    }

    ghoul::SharedMemory sharedMemory(GlobalSharedMemoryName);
    sharedMemory.acquireLock();
    GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
    --(m->referenceCount);
    LINFO(fmt::format(
        "Global shared performance memory reference count: {}",
        static_cast<int>(m->referenceCount)
    ));
    if (m->referenceCount == 0) {
        LINFO("Removing global shared performance memory");

        // When the global memory is deleted, we have to get rid of all local memory as
        // well. In principle, none should be left, but OpenSpace crashing might leave
        // some of the memory orphaned
        for (int i = 0; i < std::numeric_limits<uint8_t>::max(); ++i) {
            std::string localName = LocalSharedMemoryNameBase + std::to_string(i);
            if (ghoul::SharedMemory::exists(localName)) {
                LINFO(fmt::format("Removing shared memory: {}", localName));
                ghoul::SharedMemory::remove(localName);
            }
        }

        ghoul::SharedMemory::remove(GlobalSharedMemoryName);
    }
    sharedMemory.releaseLock();
}

PerformanceManager::~PerformanceManager() {}

void PerformanceManager::setEnabled(bool enabled) {
    _logDir = absPath("${BASE}");
    _prefix = "PM-";

    _performanceMeasurementEnabled = enabled;

    if (enabled) {
        PerformanceManager::CreateGlobalSharedMemory();

        ghoul::SharedMemory sharedMemory(GlobalSharedMemoryName);
        sharedMemory.acquireLock();
        defer {
            sharedMemory.releaseLock();
        };

        GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());

        // The the first free block (which also coincides with the number of blocks
        uint8_t blockIndex = m->number;
        ++(m->number);

        const std::string& localName = LocalSharedMemoryNameBase +
                                       std::to_string(blockIndex);

        // Compute the total size
        const int totalSize = sizeof(PerformanceLayout);
        LINFO(fmt::format("Create shared memory '{}' of {} bytes", localName, totalSize));

        if (ghoul::SharedMemory::exists(localName)) {
            throw ghoul::RuntimeError(
                "Shared Memory '" + localName + "' block already existed"
            );
        }

        ghoul::SharedMemory::create(localName, totalSize);

        _performanceMemory = std::make_unique<ghoul::SharedMemory>(localName);
        // Using the placement-new to create a PerformanceLayout in the shared memory
        new (_performanceMemory->memory()) PerformanceLayout;
    }
    else {
        if (loggingEnabled()) {
            outputLogs();
        }

        if (_performanceMemory) {
            ghoul::SharedMemory sharedMemory(GlobalSharedMemoryName);
            sharedMemory.acquireLock();
            GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
            --(m->number);
            sharedMemory.releaseLock();

            LINFO(fmt::format("Remove shared memory '{}'", _performanceMemory->name()));
            ghoul::SharedMemory::remove(_performanceMemory->name());

            _performanceMemory = nullptr;
        }

        PerformanceManager::DestroyGlobalSharedMemory();
    }
}

bool PerformanceManager::isEnabled() const {
    return _performanceMeasurementEnabled;
}

void PerformanceManager::resetPerformanceMeasurements() {
    // Using the placement-new to create a PerformanceLayout in the shared memory
    _performanceMemory->acquireLock();
    new (_performanceMemory->memory()) PerformanceLayout;
    _performanceMemory->releaseLock();

    individualPerformanceLocations.clear();
}

void PerformanceManager::outputLogs() {
    // Log Layout values
    PerformanceLayout* layout = performanceData();
    const size_t writeStart = (PerformanceLayout::NumberValues - 1) - _currentTick;

    // Log function performance
    for (int16_t n = 0; n < layout->nFunctionEntries; n++) {
        const PerformanceLayout::FunctionPerformanceLayout& function =
            layout->functionEntries[n];
        std::string filename = formatLogName(function.name);
        std::ofstream out = std::ofstream(
            absPath(std::move(filename)),
            std::ofstream::out | std::ofstream::app
        );

        // Comma separate data
        for (size_t i = writeStart; i < PerformanceLayout::NumberValues; i++) {
            const std::vector<float>& data = { function.time[i] };
            writeData(out, data);
        }
        out.close();
    }

    // Log scene object performance
    for (int16_t n = 0; n < layout->nScaleGraphEntries; n++) {
        const PerformanceLayout::SceneGraphPerformanceLayout node =
            layout->sceneGraphEntries[n];

        // Open file
        std::string filename = formatLogName(node.name);
        std::ofstream out = std::ofstream(
            absPath(std::move(filename)),
            std::ofstream::out | std::ofstream::app
        );

        // Comma separate data
        for (size_t i = writeStart; i < PerformanceLayout::NumberValues; i++) {
            const std::vector<float> data = {
                node.renderTime[i],
                node.updateRenderable[i],
                node.updateRotation[i],
                node.updateScaling[i],
                node.updateTranslation[i]
            };
            writeData(out, data);
        }
        out.close();
    }
}

void PerformanceManager::writeData(std::ofstream& out, const std::vector<float>& data) {
    for (size_t i = 0; i < data.size() - 1; i++) {
        out << data[i] << ",";
    }
    out << data[data.size() - 1] << "\n";
}

 std::string PerformanceManager::formatLogName(std::string nodeName) {
    // Replace any colons with dashes
    std::replace(nodeName.begin(), nodeName.end(), ':', '-');
    // Replace spaces with underscore
    std::replace(nodeName.begin(), nodeName.end(), ' ', '_');
    return  _logDir + "/" + _prefix + nodeName + "." + _ext;
}

void PerformanceManager::logDir(std::string dir) {
    _logDir = absPath(std::move(dir));
}

const std::string& PerformanceManager::logDir() const {
    return _logDir;
}

void PerformanceManager::prefix(std::string prefix) {
    _prefix = std::move(prefix);
}

const std::string& PerformanceManager::prefix() const {
    return _prefix;
}

void PerformanceManager::enableLogging() {
    setLogging(true);
}

void PerformanceManager::disableLogging() {
    setLogging(false);
}

void PerformanceManager::toggleLogging() {
    setLogging(!_loggingEnabled);
}

void PerformanceManager::setLogging(bool enabled) {
    // Create the log directory if it doesn't exist. Do it here, so that it
    // only tests once each time output is enabled
    if (enabled) {
        // If it can't create the directory, it's not logging so set false
        enabled = createLogDir();
    }

    _loggingEnabled = enabled;
}

bool PerformanceManager::createLogDir() {
    // Done if it exists
    ghoul::filesystem::Directory dir(_logDir);
    if (FileSys.directoryExists(dir)) {
        return true;
    }

    // Error and set false if can't create
    try {
        FileSys.createDirectory(dir, ghoul::filesystem::FileSystem::Recursive::Yes);
    }
    catch (const ghoul::filesystem::FileSystem::FileSystemException& e) {
        LERROR(fmt::format("Could not create log directory: {}", e.message));
        return false;
    }
    return true;
}

bool PerformanceManager::loggingEnabled() const {
    return _loggingEnabled;
}

PerformanceLayout* PerformanceManager::performanceData() {
    void* ptr = _performanceMemory->memory();
    return reinterpret_cast<PerformanceLayout*>(ptr);
}

void PerformanceManager::tick() {
    _currentTick = (_currentTick + 1) % PerformanceLayout::NumberValues;
}

void PerformanceManager::storeIndividualPerformanceMeasurement(
                                                            const std::string& identifier,
                                                                   long long microseconds)
{
    PerformanceLayout* layout = performanceData();
    _performanceMemory->acquireLock();

    auto it = individualPerformanceLocations.find(identifier);
    PerformanceLayout::FunctionPerformanceLayout* p = nullptr;
    if (it == individualPerformanceLocations.end()) {
        p = &(layout->functionEntries[layout->nFunctionEntries]);
        individualPerformanceLocations[identifier] = layout->nFunctionEntries;
        ++(layout->nFunctionEntries);
    }
    else {
        p = &(layout->functionEntries[it->second]);
    }
#ifdef _MSC_VER
    strcpy_s(p->name, identifier.length() + 1, identifier.c_str());
#else
    strcpy(p->name, identifier.c_str());
#endif

    std::rotate(
        std::begin(p->time),
        std::next(std::begin(p->time)),
        std::end(p->time)
    );
    p->time[PerformanceLayout::NumberValues - 1] = static_cast<float>(microseconds);

    _performanceMemory->releaseLock();
}

void PerformanceManager::storeScenePerformanceMeasurements(
                                           const std::vector<SceneGraphNode*>& sceneNodes)
{
    PerformanceLayout* layout = performanceData();
    _performanceMemory->acquireLock();

    const int nNodes = static_cast<int>(sceneNodes.size());
    layout->nScaleGraphEntries = static_cast<int16_t>(nNodes);
    for (int i = 0; i < nNodes; ++i) {
        const SceneGraphNode& node = *sceneNodes[i];

        memset(layout->sceneGraphEntries[i].name, 0, PerformanceLayout::LengthName);
#ifdef _MSC_VER
        strcpy_s(
            layout->sceneGraphEntries[i].name,
            node.identifier().length() + 1,
            node.identifier().c_str()
        );
#else
        strcpy(layout->sceneGraphEntries[i].name, node.identifier().c_str());
#endif

        const SceneGraphNode::PerformanceRecord& r = node.performanceRecord();
        PerformanceLayout::SceneGraphPerformanceLayout& entry =
                                                             layout->sceneGraphEntries[i];

        // Covert nano to microseconds
        constexpr const float Micro = 1000.f;

        std::rotate(
            std::begin(entry.renderTime),
            std::next(std::begin(entry.renderTime)),
            std::end(entry.renderTime)
        );
        entry.renderTime[PerformanceLayout::NumberValues - 1] = r.renderTime / Micro;

        std::rotate(
            std::begin(entry.updateTranslation),
            std::next(std::begin(entry.updateTranslation)),
            std::end(entry.updateTranslation)
        );
        entry.updateTranslation[PerformanceLayout::NumberValues - 1] =
            r.updateTimeTranslation / Micro;

        std::rotate(
            std::begin(entry.updateRotation),
            std::next(std::begin(entry.updateRotation)),
            std::end(entry.updateRotation)
        );
        entry.updateRotation[PerformanceLayout::NumberValues - 1] =
            r.updateTimeRotation / Micro;

        std::rotate(
            std::begin(entry.updateScaling),
            std::next(std::begin(entry.updateScaling)),
            std::end(entry.updateScaling)
        );
        entry.updateScaling[PerformanceLayout::NumberValues - 1] =
            r.updateTimeScaling / Micro;

        std::rotate(
            std::begin(entry.updateRenderable),
            std::next(std::begin(entry.updateRenderable)),
            std::end(entry.updateRenderable)
        );
        entry.updateRenderable[PerformanceLayout::NumberValues - 1] =
            r.updateTimeRenderable / Micro;
    }
    _performanceMemory->releaseLock();

    if (_loggingEnabled && _currentTick == PerformanceLayout::NumberValues - 1) {
        outputLogs();
    }

    tick();
}

} // namespace openspace::performance
