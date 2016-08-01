/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/scene/scenegraphnode.h>
#include <openspace/performance/performancelayout.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/sharedmemory.h>
#include <ghoul/misc/onscopeexit.h>

namespace {
    const std::string _loggerCat = "PerformanceManager";
    
    const std::string GlobalSharedMemoryName = "OpenSpacePerformanceMeasurementData";
    // Probably 255 performance blocks per node are enough, so we can get away with
    // 260 bytes (one uint8_t for the number, 255 uint8_t for the names,  one uint8_t
    // for the reference count to keep the global memory alive, and 3 bytes to enforce
    // alignment)
    const size_t GlobalSharedMemorySize = 260;
    
    struct GlobalMemory {
        uint8_t number;
        uint8_t referenceCount;
        std::array<uint8_t, 255> values;
        
        std::array<uint8_t, 3> alignment;
    };
    
    const uint8_t UnusedName = 255;
    
    
    const std::string LocalSharedMemoryNameBase = "PerformanceMeasurement_";
}

namespace openspace {
namespace performance {

// The Performance Manager will use a level of indirection in order to support multiple
// PerformanceManagers running in parallel:
// The ghoul::SharedData block addressed by OpenSpacePerformanceMeasurementSharedData
// will only get allocated once and contains the total number of allocated shared memory
// blocks alongside a list of names of these blocks
//

void PerformanceManager::createGlobalSharedMemory() {
    static_assert(
        sizeof(GlobalMemory) == GlobalSharedMemorySize,
        "The global memory struct does not fit the allocated global memory space"
    );
    
    using ghoul::SharedMemory;
    
    if (SharedMemory::exists(GlobalSharedMemoryName)) {
        SharedMemory sharedMemory(GlobalSharedMemoryName);
        sharedMemory.acquireLock();
        GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
        ++(m->referenceCount);
        sharedMemory.releaseLock();
    }
    else {
        LINFO("Creating global shared memory block for performance measurements");
        SharedMemory::create(GlobalSharedMemoryName, GlobalSharedMemorySize);
        
        // Initialize the data
        SharedMemory sharedMemory(GlobalSharedMemoryName);
        sharedMemory.acquireLock();
        new (sharedMemory.memory()) GlobalMemory;
        GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
        m->number = 0;
        m->referenceCount = 1;
        m->values.fill(UnusedName);
        sharedMemory.releaseLock();
    }
}

void PerformanceManager::destroyGlobalSharedMemory() {
    using ghoul::SharedMemory;
    if (!SharedMemory::exists(GlobalSharedMemoryName)) {
        LWARNING("Global shared memory for Performance measurements did not exist");
        return;
    }
    
    SharedMemory sharedMemory(GlobalSharedMemoryName);
    sharedMemory.acquireLock();
    GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
    --(m->referenceCount);
    if (m->referenceCount == 0) {
        SharedMemory::remove(GlobalSharedMemoryName);
    }
    sharedMemory.releaseLock();
}
    
PerformanceManager::PerformanceManager()
    : _performanceMemory(nullptr)
{
    using ghoul::SharedMemory;
    
    SharedMemory sharedMemory(GlobalSharedMemoryName);
    sharedMemory.acquireLock();
    OnExit([&](){sharedMemory.releaseLock();});
    
    GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());

    // The the first free block (which also coincides with the number of blocks
    uint8_t blockIndex = m->number;
    std::string localName = LocalSharedMemoryNameBase + std::to_string(blockIndex);
    
    // Compute the total size
    const int totalSize = sizeof(PerformanceLayout);
    LINFO("Create shared memory of " << totalSize << " bytes");

    if (SharedMemory::exists(localName)) {
        throw ghoul::RuntimeError("Shared Memory block already existed");
    }
    
    ghoul::SharedMemory::create(localName, totalSize);

    _performanceMemory = std::make_unique<ghoul::SharedMemory>(localName);
    // Using the placement-new to create a PerformanceLayout in the shared memory
    new (_performanceMemory->memory()) PerformanceLayout;
}

PerformanceManager::~PerformanceManager() {
    if (_performanceMemory) {
        ghoul::SharedMemory sharedMemory(GlobalSharedMemoryName);
        sharedMemory.acquireLock();
        GlobalMemory* m = reinterpret_cast<GlobalMemory*>(sharedMemory.memory());
        m->number = m->number - 1;
        sharedMemory.releaseLock();
        
        ghoul::SharedMemory::remove(_performanceMemory->name());
    }
}

void PerformanceManager::resetPerformanceMeasurements() {
    // Using the placement-new to create a PerformanceLayout in the shared memory
    _performanceMemory->acquireLock();
    void* ptr = _performanceMemory->memory();
    new (ptr) PerformanceLayout;
    _performanceMemory->releaseLock();
    
    individualPerformanceLocations.clear();
}
    
bool PerformanceManager::isMeasuringPerformance() const {
    return _doPerformanceMeasurements;
}
    
PerformanceLayout* PerformanceManager::performanceData() {
    void* ptr = _performanceMemory->memory();
    return reinterpret_cast<PerformanceLayout*>(ptr);
}

void PerformanceManager::storeIndividualPerformanceMeasurement
                                         (std::string identifier, long long microseconds)
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
    p->time[PerformanceLayout::NumberValues - 1] =
        static_cast<float>(microseconds);

    _performanceMemory->releaseLock();
}

void PerformanceManager::storeScenePerformanceMeasurements(
                                           const std::vector<SceneGraphNode*>& sceneNodes)
{
    using namespace performance;

    PerformanceLayout* layout = performanceData();
    _performanceMemory->acquireLock();
    
    int nNodes = static_cast<int>(sceneNodes.size());
    layout->nScaleGraphEntries = nNodes;
    for (int i = 0; i < nNodes; ++i) {
        SceneGraphNode* node = sceneNodes[i];

        memset(layout->sceneGraphEntries[i].name, 0, PerformanceLayout::LengthName);
#ifdef _MSC_VER
        strcpy_s(layout->sceneGraphEntries[i].name, node->name().length() + 1, node->name().c_str());
#else
        strcpy(layout->sceneGraphEntries[i].name, node->name().c_str());
#endif
        
        SceneGraphNode::PerformanceRecord r = node->performanceRecord();
        PerformanceLayout::SceneGraphPerformanceLayout& entry = layout->sceneGraphEntries[i];

        std::rotate(
            std::begin(entry.renderTime),
            std::next(std::begin(entry.renderTime)),
            std::end(entry.renderTime)
        );
        entry.renderTime[PerformanceLayout::NumberValues - 1] = r.renderTime / 1000.f;
        
        std::rotate(
            std::begin(entry.updateEphemeris),
            std::next(std::begin(entry.updateEphemeris)),
            std::end(entry.updateEphemeris)
        );
        entry.updateEphemeris[PerformanceLayout::NumberValues - 1] = r.updateTimeEphemeris / 1000.f;
        
        std::rotate(
            std::begin(entry.updateRenderable),
            std::next(std::begin(entry.updateRenderable)),
            std::end(entry.updateRenderable)
        );
        entry.updateRenderable[PerformanceLayout::NumberValues - 1] = r.updateTimeRenderable / 1000.f;
    }
    _performanceMemory->releaseLock();
}

} // namespace performance
} // namespace openspace
