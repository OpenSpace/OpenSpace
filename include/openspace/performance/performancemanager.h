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

#ifndef __OPENSPACE_CORE___PERFORMANCEMANAGER___H__
#define __OPENSPACE_CORE___PERFORMANCEMANAGER___H__

#include <openspace/performance/performancelayout.h>

#include <ghoul/misc/sharedmemory.h>

#include <map>
#include <memory>
#include <vector>

namespace ghoul { class SharedMemory; }

namespace openspace { class SceneGraphNode; }

namespace openspace::performance {

class PerformanceManager {
public:
    static void createGlobalSharedMemory();
    static void destroyGlobalSharedMemory();
    
    PerformanceManager();
    ~PerformanceManager();

    void resetPerformanceMeasurements();
    
    bool isMeasuringPerformance() const;

    void storeIndividualPerformanceMeasurement(std::string identifier, long long nanoseconds);
    void storeScenePerformanceMeasurements(const std::vector<SceneGraphNode*>& sceneNodes);
    
    void outputLogs();

    void writeData(std::ofstream& out, const std::vector<float>& data);

    std::string formatLogName(std::string nodeName);

    void logDir(std::string dir);
    std::string logDir() const;
    void prefix(std::string prefix);
    std::string prefix() const;

    void enableLogging();
    void disableLogging();
    void toggleLogging();
    void setLogging(bool enabled);
    bool loggingEnabled() const;

    PerformanceLayout* performanceData();

private:
    bool _doPerformanceMeasurements;
    bool _loggingEnabled;

    std::string _logDir;
    std::string _prefix;
    std::string _suffix;
    std::string _ext;
    
    std::map<std::string, size_t> individualPerformanceLocations;
    
    std::unique_ptr<ghoul::SharedMemory> _performanceMemory;

    size_t _tick;
    
    void tick();
    bool createLogDir();
};

} // namespace openspace::performance

#endif // __OPENSPACE_CORE___PERFORMANCEMANAGER___H__
