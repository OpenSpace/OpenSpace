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

#ifndef __OPENSPACE_CORE___PERFORMANCEMANAGER___H__
#define __OPENSPACE_CORE___PERFORMANCEMANAGER___H__

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace ghoul { class SharedMemory; }
namespace openspace { class SceneGraphNode; }

namespace openspace::performance {

struct PerformanceLayout;

class PerformanceManager {
public:
    static void CreateGlobalSharedMemory();
    static void DestroyGlobalSharedMemory();

    ~PerformanceManager();

    void setEnabled(bool enabled);
    bool isEnabled() const;

    void resetPerformanceMeasurements();

    void storeIndividualPerformanceMeasurement(const std::string& identifier,
        long long microseconds);
    void storeScenePerformanceMeasurements(
        const std::vector<SceneGraphNode*>& sceneNodes);

    void outputLogs();

    void writeData(std::ofstream& out, const std::vector<float>& data);

    std::string formatLogName(std::string nodeName);

    void logDir(std::string dir);
    const std::string& logDir() const;
    void prefix(std::string prefix);
    const std::string& prefix() const;

    void enableLogging();
    void disableLogging();
    void toggleLogging();
    void setLogging(bool enabled);
    bool loggingEnabled() const;

    PerformanceLayout* performanceData();

private:
    bool _performanceMeasurementEnabled = false;
    bool _loggingEnabled = false;

    std::string _logDir;
    std::string _prefix;
    std::string _ext = "log";

    std::map<std::string, size_t> individualPerformanceLocations;

    std::unique_ptr<ghoul::SharedMemory> _performanceMemory;

    size_t _currentTick = 0;

    void tick();
    bool createLogDir();
};

} // namespace openspace::performance

#endif // __OPENSPACE_CORE___PERFORMANCEMANAGER___H__
