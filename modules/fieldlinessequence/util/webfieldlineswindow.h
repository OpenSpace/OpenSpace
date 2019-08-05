/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESWINDOW___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESWINDOW___H__

#include <modules/fieldlinessequence/util/webfieldlinesworker.h>
#include <vector>

namespace openspace {
    
struct Window {
    
    // This is the essence of the Window, pairs of triggertimes and download keys
    std::vector<std::pair<double, std::string>> triggerTimes;
    
    int activeTriggerTime;
    
    // Amount of steps from current timetrigger to first respectively last trigger in window
    // We might want different widths depending on direction and speed of time
    int backWidth;
    int forwardWidth;
    
    // Number of elements in window
    int nTriggerTimes;
};
    
class WebFieldlinesWindow{
public:
    // Constructors
    WebFieldlinesWindow() = default;
    
    // If files existed on disk already
    WebFieldlinesWindow(std::string syncDir, std::string serverUrl,
                        std::vector<std::string>& _sourceFiles,
                        std::vector<double>& _startTimes, size_t& _nStates);
    
    // PUBLIC MEMBER FUNCTIONS
    bool timeIsInTriggerTimesWebList(double time);
    
    void getNewTriggerTimesWebList(double time);
    
    // Returns true if time is inside the current window
    bool timeIsInWindow(double time);
    
    // Returns true if time is at edge of the current window,
    // and will probably need to update window
    bool timeIsInWindowMargin(double time, double direction);

    // Release the worker for execution, the worker is taking care of the downloading-priorities and logic
    void executeDownloadWorker();
    
    void newWindow(double time);

    // Returns the first and last trigger of window
    double windowStart();
    double windowEnd();

private:
    
    // PRIVATE MEMBER VARIABLES
    
    // Sliding download window
    Window _window;
    
    // The worker that handles downloads
    WebFieldlinesWorker _worker;
    
    // Keeps track of the files available on disk
    // Shared ptr because WebfieldWorker has the same copy
    // (Still unsure if this is the best way)
    std::shared_ptr<std::vector<std::pair<double, std::string>>> _triggerTimesOnDisk;
    
    // This is a long list of everything available online,
    // they are tuples of 3; double is timetrigger, string is donwload key, int is index to where it is on disk
    // and indices to the respective files on disk if they exist/are downloaded
    // -1 means not downloaded
    std::vector<std::tuple<double, std::string, int>> _triggerTimesWeb;

    int _nAvailableWeb;
    
    // POINTERS TO RENDERABLEFIELDLIENSSEQUENCE
    // Number of states in the sequence
    size_t *rfs_nStates;
    // Stores the provided source file paths
    std::vector<std::string> *rfs_sourceFiles;
    // Contains the _triggerTimes for all FieldlineStates in the sequence
    std::vector<double> *rfs_startTimes;
    
    // PRIVATE MEMBER FUNCTIONS
    
    

    

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESWINDOW___H__
