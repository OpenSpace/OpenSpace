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

#include <modules/fieldlinessequence/util/webfieldlineswindow.h>

#include <ghoul/logging/logmanager.h>
#include <openspace/util/httprequest.h>
#include <modules/sync/syncs/httpsynchronization.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <openspace/util/timemanager.h>
#include <openspace/engine/globals.h>

 // Tracy
//#include <Tracy.hpp>


namespace {
    constexpr const char* _loggerCat = "WebFieldlinesWindow";
    
} // namespace

namespace openspace{

WebFieldlinesWindow::WebFieldlinesWindow(std::string syncDir, std::string serverUrl,
                                                std::vector<std::string>& _sourceFiles,
                                    std::vector<double>& _startTimes)
{

    _window.backWidth = 3;
    _window.forwardWidth = 3;
    _window.nTriggerTimes = _sourceFiles.size();

    for(int i = 0; i < _window.nTriggerTimes ; i++){
        TriggerTime tt = { _startTimes[i], _sourceFiles[i], true };
        _window.triggerTimes.push_back(tt);
    }
        
    rfs_sourceFiles = &_sourceFiles;
        
    _nAvailableWeb = 0; // haven't downloaded that list yet
        
    _worker = WebFieldlinesWorker(syncDir, serverUrl);

    for (int i = 0; i < _window.nTriggerTimes; i++) {
        _worker.addToDownloadedList(_startTimes[i], _sourceFiles[i]);
    }
}
       
// Returns true if time is inside the current window
bool WebFieldlinesWindow::timeIsInWindow(double time) {
    if (_window.triggerTimes.empty())
        return false;
    return time >= windowStart() && time <= windowEnd();
}
    
// Returns true if time is at edge of the current window,
// and will probably need to update window
bool WebFieldlinesWindow::timeIsInWindowMargin(double time, double direction) {
    const int threshold = 2; //TODO base this on speed later
        
    if (direction > 0){ // If time is moving forward
        if (time >= _window.triggerTimes[_window.nTriggerTimes - threshold].triggertime) {
            if (time > windowEnd()) {
                return false;
            }
            return true;
        }
        else return false;
    }
    else{ // If time is moving backwards
        if (time <= _window.triggerTimes[threshold].triggertime){
            if (time < windowStart()) {
                return false;
            }
            return true;
        }
        else return false;
    }
}

/*
    Release the worker for execution,
    Pick up a timestep to request for download,
    Check if that timestep is already on disk,
    repeated until a proper timestep to download is found, and start download
*/
void WebFieldlinesWindow::executeDownloadWorker() {
    _worker.downloadWindow(_window.triggerTimes);
    _worker.updateRFSSourceFiles(*rfs_sourceFiles);
    _edgeWindowReady = false;
}
    
void WebFieldlinesWindow::newWindow(double time) {
    auto it = std::find_if(
        _triggerTimesWeb.rbegin(),
        _triggerTimesWeb.rend(),
        [time](auto element) {
            return time > element.first;
        }
    );

    const int index = static_cast<int>(std::distance(it, _triggerTimesWeb.rend())) - 1;
    _window.triggerTimes.clear();
    _window.nTriggerTimes = 0;

    // This should be safe, because in the manager,
    // it is checked wether the current position is within
    // the boundaries with respect to back & forward width
    for(int i = std::max(index - _window.backWidth,0);
            i <= std::min(index + _window.forwardWidth,
                static_cast<int>(_triggerTimesWeb.size() -1));
            i++) {

      //bool onFile = FileSys.fileExists();     //put onFile instead of false next line
        TriggerTime tt = {_triggerTimesWeb[i].first, _triggerTimesWeb[i].second, false };
        _window.triggerTimes.push_back(tt);
        _window.nTriggerTimes++;
    }

    if (_worker.edgeMode()) {
        _edgeWindowReady = true;
    }

    _worker.newWindowToDownload();
}
    
bool WebFieldlinesWindow::timeIsInTriggerTimesWebList(double time) {
    // There are no files to compare with, just going to be false.
    if (_nAvailableWeb == 0) return false;

    // Most cases, we are currently in the middle of a bunch of datasets,
    // if we are not, lets get some new ones.
    return (
        time >= (_triggerTimesWeb.front().first) &&
        time <= (_triggerTimesWeb.back().first)
    );
}
    
void WebFieldlinesWindow::getNewTriggerTimesWebList(double time) {
    // _triggerTimesWeb gets initialized here
    _worker.getRangeOfAvailableTriggerTimes(time, _triggerTimesWeb);
    _nAvailableWeb = static_cast<int>(_triggerTimesWeb.size());
}

bool WebFieldlinesWindow::workerWindowIsReady() {
    return _worker.windowIsComplete();
}

bool WebFieldlinesWindow::expectedWindowIsOutOfBounds(double time) {
    auto resultForwards = std::find_if(
        _triggerTimesWeb.rbegin(),
        _triggerTimesWeb.rbegin() + _window.forwardWidth,
        [time](auto pair) {
            return time > pair.first;
        }
    );

    auto resultBackwards = std::find_if(
        _triggerTimesWeb.begin(),
        _triggerTimesWeb.begin() + _window.backWidth,
        [time](auto pair) {
            return time < pair.first;
        }
    );

    return resultForwards != _triggerTimesWeb.rbegin() + _window.forwardWidth ||
        resultBackwards != _triggerTimesWeb.begin() + _window.backWidth;
}

void WebFieldlinesWindow::rfsHasUpdated() {
    _worker.flagUpdated();
}

bool WebFieldlinesWindow::checkWorkerEdgeMode(){
    return _worker.edgeMode();
}

bool WebFieldlinesWindow::edgeWindowReady(){
    return _edgeWindowReady;
}

// Returns first trigger of window
double WebFieldlinesWindow::windowStart(){
    return _window.triggerTimes.front().triggertime;
}

// Returns last trigger of window
double WebFieldlinesWindow::windowEnd(){
    return _window.triggerTimes.back().triggertime;
}
    
} // namespace openspace
