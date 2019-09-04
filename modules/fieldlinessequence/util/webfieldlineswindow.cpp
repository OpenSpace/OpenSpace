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


namespace {
    constexpr const char* _loggerCat = "FieldlinesSequence[ Web FLs Window ]";
    
} // namespace

namespace openspace{
    // --------------------------- CONSTRUCTORS ---------------------------------------//
    WebFieldlinesWindow::WebFieldlinesWindow(std::string syncDir, std::string serverUrl,
                                             std::vector<std::string>& _sourceFiles,
                                             std::vector<double>& _startTimes, size_t& _nStates, int apiID):
                                             _apiID(apiID){
        _window.backWidth = 3;
        _window.forwardWidth = 3;
        
        _window.nTriggerTimes = static_cast<int>(_nStates);
        
        for(int i = 0; i < _window.nTriggerTimes ; i++){
            _window.triggerTimes.push_back(std::make_pair(_startTimes[i], _sourceFiles[i]));
        }
        
        rfs_nStates = &_nStates;
        rfs_sourceFiles = &_sourceFiles;
        rfs_startTimes = &_startTimes;
        
        _nAvailableWeb = 0; // haven't downloaded that list yet
        
        _worker = WebFieldlinesWorker(syncDir, serverUrl);
                
    }
    
    // -------------------------- PUBLIC FUNCTIONS  -----------------------------------//
    
    // Returns true if time is inside the current window
    bool WebFieldlinesWindow::timeIsInWindow(double time){
        if(time >= windowStart() && time <= windowEnd())
            return true;
        else return false;
    }
    
    // Returns true if time is at edge of the current window,
    // and will probably need to update window
    bool WebFieldlinesWindow::timeIsInWindowMargin(double time, double direction){
        int threshold = 2; // base this on speed later
        
        if (direction > 0){ // If time is moving forward
            if(time >= _window.triggerTimes[_window.nTriggerTimes - threshold].first){
                if(time > windowEnd()){
                    LERROR("Time is outside of window in margin-check-function. This shouldn't happen.");
                    return false;
                }
                return true;
            }
            else return false;
        }
        else{ // If time is moving backwards
            if(time <= _window.triggerTimes[threshold].first){
                if(time < windowStart()){
                    LERROR("Time is outside of window in margin-check-function. This shouldn't happen.");
                    return false;
                }
                return true;
            }
            else return false;
        }
    }

    /*  Release the worker for execution,
        Pick up a timestep to request for download,
        Check if that timestep is already on disk,
        repeated until a proper timestep to download is found, and start download */
    void WebFieldlinesWindow::executeDownloadWorker(){
        _worker.downloadWindow(_window.triggerTimes);
        _worker.updateRFSSourceFiles(*rfs_sourceFiles);
    }
    
    void WebFieldlinesWindow::newWindow(double time){
        // Find where in the list we are
        auto it = std::find_if(_triggerTimesWeb.rbegin(), _triggerTimesWeb.rend(), [time](auto element) {
            if (time > element.first) return true;
        });

        const int index = static_cast<int>(std::distance(it, _triggerTimesWeb.rend())) - 1;

        _window.triggerTimes.clear();
        _window.nTriggerTimes = 0;
        // This should be safe, because in the manager, it is checked wether the current position is within
        // the boundaries with respect to back & forward width
        for(int i = index - _window.backWidth; i <= index + _window.forwardWidth; i++){
            if(i < 0) i = 0;
            _window.triggerTimes.push_back(std::make_pair(_triggerTimesWeb[i].first, _triggerTimesWeb[i].second));
            _window.nTriggerTimes++;
        }
        LERROR("new window");
        _worker.newWindowToDownload();
    }
    
    bool WebFieldlinesWindow::timeIsInTriggerTimesWebList(double time){
        if(_nAvailableWeb == 0) return false;

        if(time >= (_triggerTimesWeb.front().first) && time <= (_triggerTimesWeb.back().first))
            return true;
        else 
            return false;
    }
    
    void WebFieldlinesWindow::getNewTriggerTimesWebList(double time){

        _worker.getRangeOfAvailableTriggerTimes(time, time, _triggerTimesWeb);
        _nAvailableWeb = static_cast<int>(_triggerTimesWeb.size());
    }


    bool WebFieldlinesWindow::workerWindowIsReady()
    {
        return _worker.windowIsComplete();
    }

    bool WebFieldlinesWindow::expectedWindowIsOutOfBounds(double time) {
        auto resultForwards = std::find_if(_triggerTimesWeb.rbegin(), _triggerTimesWeb.rbegin() + _window.forwardWidth, [time](auto pair) {
            return time > pair.first;
        });

        auto resultBackwards = std::find_if(_triggerTimesWeb.begin(), _triggerTimesWeb.begin() + _window.backWidth, [time](auto pair) {
            return time < pair.first;
        });

        return resultForwards != _triggerTimesWeb.rbegin() + _window.forwardWidth || resultBackwards != _triggerTimesWeb.begin() + _window.backWidth;
    }

    void WebFieldlinesWindow::rfsHasUpdated() {
        _worker.flagUpdated();
    }
    
    // -------------------------- PRIVATE FUNCTIONS  -----------------------------------//
    // Returns first trigger of window
    double WebFieldlinesWindow::windowStart(){
        return _window.triggerTimes.front().first;
    }
    // Returns last trigger of window
    double WebFieldlinesWindow::windowEnd(){
        return _window.triggerTimes.back().first;
    }


    
    
    
} // namespace openspace
