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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESMANAGER___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESMANAGER___H__

#include <modules/fieldlinessequence/util/webfieldlineswindow.h>
#include <vector>
#include <string>

namespace openspace {

class WebFieldlinesManager{
public:
    // Constructor
    WebFieldlinesManager() = default;

    // To replace the constructor, takes the identifier of the field line, is used for storing the field lines mainly
    // Also takes a second parameter containing the name of the field line model used.
    // These may in the future be the same.
    void initializeWebFieldlinesManager(std::string identifier, std::string url, std::vector<std::string>& _sourceFiles, std::vector<double>& _startTimes);
    
    std::string initializeSyncDirectory(std::string identifier);
    
    // Temporary function - this should be moved to the worker. It's to download
    // the start lines if the directory is empty or launching for the first time
    void preDownload(std::string dUrl);

    // Returns the sync directory
    std::string getDirectory();
    
    // Function to run in FieldLinesSequence's update loop
    void update();

    // Returns wether the worker has finished downloading a window.
    bool checkIfWindowIsReadyToLoad();

	void resetWorker();

    bool hasUpdated = false;

    bool notifyUpdate = false;

private:

    // Flag wether the manager is properly connected
    bool _connected = false;

    std::string _syncDir;
        
    // The datastructure for managing the interval of fieldline sets to be downloaded
    WebFieldlinesWindow _webFieldlinesWindow;
    
};


} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESMANAGER___H__
