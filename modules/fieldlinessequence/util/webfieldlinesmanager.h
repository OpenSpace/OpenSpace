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

#include <vector>
#include <string>

namespace openspace {
    
class WebFieldlinesManager{
public:
    // Constructor
    WebFieldlinesManager() = default;

    // download files specified in _filestodownload
    void downloadFieldlines();

    //---------------- OPERATORS ------------------------- //

    // To replace the constructor, takes the identifier of the field line, is used for storing the field lines mainly
    // Also takes a second parameter containing the name of the field line model used.
    // These may in the future be the same.
    void initializeWebFieldlinesManager(std::string identifier, std::string fieldLineModelType, int& _activeTriggerTimeIndex, size_t& _nStates, std::vector<std::string>& _sourceFiles, std::vector<double>& _startTimes);

    // Returns the sync directory
    std::string getDirectory();

    
private:
    std::string _syncDir;
    
    // What model is this field line derived from, may come to be the same as the identifier
    std::string _flsType;
    
    // How many fieldlines around the current time point
    int _downloadMargin;
    
    // How long between the timesteps?
    double _timeTriggerDelta;
    
    /******************************************************************************
     * Pointers to stuff in RenderableFieldlinesSequence (which own this instance)*
     ******************************************************************************/
    // Active index of _startTimes
    int *rfs_activeTriggerTimeIndex;
    // Number of states in the sequence
    size_t *rfs_nStates;
    // Stores the provided source file paths
    std::vector<std::string> *rfs_sourceFiles;
    // Contains the _triggerTimes for all FieldlineStates in the sequence
    std::vector<double> *rfs_startTimes;
    
    /****************************** End of pointers ******************************/
    
    // List of all triggertimes(field lines states) available for download
    std::vector<std::pair<double, std::string>> _availableTriggertimes;
    
    // Indices for what fieldlines to download
    std::vector<int> _filesToDownload;
    
    // Function to run in FieldLinesSequence's update loop
    void update();
    
    // Download one file, given what model type and triggertime in J2000
    // ***turn into ints later***
    std::string downloadOsfls(std::string triggertime);
    
    std::string initializeSyncDirectory(std::string identifier);

    // Get list of all triggertimes(field lines states) available form the server
    void getAvailableTriggertimes();
    
    // Decide what the first sequence of fieldlines should be
    // (can be empty during start up of openspace)
    void setInitialSet(double openSpaceTime);
    
    // Download a sequence
    void downloadInitialSequence();
    
    // Update the list of osfls available on disk. Should be in sync with
    // _startTimes member var in FieldLinesSequence
    void updateStartTimes();
    
    // Parse the data from http request
    void parseTriggerTimesList(std::string s);
    
    // some temporary functions to translate the filenames to ints (doubles?)
    int triggerTimeString2Int(std::string s);
    void triggerTimeInt2String(int d, std::string& s);
    
};


} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESMANAGER___H__
