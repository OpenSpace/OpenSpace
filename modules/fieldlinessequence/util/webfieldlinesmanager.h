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
    WebFieldlinesManager() = default;

    // Constructor
    WebFieldlinesManager(std::string identifier, std::string fieldLineModelType);
    
    // For testing purposes
    void downloadFieldlines(std::vector<std::string>& _sourceFile, std::vector<double>& _startTimes, size_t& _nStates);

    //---------------- OPERATORS ------------------------- //

    // To replace the constructor, takes the identifier of the field line, is used for storing the field lines mainly
    // Also takes a second parameter containing the name of the field line model used.
    // These may in the future be the same.
    void operator ()(std::string identifier, std::string fieldLineModelType);

    // Returns the sync directory
    std::string getDirectory();

    
private:
    
    // Directory for saving this specific typ of field line, named by the identifier
    std::string _syncDir;
    
    // What model is this field line derived from, may come to be the same as the identifier
    std::string _flsType;
    
    // List of all triggertimes(field lines states) available for download
    // ***turn into ints later***
    std::vector<std::string> _availableTriggertimes;
    
    // Function to run in FieldLinesSequence's update loop
    void update();
    
    // Download one file, given what model type and triggertime in J2000
    // ***turn into ints later***
    void downloadOsfls(std::string triggertime);

    // Checks if there is a sync directory for one specific set of field lines
    // If not creates one and returns the string to that directory
    std::string initializeSyncDirectory(std::string identifier);
    
    // Get list of all triggertimes(field lines states) available form the server
    void getAvailableTriggertimes();
    
    // Decide what the first sequence of fieldlines should be
    // (can be empty during start up of openspace)
    void setInitialSet(double openSpaceTime);
    
    // Download a sequence
    void downloadInitialSequence(std::vector<double> triggertimes);
    
    // Update the list of osfls available on disk. Should be in sync with
    // _startTimes member var in FieldLinesSequence
    void updateStartTimes();
    
    // Parse the data from http request
    void parseTriggerTimesList(std::string s);
    
    // some temporary functions to translate the filenames to ints
    void triggerTimeString2Int(std::string s, int& d);
    void triggerTimeInt2String(int d, std::string& s);
    
};


} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESMANAGER___H__
