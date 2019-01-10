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
#ifndef __OPENSPACE_MODULE_DSN___RADECMANAGER___H__
#define __OPENSPACE_MODULE_DSN___RADECMANAGER___H__

#include <ghoul/misc/dictionary.h>
#include <ghoul/logging/logmanager.h>
#include <modules/dsn/managers/datafilehelper.h>
#include <openspace/json.h>
#include <fstream>

namespace openspace {

    class RadecManager {

    public:
       RadecManager();

       struct Position {
           mutable std::string timeStamp;
           mutable double ra;
           mutable double dec;
           mutable double range;
           mutable double lightTravelTime; //Downlink light time travel time in seconds
           mutable double lightTravelHours = 1; //Downlink light time travel time in seconds
       };
       mutable bool isReady = false;

       //Used to determine if we need to search for new data
       mutable int prevFileIndex;
       mutable int prevMinIndex;

       mutable std::vector<Position> positions;
       mutable std::vector<double> minuteTimes;
       mutable Position position; 
       mutable double updateFrequency = 1;
       mutable double activeMinute = -1;
       /* Identifier for object using the translation, used for logging */
       std::string objectIdentifier;
       /*Used to check if the loaded file is still relevant or if we should look for another one. */
       mutable double _checkFileTime;
       /*Time range for the files*/
       mutable double _checkFileEndTime;
       /* A vector with all our datafile paths*/
       std::vector<std::string> _dataFiles;
       /* A vector with all our datafile times in j2000*/
       mutable std::vector<double> timeDoubles;
       /* Extracts all the mandatory information we need from our asset file */
       bool extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary);
       /*gets the correct datafile, that matches the current time in open space*/
       glm::vec3 getPosForTime(double time) const;
       /* parses positioningdata from a file given an index in our preloaded filename vector */
       bool radecParser(int index) const;
       /*Updates what minute the last ra dec positioning data has looked for*/
       void updateActiveMinute(int idx) const;
       /*Find the correct minute in the vector of loaded positions*/
       RadecManager::Position getPositionInVector(int idx) const;
       /*Check if current file in open space is already loaded*/
       bool correctFileInterval(double time) const;
       /*Check if current time interval is still relevant*/
       bool correctUpdateInterval(double time) const;
       /*Update and reate buffer of data so that we can compensate for light travel time without getting out of bounce*/
       void updateRadecData(int index) const;
       /*Sets the update frequency from property*/
       void setUpdateFrequency(double updateFrequency);

    private:
        /* Our light travel positioning compensation, expressed in hours */
      //  mutable int _lightTravelHours = 1;
    };
}

#endif // __OPENSPACE_MODULE_DSN___RADECMANAGER___H__
