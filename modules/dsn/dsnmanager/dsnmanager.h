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
#ifndef __OPENSPACE_MODULE_DSN___DSNMANAGER___H__
#define __OPENSPACE_MODULE_DSN___DSNMANAGER___H__

#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/json.h>

#include <modules/dsn/rendering/renderablecommunicationpackage.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <fstream>

namespace openspace {

    class DsnManager {

    public:

    static struct Signal {        
        int signalId;
        std::string dishName;
        int year;
        std::string spacecraft;
        std::string direction; 
        std::string startTime;
        std::string endTime;
        glm::vec3 color;

      };

    static struct DsnData {
        //filename is on the format of YYYY-DDDT (excluding '.json')
        bool isLoaded = false;
        double sequenceStartTime;
        double sequenceEndTime = sequenceStartTime + 86400.0; // 24 hours from startTime 
        std::vector<Signal> signals;
        //std::vector<double> signalStartTimes;
     };

      /* The data that is currently loaded into open space*/
      static DsnData _dsnData;
      /* A vector with all start times for our datafiles*/
      static std::vector<double> _fileStartTimes;
      /* A vector with all our datafile paths*/
      static std::vector<std::string> _dataFiles;

      /* Extracts all the mandatory information we need from our asset file */
      static bool extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary);
      /* Approximates a spacecraft position using XXXXX */
      static glm::vec3 approximateSpacecraftPosition(const char* dishId, glm::vec3 dishPos);
      /* Saves all filenames in a vector to be compared current time in open space */
      static void extractTriggerTimesFromFileNames(std::vector<std::string> _dataFiles);
      /* parses data from a file given an index in our preloaded filename vector */
      static bool jsonParser(int index);

    private:
	   static double deg2rad(double degrees);

	   /**  Converts a Range, Azimuth, Elevation location to South East Zenith coordinates**/    
	   static void convertRaeToSez(double siteLLA[], double rae[], double sez[]);
	   /**  Converts a Range, Azimuth, Elevation location to Earth centered Earth fixed coordinates**/
	   static void convertRaeToEcef(double observerLLA[], double observerXyz[], double objectRae[], double objectEcef[]);
	   /**  Converts South East Zenith location to Earth centered Earth fixed coordinates**/
	   static void convertSezToEcef(double observerLla[], double observerXyz[], double objectSez[], double objectEcef[]);

    };

}


#endif 
