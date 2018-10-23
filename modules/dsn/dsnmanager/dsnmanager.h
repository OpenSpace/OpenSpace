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

#include <ext/xml/rapidxml.hpp>
#include <ext/xml/rapidxml_utils.hpp>
#include <openspace/json.h>

#include <modules/dsn/rendering/renderablecommunicationpackage.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>

#include <fstream>

namespace openspace {

    class DsnManager {

    public:
    static struct Signal {        
        std::string dishName ;
        std::string spacecraft;
        std::string direction; 
        std::string type;
        float startTime;
        std::string dataRate;
      };
    static struct DsnData {
        std::vector<Signal> signals;
     };
      static bool extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary);
	  static glm::vec3 approximateSpacecraftPosition(const char* dishId, glm::vec3 dishPos);
      static void fillVertexArray(std::vector<RenderableCommunicationPackage::PackageVBOLayout> &vertexArray); 

    private:
       static void readDataFromXml(std::vector<std::string> _dataFiles);
       static void readDataFromJson(std::vector<std::string> _dataFiles);
       static void xmlParser(std::string filename, std::ofstream &logfile);
       static void jsonParser(std::string filename, std::ofstream &logfile);
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
