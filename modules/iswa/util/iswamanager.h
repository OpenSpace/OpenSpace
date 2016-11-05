/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#ifndef __ISWAMANAGER_H__
#define __ISWAMANAGER_H__

#include <ghoul/designpattern/singleton.h>
#include <ghoul/designpattern/event.h>
#include <modules/iswa/util/luacygnetconverter.h>
#include <map>
#include <future>
#include <ghoul/glm.h>
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
#include <ccmc/Kameleon.h>
#endif
#include <openspace/engine/downloadmanager.h>
#include <openspace/util/time.h>


namespace openspace {
class IswaBaseGroup;
class IswaCygnet; 

/**
 * The info needed to create a kameleonplane
 */
struct CdfInfo {
    std::string name;
    std::string path;
    std::string group;
    std::string date;
    std::string fieldlineSeedsIndexFile; // Path to a file that lists all seedpoints files for this cdf 
};

/**
 * CygnetInfo is the info we get from each
 * cygnet we list in the GUI. This is requested
 * from iSWAs CygnetHealthService.
 */
struct CygnetInfo {
    std::string name;
    std::string description;
    int updateInterval;
    bool selected;
};

/**
 * Metadata provided by OpenSpace and iSWA that is
 * needed to create a cygnet.
 */
struct MetadataFuture {
    int id;
    std::string group;
    std::string name;
    int resourceType;
    int cygnetType;
    std::string json; // Metadata from iSWA
};

class IswaManager : public ghoul::Singleton<IswaManager> { 
public:

    enum CygnetType {TexturePlane, DataPlane, DataSphere, KameleonPlane, NoCygnetType};
    enum ResourceType {Texture, Json, Text, Cdf, NoResourceType};
    enum GeometryType {Plane, Sphere};

    IswaManager();
    ~IswaManager();

    void addIswaCygnet(int id, ResourceType type = ResourceType::Texture, std::string group = "");
    void addKameleonCdf(std::string group, int pos);
    void createFieldline(std::string name, std::string cdfPath, std::string seedPath) const ;

    std::future<DownloadManager::MemoryFile> fetchImageCygnet(int id, double timestamp);
    std::future<DownloadManager::MemoryFile> fetchDataCygnet(int id, double timestamp);
    std::string iswaUrl(int id, double timestamp = Time::ref().j2000Seconds(), std::string type = "image");

    std::shared_ptr<IswaBaseGroup> iswaGroup(std::string name);
    
    std::map<int, std::shared_ptr<CygnetInfo>>& cygnetInformation();
    std::map<std::string, std::shared_ptr<IswaBaseGroup>>& groups();
    std::map<std::string, std::vector<CdfInfo>>& cdfInformation();
    std::string cygnetType(int i);

    static scripting::LuaLibrary luaLibrary();

    ghoul::Event<>& iswaEvent(){
        return _iswaEvent;
    }

    void addCdfFiles(std::string path);
    void setBaseUrl(std::string bUrl);
    void unregisterGroup(std::string groupName);
    void clearGroupBuildData(std::string name);

    bool getResourceType(const std::string& type, ResourceType& enumType);
    bool getCygnetType(const std::string& type, CygnetType& enumType);

private:
    
    void registerGroup(std::string groupName, CygnetType cygnetType, ResourceType resourceType);
    void createScreenSpace(int id);
    void createIswaCygnet(std::shared_ptr<MetadataFuture> metadata);
    void createKameleonPlane(CdfInfo info, std::string cut);
    void fillCygnetInfo(std::string jsonString);

    std::map<std::string, std::string> _month;
    std::map<int, std::string> _resourceType;
    std::map<int, std::string> _geometryType;
    std::map<int, std::string> _cygnetType;

    std::shared_ptr<ccmc::Kameleon> _kameleon;
    std::set<std::string> _kameleonFrames;

    std::map<std::string, std::shared_ptr<IswaBaseGroup>> _groups;
    std::map<int, std::shared_ptr<CygnetInfo>> _cygnetInformation;
    std::map<std::string, std::vector<CdfInfo>> _cdfInformation;

    LuaCygnetConverter _luaConverter;
};

} //namespace openspace
#endif //__ISWAMANAGER_H__