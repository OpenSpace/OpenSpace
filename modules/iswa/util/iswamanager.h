/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_ISWA___ISWAMANAGER___H__
#define __OPENSPACE_MODULE_ISWA___ISWAMANAGER___H__

#include <ghoul/designpattern/singleton.h>
#include <ghoul/designpattern/event.h>

#include <memory>
#include <map>
#include <future>
#include <ghoul/glm.h>
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED


#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4619) // #pragma warning: there is no warning number '4675'
#endif // WIN32

#include <ccmc/Kameleon.h>

#ifdef WIN32
#pragma warning (pop)
#endif

#endif


#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/downloadmanager.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/rendering/renderable.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>


namespace openspace {

class IswaBaseGroup;
class IswaCygnet; 

struct CdfInfo {
    std::string name;
    std::string path;
    std::string group;
    std::string date;
    std::string fieldlineSeedsIndexFile;
};

struct CygnetInfo {
    std::string name;
    std::string description;
    int updateInterval;
    bool selected;
};

struct MetadataFuture {
    int id;
    std::string group;
    std::string name;
    std::string json;
    int type;
    int geom;
    bool isFinished;
};


class IswaManager : public ghoul::Singleton<IswaManager>, public properties::PropertyOwner {
    friend class ghoul::Singleton<IswaManager>;

public:
    enum CygnetType {Texture, Data, Kameleon, NoType};
    enum CygnetGeometry {Plane, Sphere};

    IswaManager();
    ~IswaManager();

    void addIswaCygnet(int id, std::string type = "Texture", std::string group = "");
    void addKameleonCdf(std::string group, int pos);
    void createFieldline(std::string name, std::string cdfPath, std::string seedPath);

    std::future<DownloadManager::MemoryFile> fetchImageCygnet(int id, double timestamp);
    std::future<DownloadManager::MemoryFile> fetchDataCygnet(int id, double timestamp);
    std::string iswaUrl(int id, double timestamp = OsEng.timeManager().time().j2000Seconds(), std::string type = "image");

    std::shared_ptr<IswaBaseGroup> iswaGroup(std::string name);
    
    std::map<int, std::shared_ptr<CygnetInfo>>& cygnetInformation();
    std::map<std::string, std::shared_ptr<IswaBaseGroup>>& groups();
    std::map<std::string, std::vector<CdfInfo>>& cdfInformation();

    static scripting::LuaLibrary luaLibrary();

    ghoul::Event<>& iswaEvent(){
        return _iswaEvent;
    }

    void addCdfFiles(std::string path);
    void setBaseUrl(std::string bUrl);
private:
    std::shared_ptr<MetadataFuture> downloadMetadata(int id);
    std::string jsonPlaneToLuaTable(std::shared_ptr<MetadataFuture> data);
    std::string jsonSphereToLuaTable(std::shared_ptr<MetadataFuture> data);
    std::string parseKWToLuaTable(CdfInfo info, std::string cut="z");
    
    void createScreenSpace(int id);
    void createPlane(std::shared_ptr<MetadataFuture> data);
    void createSphere(std::shared_ptr<MetadataFuture> data);
    void createKameleonPlane(CdfInfo info, std::string cut);

    void fillCygnetInfo(std::string jsonString);
    void registerGroup(std::string groupName, std::string type);
    
    std::map<std::string, std::string> _month;
    std::map<int, std::string> _type;
    std::map<int, std::string> _geom;

    std::shared_ptr<ccmc::Kameleon> _kameleon;
    std::set<std::string> _kameleonFrames;

    std::map<std::string, std::shared_ptr<IswaBaseGroup>> _groups;
    std::map<int, std::shared_ptr<CygnetInfo>> _cygnetInformation;
    std::map<std::string, std::vector<CdfInfo>> _cdfInformation;

    ghoul::Event<> _iswaEvent;

    std::string _baseUrl;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___ISWAMANAGER___H__
