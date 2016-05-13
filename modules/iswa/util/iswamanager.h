/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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

#include <memory>
#include <map>
#include <future>
#include <ghoul/glm.h>
#include <ccmc/Kameleon.h>
#include <openspace/engine/downloadmanager.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/rendering/renderable.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/properties/selectionproperty.h>

// #include <modules/iswa/rendering/iswacygnet.h>
// #include <modules/iswa/rendering/iswagroup.h>


namespace openspace {
class IswaGroup;
class IswaCygnet; 

struct CygnetInfo {
    std::string name;
    std::string description;
    int updateInterval;
    bool selected;
};

struct MetadataFuture {
    int id;
    int group;
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

    void addIswaCygnet(std::string info);
    void addIswaCygnet(int id, std::string info = "Texture", int group = -1);
    // void deleteIswaCygnet(std::string);

    std::future<DownloadManager::MemoryFile> fetchImageCygnet(int id);
    std::future<DownloadManager::MemoryFile> fetchDataCygnet(int id);
    std::string iswaUrl(int id, std::string type = "image");

    void registerToGroup(int id, CygnetType type, IswaCygnet* cygnet);
    void unregisterFromGroup(int id, IswaCygnet* cygnet);
    void registerOptionsToGroup(int id, const std::vector<properties::SelectionProperty::Option>& options);
    std::shared_ptr<IswaGroup> iswaGroup(std::string name);
    
    std::map<int, std::shared_ptr<CygnetInfo>>& cygnetInformation();
    std::map<int, std::shared_ptr<IswaGroup>>& groups();

    static scripting::ScriptEngine::LuaLibrary luaLibrary();

private:
    std::shared_ptr<MetadataFuture> downloadMetadata(int id);
    std::string parseJSONToLuaTable(std::shared_ptr<MetadataFuture> data);
    
    void createScreenSpace(int id);
    void createPlane(std::shared_ptr<MetadataFuture> data);

    void fillCygnetInfo(std::string jsonString);

    std::map<std::string, std::string> _month;
    std::map<int, std::string> _type;
    std::map<int, std::string> _geom;

    std::shared_ptr<ccmc::Kameleon> _kameleon;
    std::set<std::string> _kameleonFrames;

    std::map<int, std::shared_ptr<IswaGroup>> _groups;
    std::map<int, std::shared_ptr<CygnetInfo>> _cygnetInformation;
};

} //namespace openspace
#endif //__ISWAMANAGER_H__