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
#include <openspace/engine/downloadmanager.h>
#include <ghoul/glm.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/rendering/renderable.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/scripting/scriptengine.h>

// #include <modules/iswa/rendering/iswacygnet.h>
// #include <modules/iswa/rendering/iswagroup.h>


namespace openspace {
class ISWAGroup;
class ISWACygnet; 


struct MetadataFuture {
    int id;
    int group;
    std::string name;
    std::string json;
    int type;
    int geom;
    bool isFinished;
};


class ISWAManager : public ghoul::Singleton<ISWAManager> {
    friend class ghoul::Singleton<ISWAManager>;

public:
    enum CygnetType {Texture, Data, Kameleon};
    enum CygnetGeometry {Plane, Sphere};

    ISWAManager();
    ~ISWAManager();

    void addISWACygnet(std::string info);
    void addISWACygnet(int id, std::string info = "TEXTURE", int group = -1);
    void deleteISWACygnet(std::string);

    // std::shared_ptr<DownloadManager::FileFuture> downloadImage(int, std::string);
    std::shared_ptr<DownloadManager::FileFuture> downloadImageToMemory(int id, std::string& buffer);
    std::shared_ptr<DownloadManager::FileFuture> downloadDataToMemory(int id, std::string& buffer);

    void registerToGroup(int id, ISWACygnet* cygnet, CygnetType type);
    void unregisterFromGroup(int id, ISWACygnet* cygnet);
    void registerOptionsToGroup(int id, const std::vector<properties::SelectionProperty::Option>& options);
    std::shared_ptr<ISWAGroup> iSWAGroup(std::string name);

    static scripting::ScriptEngine::LuaLibrary luaLibrary();

private:
    std::string iSWAurl(int id, std::string type = "image");
    std::shared_ptr<MetadataFuture> downloadMetadata(int id);

    void createScreenSpace(int id);
    void createPlane(std::shared_ptr<MetadataFuture> data);
    std::string parseJSONToLuaTable(std::shared_ptr<MetadataFuture> data);

    void createKameleonPlane(std::string kwPath, int group = -1); 
    std::string parseKWToLuaTable(std::string kwPath, int group);

    std::map<std::string, std::string> _month;
    std::map<int, std::string> _type;
    std::map<int, std::string> _geom;

    std::map<int, std::shared_ptr<ISWAGroup>> _groups;
};

} //namespace openspace
#endif //__ISWAMANAGER_H__