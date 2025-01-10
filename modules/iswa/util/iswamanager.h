/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/properties/propertyowner.h>

#include <openspace/engine/downloadmanager.h>
#include <ghoul/designpattern/event.h>
#include <future>
#include <set>
#include <string>

namespace ccmc { class Kameleon; }

namespace openspace {

namespace scripting { struct LuaLibrary; }

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

class IswaManager : public properties::PropertyOwner {
public:
    enum CygnetType { Texture, Data, Kameleon, NoType };
    enum CygnetGeometry { Plane, Sphere };

    IswaManager();
    ~IswaManager();

    static void initialize();
    static void deinitialize();
    static bool isInitialized();
    static IswaManager& ref();

    void addIswaCygnet(int id, const std::string& type = "Texture",
        std::string group = "");
    void addKameleonCdf(std::string group, int pos);
    void createFieldline(std::string name, std::filesystem::path cdfPath,
        std::string seedPath);

    std::future<DownloadManager::MemoryFile> fetchImageCygnet(int id, double timestamp);
    std::future<DownloadManager::MemoryFile> fetchDataCygnet(int id, double timestamp);
    std::string iswaUrl(int id, double timestamp, const std::string& type = "image");

    IswaBaseGroup* iswaGroup(const std::string& name);

    std::map<int, std::shared_ptr<CygnetInfo>>& cygnetInformation();
    std::map<std::string, std::shared_ptr<IswaBaseGroup>>& groups();
    std::map<std::string, std::vector<CdfInfo>>& cdfInformation();

    static scripting::LuaLibrary luaLibrary();

    ghoul::Event<>& iswaEvent();

    void addCdfFiles(std::string path);
    void setBaseUrl(std::string bUrl);

private:
    std::shared_ptr<MetadataFuture> downloadMetadata(int id);
    std::string jsonPlaneToLuaTable(MetadataFuture& data);
    std::string jsonSphereToLuaTable(MetadataFuture& data);
    std::string parseKWToLuaTable(const CdfInfo& info, const std::string& cut = "z");

    void createPlane(MetadataFuture& data);
    void createSphere(MetadataFuture& data);
    void createKameleonPlane(CdfInfo info, std::string cut);

    void fillCygnetInfo(std::string jsonString);
    void registerGroup(std::string groupName, std::string type);

    std::map<int, std::string> _type;
    std::map<int, std::string> _geom;

    std::shared_ptr<ccmc::Kameleon> _kameleon;
    std::set<std::string> _kameleonFrames;

    std::map<std::string, std::shared_ptr<IswaBaseGroup>> _groups;
    std::map<int, std::shared_ptr<CygnetInfo>> _cygnetInformation;
    std::map<std::string, std::vector<CdfInfo>> _cdfInformation;

    ghoul::Event<> _iswaEvent;

    std::string _baseUrl;

    static IswaManager* _instance;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___ISWAMANAGER___H__
