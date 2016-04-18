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


namespace openspace {
class ISWACygnet;
class ISWAContainer;

struct ExtensionFuture {
	std::string extension;
	bool isFinished;
	int id;
	std::string parent;
};

struct Metadata {
	int id;
	std::string path;
	std::string parent;
	std::string frame;
	glm::vec4 offset;
	glm::vec4 scale;
	std::string scaleVariable;
	std::shared_ptr<KameleonWrapper> kw;
};

struct MetadataFuture {
	int id;
	std::string json;
	bool isFinished;
};


class ISWAManager : public ghoul::Singleton<ISWAManager> {
	friend class ghoul::Singleton<ISWAManager>;

public:

	ISWAManager();
	~ISWAManager();

	std::shared_ptr<ISWACygnet> createISWACygnet(std::shared_ptr<Metadata> metadata);
	void addISWACygnet(std::string info);
	void addISWACygnet(int id, std::string info);
	void deleteISWACygnet(std::string);

	std::shared_ptr<DownloadManager::FileFuture> downloadImage(int, std::string);
	std::shared_ptr<DownloadManager::FileFuture> downloadImageToMemory(int id, std::string& buffer);
	std::shared_ptr<DownloadManager::FileFuture> downloadDataToMemory(int id, std::string& buffer);
	std::shared_ptr<ExtensionFuture> fileExtension(int);

	void setContainer(ISWAContainer*);
	std::shared_ptr<ISWACygnet> iSWACygnet(std::string);

	void update();

private:
	std::string iSWAurl(int);
	std::string iSWADataUrl(int);
	std::shared_ptr<MetadataFuture> downloadMetadata(int id);
	std::string getDictionaryTable(int id, std::string path);
	std::string parseJSONToLuaTable(int id, std::string json);
	std::string parseKWToLuaTable(std::string kwPath);

	void createDataPlane(std::string kwPath);
	void createTexturePlane(int id, std::string json);
	void createScreenSpace(int id);

	std::map<std::string, std::string> _month;
	ISWAContainer* _container;
	std::vector<std::shared_ptr<ExtensionFuture>> _extFutures;
	std::vector<std::shared_ptr<MetadataFuture>> _metaFutures;
};

} //namespace openspace
#endif //__ISWAMANAGER_H__