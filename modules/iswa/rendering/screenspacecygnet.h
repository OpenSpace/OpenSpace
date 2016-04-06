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

#ifndef __SCREENSPACECYGNET_H__
#define __SCREENSPACECYGNET_H__

#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/engine/downloadmanager.h>
#include <modules/iswa/util/iswamanager.h>

namespace openspace{

class ScreenSpaceCygnet : public ScreenSpaceRenderable {
public:
	ScreenSpaceCygnet(int cygnetId, std::string path);
	ScreenSpaceCygnet(std::shared_ptr<Metadata> data);
	~ScreenSpaceCygnet();

	void render() override;
	bool initialize() override;
	bool deinitialize() override;
	void update() override;
	bool isReady() const override;

private:
	static int id();
	void updateTexture();
	void loadTexture();

	properties::FloatProperty _updateInterval;

	std::string _path;
	const int _cygnetId;
	float _time;
	float _lastUpdateTime = 0.0f;

	std::shared_ptr<DownloadManager::FileFuture> _futureTexture;
	std::string _memorybuffer;
	
	int _id;	
};

 } // namespace openspace

#endif //__SCREENSPACECYGNET_H__