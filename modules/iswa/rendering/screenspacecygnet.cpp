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

#include <modules/iswa/rendering/screenspacecygnet.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem>
#include <openspace/util/time.h>
#include <modules/iswa/util/iswamanager.h>

namespace {
    const std::string _loggerCat = "ScreenSpaceCygnet";
}

namespace openspace {

ScreenSpaceCygnet::ScreenSpaceCygnet(const ghoul::Dictionary& dictionary)
    : ScreenSpaceImage(dictionary)
{
    // hacky, have to first get as float and then cast to int.
    float cygnetid;
    dictionary.getValue("CygnetId", cygnetid);
    _cygnetId = (int)cygnetid;
    
    float interval;
    dictionary.getValue("UpdateInterval", interval);
    _updateTime = (int) interval;

    _downloadImage = true;
    _url = IswaManager::ref().iswaUrl(_cygnetId);
        
    _openSpaceTime = Time::ref().currentTime();
    _lastUpdateOpenSpaceTime = _openSpaceTime;

    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;
     _minRealTimeUpdateInterval = 100;

}

ScreenSpaceCygnet::~ScreenSpaceCygnet(){}

void ScreenSpaceCygnet::update(){
    _openSpaceTime = Time::ref().currentTime();
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());

    bool timeToUpdate = (fabs(_openSpaceTime-_lastUpdateOpenSpaceTime) >= _updateTime &&
                        (_realTime.count()-_lastUpdateRealTime.count()) > _minRealTimeUpdateInterval);

    if((Time::ref().timeJumped() || timeToUpdate )){
        _url = IswaManager::ref().iswaUrl(_cygnetId);
        updateTexture();
        _lastUpdateRealTime = _realTime;
        _lastUpdateOpenSpaceTime = _openSpaceTime;
    }

    if(_futureImage.valid() && DownloadManager::futureReady(_futureImage)) {
        loadTexture();
    }
}
}