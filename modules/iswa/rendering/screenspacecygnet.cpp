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
    , _updateInterval("updateInterval", "Update Interval", 1.0, 0.0 , 10.0)
{
    // hacky, have to first get as float and then cast to int.
    float cygnetid;
    dictionary.getValue("CygnetId", cygnetid);
    _cygnetId = (int)cygnetid;
    
    // setName("iSWACygnet" + std::to_string(_cygnetId));
    addProperty(_updateInterval);

    _downloadImage = true;

    _url = ISWAManager::ref().iSWAurl(_cygnetId);
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;
}

ScreenSpaceCygnet::~ScreenSpaceCygnet(){}

void ScreenSpaceCygnet::update(){
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    int updateInterval = (int) (_updateInterval.value()*1000);
    bool timeToUpdate = ((_realTime.count()-_lastUpdateRealTime.count()) > updateInterval) &&
                        (Time::ref().deltaTime() != 0);

    if(updateInterval != 0 && (Time::ref().timeJumped() || timeToUpdate )){
        _url = ISWAManager::ref().iSWAurl(_cygnetId);
        updateTexture();
        _lastUpdateRealTime = _realTime;
    }

    if(_futureTexture && _futureTexture->isFinished){
        loadTexture();
        _futureTexture = nullptr;
    }
}

}