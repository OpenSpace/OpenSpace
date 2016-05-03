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

#ifndef __ISWACYGNET_H__
#define __ISWACYGNET_H__

#define _USE_MATH_DEFINES
#include <math.h>

#include <memory>
#include <chrono>
#include <openspace/properties/propertyowner.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/scene/scenegraphnode.h>
#include <modules/onscreengui/include/gui.h>
#include <ghoul/opengl/texture.h>
#include <modules/iswa/util/iswamanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <modules/iswa/util/iswamanager.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/transferfunction.h>


namespace openspace{
class ISWAGroup;

struct Metadata {
    int id;
    int groupId;
    int updateTime;
    std::string path;
    std::string parent;
    std::string frame;
    glm::vec3 gridMin;
    glm::vec3 gridMax;
    glm::vec3 offset;
    glm::vec3 scale;
    glm::vec4 spatialScale;
    std::string scaleVariable;
    std::string coordinateType;
};



class ISWACygnet : public Renderable, public std::enable_shared_from_this<ISWACygnet> {
    friend class ISWAGroup;

public:
    ISWACygnet(const ghoul::Dictionary& dictionary);
    ~ISWACygnet();

    virtual bool initialize() = 0;
    virtual bool deinitialize() = 0;

protected:
    void registerProperties();
    void unregisterProperties();
    void initializeTime();
    
    void enabled(bool enabled){_enabled.setValue(enabled);};

    properties::TriggerProperty _delete;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;

    std::shared_ptr<Metadata> _data;
    std::string _memorybuffer;

    glm::dmat3 _stateMatrix;

    double _openSpaceTime;
    double _lastUpdateOpenSpaceTime;

    std::chrono::milliseconds _realTime;
    std::chrono::milliseconds _lastUpdateRealTime;
    int _minRealTimeUpdateInterval;

    std::vector<std::shared_ptr<TransferFunction>> _transferFunctions;

    ISWAManager::CygnetType _type;
};

}//namespace openspace
#endif