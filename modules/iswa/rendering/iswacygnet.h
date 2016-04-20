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

#include <openspace/properties/propertyowner.h>
#include <memory>
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


namespace openspace{
class ISWACygnet : public Renderable{
public:
    // ISWACygnet(std::shared_ptr<Metadata> data);
    ISWACygnet(const ghoul::Dictionary& dictionary);
    ~ISWACygnet();

    virtual bool initialize() = 0;
    virtual bool deinitialize() = 0;

    // virtual void render(const RenderData& data) = 0;
    // virtual void update(const UpdateData& data) = 0;
    // virtual bool isReady() = 0;

    // bool enabled(){return _enabled.value();}

protected:
    // void setPscUniforms(ghoul::opengl::ProgramObject* program, const Camera* camera, const PowerScaledCoordinate& position);
    void registerProperties();
    void unregisterProperties();
    // void setParent();

    // properties::BoolProperty _enabled;
    properties::FloatProperty _updateInterval;
    properties::TriggerProperty _delete;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    std::unique_ptr<ghoul::opengl::Texture> _texture;

    std::shared_ptr<Metadata> _data;
    std::string _memorybuffer;

    glm::dmat3 _stateMatrix;

    double _time;
    double _lastUpdateTime = 0;

    int _id;
};

}//namespace openspace
#endif