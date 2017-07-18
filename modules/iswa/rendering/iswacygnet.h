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

#ifndef __OPENSPACE_MODULE_ISWA___ISWACYGNET___H__
#define __OPENSPACE_MODULE_ISWA___ISWACYGNET___H__

#include <chrono>
#include <modules/iswa/util/iswamanager.h>
#include <ghoul/designpattern/event.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/misc/dictionary.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/transferfunction.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>

#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/rendering/iswadatagroup.h>
#include <modules/iswa/rendering/iswakameleongroup.h>

namespace openspace {

class IswaBaseGroup;

struct Metadata {
    int id;
    int updateTime;
    std::string groupName;
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


class IswaCygnet : public Renderable, public std::enable_shared_from_this<IswaCygnet> {

public:
    IswaCygnet(const ghoul::Dictionary& dictionary);
    ~IswaCygnet();

    virtual bool initialize();
    virtual bool deinitialize();
    virtual bool isReady() const;
    void render(const RenderData& data, RendererTasks& rendererTask);
    void update(const UpdateData& data);

protected:

    void enabled(bool enabled){_enabled.setValue(enabled);};

    /**
     * Registers the properties that are equal in all IswaCygnets
     * regardless of being part of a group or not
     */
    void registerProperties();
    void unregisterProperties();
    void initializeTime();
    void initializeGroup();
    /**
     * Creates the shader program. Concrete IswaCygnets must set 
     * _vsPath, _fsPath and _programName before this function in called.
     * @return true if successful creation
     */
    bool createShader();
    
    // Subclass interface
    // ==================
    virtual bool createGeometry() = 0;
    virtual bool destroyGeometry() = 0;
    virtual void renderGeometry() const = 0;

    /**
     * Should create a new texture and populate the _textures vector
     * @return true if update was successfull
     */
    virtual bool updateTexture() = 0;
    /**
     * Is called before updateTexture. For IswaCygnets getting data
     * from a http request, this function should get the dataFile
     * from the future object.
     * @return true if update was successfull
     */
    virtual bool updateTextureResource() = 0;
    /**
     * should send a http request to get the resource it needs to create 
     * a texture. For Texture cygnets, this should be an image. For DataCygnets,
     * this should be the data file.
     * @return true if update was successfull
     */
    virtual bool downloadTextureResource(double timestamp = OsEng.timeManager().time().j2000Seconds()) = 0;
    virtual bool readyToRender() const = 0;
     /**
     * should set all uniforms needed to render
     */
    virtual void setUniforms() = 0;

    properties::FloatProperty _alpha;
    properties::TriggerProperty _delete;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;

    std::shared_ptr<Metadata> _data;

    std::vector<std::shared_ptr<TransferFunction>> _transferFunctions;
    std::future<DownloadManager::MemoryFile> _futureObject;

    std::shared_ptr<IswaBaseGroup> _group;

    bool _textureDirty;

    // Must be set by children.
    std::string _vsPath;
    std::string _fsPath;
    std::string _programName;

    glm::mat4 _rotation; //to rotate objects with fliped texture coordniates
private:
    bool destroyShader();
    glm::dmat3 _stateMatrix;

    double _openSpaceTime;
    double _lastUpdateOpenSpaceTime;

    std::chrono::milliseconds _realTime;
    std::chrono::milliseconds _lastUpdateRealTime;
    int _minRealTimeUpdateInterval;

};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___ISWACYGNET___H__
