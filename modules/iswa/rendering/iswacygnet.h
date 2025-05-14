/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/rendering/renderable.h>

#include <openspace/engine/downloadmanager.h>
#include <openspace/properties/misc/triggerproperty.h>
#include <openspace/rendering/transferfunction.h>
#include <ghoul/glm.h>
#include <chrono>
#include <future>
#include <string>

namespace openspace {

class IswaBaseGroup;
class TransferFunction;

class IswaCygnet : public Renderable {

public:
    explicit IswaCygnet(const ghoul::Dictionary& dictionary);
    virtual ~IswaCygnet();

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

protected:
    struct Metadata {
        int id = -1;
        int updateTime = -1;
        std::string groupName;
        std::string path;
        std::string parent;
        std::string frame;
        glm::vec3 gridMin = glm::vec3(0.f);
        glm::vec3 gridMax = glm::vec3(0.f);
        glm::vec3 offset = glm::vec3(0.f);
        glm::vec3 scale = glm::vec3(0.f);
        glm::vec4 spatialScale = glm::vec4(0.f);
        std::string scaleVariable;
        std::string coordinateType;
    };

    void enabled(bool enabled);

    /**
     * Registers the properties that are equal in all IswaCygnets regardless of being part
     * of a group or not.
     */
    virtual void registerProperties();
    virtual void unregisterProperties();
    void initializeTime();
    void initializeGroup();

    // Subclass interface
    // ==================
    virtual bool createGeometry() = 0;
    virtual bool destroyGeometry() = 0;
    virtual void renderGeometry() const = 0;

    /**
     * Should create a new texture and populate the _textures vector.
     *
     * \return `true` if update was successful
     */
    virtual bool updateTexture() = 0;

    /**
     * Is called before updateTexture. For IswaCygnets getting data from a HTTP request,
     * this function should get the dataFile from the future object.
     *
     * \return `true` if update was successful
     */
    virtual bool updateTextureResource() = 0;

    /**
     * Should send a HTTP request to get the resource it needs to create a texture. For
     * Texture cygnets, this should be an image. For DataCygnets, this should be the data
     * file.
     *
     * \return `true` if update was successful
     */
    virtual bool downloadTextureResource(double timestamp) = 0;

    virtual bool readyToRender() const = 0;

    /**
     * Should set all uniforms needed to render.
     */
    virtual void setUniforms() = 0;

    properties::FloatProperty _alpha;
    properties::TriggerProperty _delete;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shader;
    std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;
    bool _textureDirty = false;

    std::vector<TransferFunction> _transferFunctions;
    std::future<DownloadManager::MemoryFile> _futureObject;

    IswaBaseGroup* _group = nullptr;

    Metadata _data;

    // to rotate objects with flipped texture coordinates
    glm::mat4 _rotation = glm::mat4(1.f);

private:
    glm::dmat3 _stateMatrix = glm::dmat3(1.0);

    double _openSpaceTime = 0.0;
    double _lastUpdateOpenSpaceTime = 0.0;

    std::chrono::milliseconds _realTime;
    std::chrono::milliseconds _lastUpdateRealTime;
    int _minRealTimeUpdateInterval;
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___ISWACYGNET___H__
