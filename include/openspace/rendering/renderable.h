/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_CORE___RENDERABLE___H__
#define __OPENSPACE_CORE___RENDERABLE___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/misc/managedmemoryuniqueptr.h>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

struct RenderData;
struct UpdateData;
struct RendererTasks;
struct SurfacePositionHandle;

namespace documentation { struct Documentation; }

class Camera;

class Renderable : public properties::PropertyOwner {
public:
    enum class RenderBin : int {
        Background = 1,
        Opaque = 2,
        PreDeferredTransparent = 4,
        PostDeferredTransparent = 8,
        Overlay = 16
    };

    static ghoul::mm_unique_ptr<Renderable> createFromDictionary(
        ghoul::Dictionary dictionary);

    Renderable(const ghoul::Dictionary& dictionary);
    virtual ~Renderable() = default;

    virtual void initialize();
    virtual void initializeGL();
    virtual void deinitialize();
    virtual void deinitializeGL();

    virtual bool isReady() const = 0;
    bool isEnabled() const;
    bool shouldUpdateIfDisabled() const;

    double boundingSphere() const;
    double interactionSphere() const;

    virtual void render(const RenderData& data, RendererTasks& rendererTask);
    virtual void update(const UpdateData& data);

    // The 'surface' in this case is the interaction sphere of this renderable. In some
    // cases (i.e., planets) this corresponds directly to the physical surface, but in
    // many cases, models, volumetric data, this will not. Regardless of what the physical
    // representation is, the 'surface' is always the sphere around which interaction is
    // handled
    virtual SurfacePositionHandle calculateSurfacePositionHandle(
                                                const glm::dvec3& targetModelSpace) const;

    virtual bool renderedWithDesiredData() const;

    RenderBin renderBin() const;
    void setRenderBin(RenderBin bin);
    bool matchesRenderBinMask(int binMask);

    bool isVisible() const;

    void onEnabledChange(std::function<void(bool)> callback);

    static documentation::Documentation Documentation();

protected:
    properties::BoolProperty _enabled;
    properties::FloatProperty _opacity;
    properties::StringProperty _renderableType;

    void setBoundingSphere(double boundingSphere);
    void setInteractionSphere(double interactionSphere);

    void setRenderBinFromOpacity();
    void registerUpdateRenderBinFromOpacity();

    double _boundingSphere = 0.0;
    double _interactionSphere = 0.0;
    SceneGraphNode* _parent = nullptr;
    bool _shouldUpdateIfDisabled = false;
    RenderBin _renderBin = RenderBin::Opaque;

private:
    // We only want the SceneGraphNode to be able manipulate the parent, so we don't want
    // to provide a set method for this. Otherwise, anyone might mess around with our
    // parentage and that's no bueno
    friend ghoul::mm_unique_ptr<SceneGraphNode> SceneGraphNode::createFromDictionary(
        const ghoul::Dictionary&);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RENDERABLE___H__
