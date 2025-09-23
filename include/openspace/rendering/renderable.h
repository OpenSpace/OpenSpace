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

#ifndef __OPENSPACE_CORE___RENDERABLE___H__
#define __OPENSPACE_CORE___RENDERABLE___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/fadeable.h>

#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/misc/managedmemoryuniqueptr.h>
#include <string_view>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

class Camera;
class Ellipsoid;
struct RenderData;
struct RendererTasks;
struct SurfacePositionHandle;
struct UpdateData;

namespace documentation { struct Documentation; }

// Unfortunately we can't move this struct into the Renderable until
// https://bugs.llvm.org/show_bug.cgi?id=36684 is fixed
struct RenderableSettings {
    bool automaticallyUpdateRenderBin = true;
    bool shouldUpdateIfDisabled = false;
};

class Renderable : public properties::PropertyOwner, public Fadeable {
public:
    enum class RenderBin : int {
        Background = 1,
        Opaque = 2,
        PreDeferredTransparent = 4,
        Overlay = 8,
        PostDeferredTransparent = 16,
        Sticker = 32
    };

    static ghoul::mm_unique_ptr<Renderable> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    explicit Renderable(const ghoul::Dictionary& dictionary,
        RenderableSettings settings = RenderableSettings());
    virtual ~Renderable() override = default;

    virtual void initialize();
    virtual void initializeGL();
    virtual void deinitialize();
    virtual void deinitializeGL();

    virtual bool isReady() const = 0;
    bool isEnabled() const;
    bool shouldUpdateIfDisabled() const noexcept;

    double boundingSphere() const noexcept;
    double interactionSphere() const noexcept;

    std::string_view typeAsString() const noexcept;

    virtual void update(const UpdateData& data);
    virtual void render(const RenderData& data, RendererTasks& rendererTask);
    virtual void renderSecondary(const RenderData& data, RendererTasks& rendererTask);

    // The 'surface' in this case is the interaction sphere of this renderable. In some
    // cases (i.e., planets) this corresponds directly to the physical surface, but in
    // many cases, models, volumetric data, this will not. Regardless of what the physical
    // representation is, the 'surface' is always the sphere around which interaction is
    // handled
    virtual SurfacePositionHandle calculateSurfacePositionHandle(
        const glm::dvec3& targetModelSpace) const;

    virtual Ellipsoid ellipsoid() const;

    virtual bool renderedWithDesiredData() const;

    RenderBin renderBin() const;
    void setRenderBin(RenderBin bin);
    bool matchesRenderBinMask(int binMask) const noexcept;

    bool matchesSecondaryRenderBin(int binMask) const noexcept;

    bool isVisible() const override;

    void onEnabledChange(std::function<void(bool)> callback);

    static documentation::Documentation Documentation();

protected:
    properties::BoolProperty _enabled;
    properties::StringProperty _renderableType;
    properties::BoolProperty _dimInAtmosphere;

    void setBoundingSphere(double boundingSphere);
    void setInteractionSphere(double interactionSphere);

    void setRenderBinFromOpacity();

    /**
     * Returns the full opacity constructed from the _opacity and _fade property values.
     */
    float opacity() const noexcept override;

    SceneGraphNode* parent() const noexcept;

    bool automaticallyUpdatesRenderBin() const noexcept;
    bool hasOverrideRenderBin() const noexcept;

    RenderBin _renderBin = RenderBin::Opaque;

    // An optional renderbin that renderables can use for certain components, in cases
    // where all parts of the renderable should not be rendered in the same bin
    std::optional<RenderBin> _secondaryRenderBin;

    struct AlternativeTransform {
        std::optional<glm::dvec3> translation = std::nullopt;
        std::optional<glm::dmat3> rotation = std::nullopt;
        std::optional<glm::dvec3> scale = std::nullopt;
    };

    /**
     * Calculates the model transformation matrix with the given data and returns it.
     *
     * \param data The RenderData for the object that the model transformation matrix
     *        should be calculated for
     * \param altTransform An object containing alternative transformations to use instead
     *        of those given in data. The transforms can be translation, rotation and
     *        scale.
     *
     * \return The resulting model transformation matrix in double precision
     */
    glm::dmat4 calcModelTransform(const RenderData& data,
        const AlternativeTransform& altTransform = {
            std::nullopt, std::nullopt, std::nullopt
        }) const;

    /**
     * Calculates the model view transformation matrix with the given data and returns it.
     *
     * \param data The RenderData for the object that the model view transformation matrix
     *        should be calculated for
     * \param modelTransform An alternative model transformation matrix to use. If not
     *        provided the function will calculate a new one.
     * \return The resulting model view transformation matrix in double precision
     */
    glm::dmat4 calcModelViewTransform(const RenderData& data,
        const std::optional<glm::dmat4>& modelTransform = std::nullopt) const;

    /**
     * Calculates the model view projection transformation matrix with the given data and
     * returns it.
     *
     * \param data The RenderData for the object that the model view projection
     *        transformation matrix should be calculated for
     * \param modelTransform An alternative model transformation matrix to use. If not
     *        provided the function will calculate a new one.
     * \return The resulting model view projection transformation matrix in double
     *         precision
     */
    glm::dmat4 calcModelViewProjectionTransform(const RenderData& data,
        const std::optional<glm::dmat4>& modelTransform = std::nullopt) const;

    /**
     * Calculates the model, model view, and the model view projection transformation
     * matricies with the given data and returns them in a tuple object.
     *
     * \param data The RenderData for the object that the transforms should be
     *        calculated for
     * \param altModelTransform An object containing alternative transformations to use
     *        instead of those given in data. The transforms can be translation, rotation
     *        and scale.
     * \return A tuple object containing the resulting model, model view, and the
     *         model view projection transformation matrices
     */
    std::tuple<glm::dmat4, glm::dmat4, glm::dmat4> calcAllTransforms(
        const RenderData& data, const AlternativeTransform& altModelTransform = {
            std::nullopt, std::nullopt, std::nullopt
        }) const;

private:
    void registerUpdateRenderBinFromOpacity();

    double _boundingSphere = 0.0;
    double _interactionSphere = 0.0;
    SceneGraphNode* _parent = nullptr;
    const bool _shouldUpdateIfDisabled = false;
    bool _automaticallyUpdateRenderBin = true;
    bool _hasOverrideRenderBin = false;

    // We only want the SceneGraphNode to be able manipulate the parent, so we don't want
    // to provide a set method for this. Otherwise, anyone might mess around with our
    // parentage and that's no bueno
    friend ghoul::mm_unique_ptr<SceneGraphNode> SceneGraphNode::createFromDictionary(
        const ghoul::Dictionary&);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RENDERABLE___H__
