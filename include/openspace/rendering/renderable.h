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

#ifndef __OPENSPACE_CORE___RENDERABLE___H__
#define __OPENSPACE_CORE___RENDERABLE___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/scalar/boolproperty.h>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
}

namespace openspace {

struct RenderData;
struct UpdateData;
struct RendererTasks;
struct SurfacePositionHandle;

namespace documentation { struct Documentation; } 

// Forward declare to minimize dependencies

class Camera;
class PowerScaledCoordinate;

class Renderable : public properties::PropertyOwner {
public:
    enum class RenderBin : int {
        Background = 1,
        Opaque = 2,
        Transparent = 4,
        Overlay = 8
    };

    static std::unique_ptr<Renderable> createFromDictionary(const ghoul::Dictionary& dictionary);

    // constructors & destructor
    Renderable();
    Renderable(const ghoul::Dictionary& dictionary);
    virtual ~Renderable();

    virtual bool initialize() = 0;
    virtual bool deinitialize() = 0;

    virtual bool isReady() const = 0;
    bool isEnabled() const;

    void setBoundingSphere(float boundingSphere);
    float boundingSphere() const;

    virtual void render(const RenderData& data, RendererTasks& rendererTask);
    virtual void update(const UpdateData& data);
    virtual SurfacePositionHandle calculateSurfacePositionHandle(
                                                      const glm::dvec3& targetModelSpace);

    RenderBin renderBin() const;
    void setRenderBin(RenderBin bin);
    bool matchesRenderBinMask(int binMask);

    bool isVisible() const;
    
    bool hasTimeInterval();
    bool getInterval(double& start, double& end);
    
    void onEnabledChange(std::function<void(bool)> callback);

    static void setPscUniforms(ghoul::opengl::ProgramObject& program, const Camera& camera, const PowerScaledCoordinate& position);

    static documentation::Documentation Documentation();

protected:
    properties::BoolProperty _enabled;
    
private:
    RenderBin _renderBin;
    float _boundingSphere;
    std::string _startTime;
    std::string _endTime;
    bool _hasTimeInterval;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___RENDERABLE___H__
