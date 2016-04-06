
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

#ifndef __RENDERABLE_H__
#define __RENDERABLE_H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/util/powerscaledscalar.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>


// Forward declare to minimize dependencies
namespace ghoul {
	namespace opengl {
		class Texture;
	}
	class Dictionary;
}

namespace openspace {

// Forward declare to minimize dependencies

class Camera;
class PowerScaledCoordinate;

class Renderable : public properties::PropertyOwner {
public:
    static Renderable* createFromDictionary(const ghoul::Dictionary& dictionary);

    // constructors & destructor
	Renderable();
    Renderable(const ghoul::Dictionary& dictionary);
    virtual ~Renderable();

    virtual bool initialize() = 0;
    virtual bool deinitialize() = 0;

	virtual bool isReady() const = 0;
	bool isEnabled() const;

    void setBoundingSphere(const PowerScaledScalar& boundingSphere);
    const PowerScaledScalar& getBoundingSphere();

    virtual void render(const RenderData& data);
    virtual void render(const RenderData& data, RendererTasks& rendererTask);
    virtual void update(const UpdateData& data);

	bool isVisible() const;
	
	bool hasTimeInterval();
	bool getInterval(double& start, double& end);
	
	bool hasBody();
	bool getBody(std::string& body);
	void setBody(std::string& body);

    void onEnabledChange(std::function<void(bool)> callback);

    static void setPscUniforms(ghoul::opengl::ProgramObject& program, const Camera& camera, const PowerScaledCoordinate& position);

private:
	properties::BoolProperty _enabled;
    PowerScaledScalar boundingSphere_;
	std::string _startTime;
	std::string _endTime;
	std::string _targetBody;
	bool _hasBody;
	bool _hasTimeInterval;
};

}  // namespace openspace

#endif  // __RENDERABLE_H__
