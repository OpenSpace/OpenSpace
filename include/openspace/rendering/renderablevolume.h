#ifndef RENDERABLEVOLUME_H
#define RENDERABLEVOLUME_H

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/volumeraycaster.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/io/rawvolumereader.h>

namespace openspace {

class RenderableVolume: public Renderable {
public:

	// constructors & destructor
	RenderableVolume(const ghoul::Dictionary& dictionary);
	~RenderableVolume();
    
    bool initialize();
    bool deinitialize();

	virtual void render(const Camera *camera, const psc& thisPosition);
	virtual void update();
    
protected:
    ghoul::RawVolumeReader::ReadHints readHints(const ghoul::Dictionary& dictionary);
    std::string findPath(const std::string& path, const std::string& relativePath);

private:
    
    // texture
    std::string _volumePath;
    
    // Object
	VolumeRaycaster *_rayCaster;
    ghoul::RawVolumeReader::ReadHints _hints;
    bool _programUpdateOnSave;
};

} // namespace openspace

#endif