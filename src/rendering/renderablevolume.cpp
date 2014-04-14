
// open space includes
#include <openspace/rendering/renderablevolume.h>

#include <ghoul/opengl/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/volumeraycastergl.h>
#include <openspace/rendering/volumeraycastercl.h>
#include <sgct.h>

namespace {
    std::string _loggerCat = "RenderableVolume";
}

namespace openspace {

RenderableVolume::RenderableVolume(const ghoul::Dictionary& dictionary): _volumePath("") {
    
    // get path if available
    std::string path = "";
    if(dictionary.hasKey("Path")) {
       dictionary.getValue("Path", path);
       path += "/";
    }
    
    if(dictionary.hasKey("Volume")) {
        dictionary.getValue("Volume", _volumePath);
        _volumePath = findPath(_volumePath, path);
    }
    
    // parse hints
    ghoul::Dictionary hintsDictionary;
    if(dictionary.hasKey("Hints"))
        dictionary.getValue("Hints", hintsDictionary);
    _hints = readHints(hintsDictionary);
    
    _programUpdateOnSave = false;
    if(dictionary.hasKey("UpdateOnSave")) {
        if(dictionary.getValue("UpdateOnSave", _programUpdateOnSave)) {
        }
    }
    
    ghoul::Dictionary raycasterDictionary;
    raycasterDictionary.setValue("Filepath", _volumePath);
    raycasterDictionary.setValue("Hints", _hints);
    raycasterDictionary.setValue("ProgramUpdateOnSave", _programUpdateOnSave);
    
    if (dictionary.hasKey("Kernel")) {
        std::string kernelPath = "";
        if(dictionary.getValue("Kernel", kernelPath)) {
            kernelPath = findPath(kernelPath, path);
        }
        // opencl
        raycasterDictionary.setValue("Kernel", kernelPath);
        _rayCaster = new VolumeRaycasterCL(raycasterDictionary);
    } else if(dictionary.hasKey("Shaders")){
        
        ghoul::Dictionary shadersDictionary;
        if(dictionary.getValue("Shaders", shadersDictionary)) {
            std::string vertexShaderPath = "";
            std::string fragShaderPath = "";
            shadersDictionary.getValue("VertexShader",  vertexShaderPath);
            shadersDictionary.getValue("FragmentShader",    fragShaderPath);
            
            LDEBUG("vertexShaderPath: " << vertexShaderPath);
            LDEBUG("fragShaderPath: " << fragShaderPath);
            vertexShaderPath = findPath(vertexShaderPath, path);
            fragShaderPath = findPath(fragShaderPath, path);
            
            raycasterDictionary.setValue("VertexShader", vertexShaderPath);
            raycasterDictionary.setValue("FragmentShader", fragShaderPath);
        }
        
        // glsl
        _rayCaster = new VolumeRaycasterGL(raycasterDictionary);
    }
    
}

RenderableVolume::~RenderableVolume() {
    deinitialize();
}

bool RenderableVolume::initialize() {
    return _rayCaster->initialize();
}

bool RenderableVolume::deinitialize() {
    if(_rayCaster)
        delete _rayCaster;
    
    return true;
}

void RenderableVolume::render(const Camera *camera, const psc &thisPosition) {

	// check so that the raycaster is set
	assert(_rayCaster);
    
	float speed = 50.0f;
	float time = sgct::Engine::getTime();
    glm::mat4 transform = camera->getViewProjectionMatrix();
    
    double factor = pow(10.0,thisPosition[3]);
    transform = glm::translate(transform, glm::vec3(thisPosition[0]*factor, thisPosition[1]*factor, thisPosition[2]*factor));
	transform = glm::rotate(transform, time*speed, glm::vec3(0.0f, 1.0f, 0.0f));
	_rayCaster->render(transform);
}

void RenderableVolume::update() {

}

std::string RenderableVolume::findPath(const std::string& path, const std::string& relativePath) {
    std::string tmp = absPath(path);
    if(FileSys.fileExists(tmp))
        return tmp;
    
    tmp = absPath(relativePath + path);
    if(FileSys.fileExists(tmp))
        return tmp;
    
    LERROR("Could not find file '" << path << "'");
    
    return "";
}

ghoul::RawVolumeReader::ReadHints RenderableVolume::readHints(const ghoul::Dictionary& dictionary) {
    ghoul::RawVolumeReader::ReadHints hints;
    hints._dimensions = glm::ivec3(1, 1, 1);
	hints._format = ghoul::opengl::Texture::Format::Red;
	hints._internalFormat = GL_R8;
    
    // parse hints
    double tempValue;
    if (dictionary.hasKey("Dimensions.1") && dictionary.getValue("Dimensions.1", tempValue)) {
        int intVal = static_cast<int>(tempValue);
        if(intVal > 0)
            hints._dimensions[0] = intVal;
    }
    if (dictionary.hasKey("Dimensions.2") && dictionary.getValue("Dimensions.2", tempValue)) {
        int intVal = static_cast<int>(tempValue);
        if(intVal > 0)
            hints._dimensions[1] = intVal;
    }
    if (dictionary.hasKey("Dimensions.3") && dictionary.getValue("Dimensions.3", tempValue)) {
        int intVal = static_cast<int>(tempValue);
        if(intVal > 0)
            hints._dimensions[2] = intVal;
    }
    
    std::string format;
    if (dictionary.hasKey("Format") && dictionary.getValue("Format", format)) {
        if(format == "RED") {
            hints._format = ghoul::opengl::Texture::Format::Red;
        } else if(format == "RG") {
            hints._format = ghoul::opengl::Texture::Format::RG;
        } else if(format == "RGB") {
            hints._format = ghoul::opengl::Texture::Format::RGB;
        } else if(format == "RGBA") {
            hints._format = ghoul::opengl::Texture::Format::RGBA;
        }
    }
    
    format = "";
    if (dictionary.hasKey("InternalFormat") && dictionary.getValue("InternalFormat", format)) {
        if(format == "R8") {
            hints._internalFormat = GL_R8;
        } else if(format == "RG8") {
            hints._internalFormat = GL_RG8;
        } else if(format == "RGB8") {
            hints._internalFormat = GL_RGB8;
        } else if(format == "RGBA8") {
            hints._internalFormat = GL_RGB8;
        } else if(format == "R32F") {
            hints._internalFormat = GL_R32F;
        } else if(format == "RG32F") {
            hints._internalFormat = GL_RG32F;
        } else if(format == "RGB32F") {
            hints._internalFormat = GL_RGB32F;
        } else if(format == "RGBA32F") {
            hints._internalFormat = GL_RGB32F;
        }
    }
    return hints;
}
	
} // namespace openspace