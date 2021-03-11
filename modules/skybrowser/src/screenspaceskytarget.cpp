#include <modules/skybrowser/include/screenspaceskytarget.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/factorymanager.h>
#include <openspace/rendering/helper.h>
#include <ghoul/opengl/textureunit.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/scripting/scriptengine.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/filesystem/filesystem.h>
#include <optional>
#include <ghoul/opengl/openglstatecache.h>
#include <glm/gtx/string_cast.hpp>

#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr const char* _loggerCat = "ScreenSpaceSkyTarget";

    constexpr const std::array<const char*, 4> UniformNames = {
    "Alpha", "ModelTransform", "ViewProjectionMatrix", "texture1"
    };

} //namespace

namespace openspace {
    ScreenSpaceSkyTarget::ScreenSpaceSkyTarget(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
    {
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceSkyTarget";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(identifier);

        std::unique_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(absPath("D:/Ylvas/OpenSpace/modules/skybrowser/target.png"));

        if (texture) {
            // Images don't need to start on 4-byte boundaries, for example if the
            // image is only RGB
            glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

            texture->uploadTexture();
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            texture->purgeFromRAM();

            _texture = std::move(texture);
            _objectSize = _texture->dimensions();
            _scale = 0.1f;
        }
    }

    void ScreenSpaceSkyTarget::bindTexture() {
        if (_texture) {
            _texture->bind();
        }
    }
 
}
