/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/spacecraftinstruments/util/projectioncomponent.h>

#include <modules/spacecraftinstruments/util/hongkangparser.h>
#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <modules/spacecraftinstruments/util/instrumenttimesparser.h>
#include <modules/spacecraftinstruments/util/labelparser.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/textureconversion.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <optional>
#include <variant>

namespace {
    constexpr const char* keyTranslation = "DataInputTranslation";
    constexpr const char* keyTimesTranslation = "TimesDataInputTranslation";
    constexpr const char* placeholderFile = "${DATA}/placeholder.png";

    constexpr const char* _loggerCat = "ProjectionComponent";

    constexpr openspace::properties::Property::PropertyInfo ProjectionInfo = {
        "PerformProjection",
        "Perform Projections",
        "If this value is enabled, this ProjectionComponent will perform projections. If "
        "it is disabled, projections will be ignored."
    };

    constexpr openspace::properties::Property::PropertyInfo ClearProjectionInfo = {
        "ClearAllProjections",
        "Clear Projections",
        "If this property is triggered, it will remove all the projections that have "
        "already been applied."
    };

    constexpr openspace::properties::Property::PropertyInfo FadingInfo = {
        "ProjectionFading",
        "Projection Fading",
        "This value fades the previously performed projections in or out. If this value "
        "is equal to '1', the projections are fully visible, if the value is equal to "
        "'0', the performed projections are completely invisible."
    };

    constexpr openspace::properties::Property::PropertyInfo TextureSizeInfo = {
        "TextureSize",
        "Texture Size",
        "This value determines the size of the texture into which the images are "
        "projected and thus provides the limit to the resolution of projections that can "
        "be applied. Changing this value will not cause the texture to be automatically "
        "updated, but triggering the 'ApplyTextureSize' property is required."
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyTextureSizeInfo = {
        "ApplyTextureSize",
        "Apply Texture Size",
        "Triggering this property applies a new size to the underlying projection "
        "texture. The old texture is resized and interpolated to fit the new size."
    };

    struct [[codegen::Dictionary(ProjectionComponent)]] Parameters {
        // This value specifies one or more directories from which images are being used
        // for image projections. If the sequence type is set to 'playbook', this value is
        // ignored
        std::optional<std::variant<std::string, std::vector<std::string>>> sequence;

        struct Instrument {
            // The instrument that is used to perform the projections
            std::string name [[codegen::annotation("A SPICE name of an instrument")]];

            // The field of view in degrees along the y axis
            float fovy;

            // The aspect ratio of the instrument in relation between x and y axis
            float aspect;
        };
        Instrument instrument;

        enum class Type {
            ImageSequence [[codegen::key("image-sequence")]],
            Playbook [[codegen::key("playbook")]],
            Hybrid [[codegen::key("hybrid")]],
            InstrumentTimes [[codegen::key("instrument-times")]],
            ImageAndInstrumentTimes [[codegen::key("image-and-instrument-times")]]
        };
        // This value determines which type of sequencer is used for generating image 
        // schedules. The 'playbook' is using a custom format designed by the New Horizons 
        // team, the 'image-sequence' uses lbl files from a directory, and the 'hybrid'
        // uses both methods
        std::optional<Type> sequenceType;

        std::optional<std::string> eventFile;

        // The observer that is doing the projection. This has to be a valid SPICE name
        // or SPICE integer
        std::string observer
            [[codegen::annotation("A SPICE name of the observing object")]];

        std::optional<std::string> timesSequence;

        // The observed object that is projected on. This has to be a valid SPICE name or
        // SPICE integer
        std::string target [[codegen::annotation("A SPICE name of the observed object")]];

        // The aberration correction that is supposed to be used for the projection. The 
        // values for the correction correspond to the SPICE definition as described in
        // ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/cspice/spkezr_c.html
        std::string aberration [[codegen::inlist("NONE", "LT", "LT+S", "CN", "CN+S",
            "XLT", "XLT+S", "XCN", "XCN+S")]];

        // The list of potential targets that are involved with the image projection
        std::optional<std::vector<std::string>> potentialTargets;

        // Determines whether the object requires a self-shadowing algorithm. This is 
        // necessary if the object is concave and might cast a shadow on itself during 
        // presentation. The default value is 'false'
        std::optional<bool> textureMap;

        // Determines whether the object requires a self-shadowing algorithm. This is 
        // necessary if the object is concave and might cast a shadow on itself during 
        // presentation. The default value is 'false'
        std::optional<bool> shadowMap;

        // Sets the desired aspect ratio of the projected texture. This might be necessary 
        // as planets usually have 2x1 aspect ratios, whereas this does not hold for 
        // non-planet objects (comets, asteroids, etc). The default value is '1.0'
        std::optional<float> aspectRatio;
    };
#include "projectioncomponent_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ProjectionComponent::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "newhorizons_projectioncomponent";
    return doc;
}

ProjectionComponent::ProjectionComponent()
    : properties::PropertyOwner({ "ProjectionComponent" })
    , _performProjection(ProjectionInfo, true)
    , _clearAllProjections(ClearProjectionInfo, false)
    , _projectionFading(FadingInfo, 1.f, 0.f, 1.f)
    , _textureSize(TextureSizeInfo, glm::ivec2(16), glm::ivec2(16), glm::ivec2(32768))
    , _applyTextureSize(ApplyTextureSizeInfo)
{
    addProperty(_performProjection);
    addProperty(_clearAllProjections);
    addProperty(_projectionFading);

    addProperty(_textureSize);
    addProperty(_applyTextureSize);
    _applyTextureSize.onChange([this]() { _textureSizeDirty = true; });
}

void ProjectionComponent::initialize(const std::string& identifier,
                                     const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _instrumentID = p.instrument.name;
    _projectorID = p.observer;
    _projecteeID = p.target;
    _fovy = p.instrument.fovy;
    _aspectRatio = p.instrument.aspect;

    _aberration = SpiceManager::AberrationCorrection(p.aberration);

    _potentialTargets = p.potentialTargets.value_or(_potentialTargets);
    _dilation.isEnabled = p.textureMap.value_or(_dilation.isEnabled);
    _shadowing.isEnabled = p.shadowMap.value_or(_shadowing.isEnabled);
    _projectionTextureAspectRatio = p.aspectRatio.value_or(_projectionTextureAspectRatio);


    if (!p.sequence.has_value()) {
        // we are done here, the rest only applies if we do have a sequence
        return;
    }

    std::variant<std::string, std::vector<std::string>> sequence = *p.sequence;

    std::vector<std::string> sequenceSources;
    if (std::holds_alternative<std::string>(sequence)) {
        sequenceSources.push_back(absPath(std::get<std::string>(sequence)));
    }
    else {
        ghoul_assert(
            std::holds_alternative<std::vector<std::string>>(sequence),
            "Something is wrong with the generated documentation"
        );
        sequenceSources = std::get<std::vector<std::string>>(sequence);
        for (std::string& s : sequenceSources) {
            s = absPath(s);
        }
    }


    if (!p.sequenceType.has_value()) {
        throw ghoul::RuntimeError("Missing SequenceType");
    }

    ghoul::Dictionary translationDictionary;
    if (dictionary.hasValue<ghoul::Dictionary>(keyTranslation)) {
        translationDictionary = dictionary.value<ghoul::Dictionary>(keyTranslation);
    }
    else {
        LWARNING("No playbook translation provided, spice calls must match playbook!");
        return;
    }

    std::vector<std::unique_ptr<SequenceParser>> parsers;
    for (std::string& sequenceSource : sequenceSources) {
        switch (*p.sequenceType) {
            case Parameters::Type::Playbook:
                parsers.push_back(
                    std::make_unique<HongKangParser>(
                        identifier,
                        std::move(sequenceSource),
                        _projectorID,
                        translationDictionary,
                        _potentialTargets
                    )
                );
                break;
            case Parameters::Type::ImageSequence:
                parsers.push_back(
                    std::make_unique<LabelParser>(
                        identifier,
                        std::move(sequenceSource),
                        translationDictionary
                    )
                );
                break;
            case Parameters::Type::Hybrid:
                // first read labels
                parsers.push_back(
                    std::make_unique<LabelParser>(
                        identifier,
                        std::move(sequenceSource),
                        translationDictionary
                    )
                );

                if (p.eventFile.has_value()) {
                    parsers.push_back(
                        std::make_unique<HongKangParser>(
                            identifier,
                            absPath(*p.eventFile),
                            _projectorID,
                            translationDictionary,
                            _potentialTargets
                        )
                    );
                }
                else {
                    LWARNING("No eventfile has been provided, please check modfiles");
                }
                break;
            case Parameters::Type::InstrumentTimes:
                parsers.push_back(
                    std::make_unique<InstrumentTimesParser>(
                        identifier,
                        std::move(sequenceSource),
                        translationDictionary
                    )
                );
                break;
            case Parameters::Type::ImageAndInstrumentTimes:
            {
                parsers.push_back(
                    std::make_unique<LabelParser>(
                        identifier,
                        std::move(sequenceSource),
                        translationDictionary
                    )
                );

                if (!p.timesSequence.has_value()) {
                    throw ghoul::RuntimeError("Could not find required TimesSequence");
                }
                ghoul::Dictionary timesTranslationDictionary;
                if (dictionary.hasValue<ghoul::Dictionary>(keyTimesTranslation)) {
                    timesTranslationDictionary =
                        dictionary.value<ghoul::Dictionary>(keyTimesTranslation);
                }

                parsers.push_back(
                    std::make_unique<InstrumentTimesParser>(
                        identifier,
                        absPath(*p.timesSequence),
                        timesTranslationDictionary
                    )
                );
                break;
            }
        }
    }

    for (std::unique_ptr<SequenceParser>& parser : parsers) {
        bool success = parser->create();
        if (!success) {
            LERROR("One or more sequence loads failed; please check mod files");
        }
        else {
            ImageSequencer::ref().runSequenceParser(*parser);
        }
    }
    parsers.clear();
}

bool ProjectionComponent::initializeGL() {
    int maxSize = OpenGLCap.max2DTextureSize();

    glm::ivec2 size;
    if (_projectionTextureAspectRatio > 1.f) {
        size.x = maxSize;
        size.y = static_cast<int>(maxSize / _projectionTextureAspectRatio);
    }
    else {
        size.x = static_cast<int>(maxSize * _projectionTextureAspectRatio);
        size.y = maxSize;
    }

    _textureSize.setMaxValue(size);
    _textureSize = size / 2;

    // We only want to use half the resolution per default:
    size /= 2;

    bool success = generateProjectionLayerTexture(size);
    success &= generateDepthTexture(size);
    success &= auxiliaryRendertarget();
    success &= depthRendertarget();

    using std::unique_ptr;
    using ghoul::opengl::Texture;
    using ghoul::io::TextureReader;

    unique_ptr<Texture> texture = TextureReader::ref().loadTexture(
        absPath(placeholderFile)
    );
    if (texture) {
        texture->uploadTexture();
        texture->setFilter(Texture::FilterMode::LinearMipMap);
        texture->setWrapping(Texture::WrappingMode::ClampToBorder);
    }
    _placeholderTexture = std::move(texture);

    if (_dilation.isEnabled) {
        _dilation.program = ghoul::opengl::ProgramObject::Build(
            "Dilation",
            absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/dilation_vs.glsl"),
            absPath("${MODULE_SPACECRAFTINSTRUMENTS}/shaders/dilation_fs.glsl")
        );

        const GLfloat plane[] = {
            -1, -1,
            1,  1,
            -1,  1,
            -1, -1,
            1, -1,
            1,  1,
        };

        glGenVertexArrays(1, &_dilation.vao);
        glGenBuffers(1, &_dilation.vbo);

        glBindVertexArray(_dilation.vao);
        glBindBuffer(GL_ARRAY_BUFFER, _dilation.vbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(plane), plane, GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(
            0,
            2,
            GL_FLOAT,
            GL_FALSE,
            sizeof(GLfloat) * 2,
            nullptr
        );

        glBindVertexArray(0);
    }

    return success;
}

bool ProjectionComponent::deinitialize() {
    _projectionTexture = nullptr;

    glDeleteFramebuffers(1, &_fboID);

    if (_dilation.isEnabled) {
        glDeleteFramebuffers(1, &_dilation.fbo);
        glDeleteVertexArrays(1, &_dilation.vao);
        glDeleteBuffers(1, &_dilation.vbo);

        _dilation.program = nullptr;
        _dilation.texture = nullptr;
    }

    return true;
}

bool ProjectionComponent::isReady() const {
    return (_projectionTexture != nullptr);
}

void ProjectionComponent::imageProjectBegin() {
    // keep handle to the current bound FBO
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);

    if (_textureSizeDirty) {
        LDEBUG(
            fmt::format(
                "Changing texture size to {}", ghoul::to_string(_textureSize.value())
            )
        );

        // If the texture size has changed, we have to allocate new memory and copy
        // the image texture to the new target

        using ghoul::opengl::Texture;
        using ghoul::opengl::FramebufferObject;

        // Make a copy of the old textures
        std::unique_ptr<Texture> oldProjectionTexture = std::move(_projectionTexture);
        std::unique_ptr<Texture> oldDilationStencil = std::move(_dilation.stencilTexture);
        std::unique_ptr<Texture> oldDilationTexture = std::move(_dilation.texture);
        std::unique_ptr<Texture> oldDepthTexture = std::move(_shadowing.texture);

        // Generate the new textures
        generateProjectionLayerTexture(_textureSize);

        if (_shadowing.isEnabled) {
            generateDepthTexture(_textureSize);
        }

        auto copyFramebuffers = [](Texture* src, Texture* dst, const std::string& msg) {
            glFramebufferTexture(
                GL_READ_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT0,
                *src,
                0
            );

            GLenum status = glCheckFramebufferStatus(GL_READ_FRAMEBUFFER);
            if (!FramebufferObject::errorChecking(status).empty()) {
                LERROR(fmt::format(
                    "Read Buffer ({}): {}",
                    msg,
                    FramebufferObject::errorChecking(status)
                ));
            }

            glFramebufferTexture(
                GL_DRAW_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT0,
                *dst,
                0
            );

            status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
            if (!FramebufferObject::errorChecking(status).empty()) {
                LERROR(fmt::format(
                    "Draw Buffer ({}): {}",
                    msg,
                    FramebufferObject::errorChecking(status)
                ));
            }

            glBlitFramebuffer(
                0, 0,
                src->dimensions().x, src->dimensions().y,
                0, 0,
                dst->dimensions().x, dst->dimensions().y,
                GL_COLOR_BUFFER_BIT,
                GL_LINEAR
            );
        };

        auto copyDepthBuffer = [](Texture* src, Texture* dst, const std::string& msg) {
            glFramebufferTexture(
                GL_READ_FRAMEBUFFER,
                GL_DEPTH_ATTACHMENT,
                *src,
                0
            );

            GLenum status = glCheckFramebufferStatus(GL_READ_FRAMEBUFFER);
            if (!FramebufferObject::errorChecking(status).empty()) {
                LERROR(fmt::format(
                    "Read Buffer ({}): {}",
                    msg,
                    FramebufferObject::errorChecking(status)
                ));
            }

            glFramebufferTexture(
                GL_DRAW_FRAMEBUFFER,
                GL_DEPTH_ATTACHMENT,
                *dst,
                0
            );

            status = glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
            if (!FramebufferObject::errorChecking(status).empty()) {
                LERROR(fmt::format(
                    "Draw Buffer ({}): {}",
                    msg,
                    FramebufferObject::errorChecking(status)
                ));
            }

            glBlitFramebuffer(
                0, 0,
                src->dimensions().x, src->dimensions().y,
                0, 0,
                dst->dimensions().x, dst->dimensions().y,
                GL_DEPTH_BUFFER_BIT,
                GL_NEAREST
            );
        };

        GLuint fbos[2];
        glGenFramebuffers(2, fbos);
        glBindFramebuffer(GL_READ_FRAMEBUFFER, fbos[0]);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fbos[1]);

        copyFramebuffers(
            oldProjectionTexture.get(),
            _projectionTexture.get(),
            "Projection"
        );

        if (_dilation.isEnabled) {
            copyFramebuffers(
                oldDilationStencil.get(),
                _dilation.stencilTexture.get(),
                "Dilation Stencil"
            );

            copyFramebuffers(
                oldDilationTexture.get(),
                _dilation.texture.get(),
                "Dilation Texture"
            );
        }

        if (_shadowing.isEnabled) {
            copyDepthBuffer(
                oldDepthTexture.get(),
                _shadowing.texture.get(),
                "Shadowing"
            );
        }

        glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
        glDeleteFramebuffers(2, fbos);

        glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D,
            *_projectionTexture,
            0
        );

        if (_dilation.isEnabled) {
            // We only need the stencil texture if we need to dilate
            glFramebufferTexture2D(
                GL_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT1,
                GL_TEXTURE_2D,
                *_dilation.stencilTexture,
                0
            );

            glBindFramebuffer(GL_FRAMEBUFFER, _dilation.fbo);
            glFramebufferTexture2D(
                GL_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT0,
                GL_TEXTURE_2D,
                *_dilation.texture,
                0
            );
        }

        if (_shadowing.isEnabled) {
            glBindFramebuffer(GL_FRAMEBUFFER, _depthFboID);
            glFramebufferTexture2D(
                GL_FRAMEBUFFER,
                GL_DEPTH_ATTACHMENT,
                GL_TEXTURE_2D,
                *_shadowing.texture,
                0
            );
        }

        _textureSizeDirty = false;
    }

    glGetIntegerv(GL_VIEWPORT, _viewport);
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);

    glViewport(
        0, 0,
        static_cast<GLsizei>(_projectionTexture->width()),
        static_cast<GLsizei>(_projectionTexture->height())
    );

    if (_dilation.isEnabled) {
        GLenum buffers[] = { GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        glDrawBuffers(2, buffers);
    }
}

bool ProjectionComponent::needsShadowMap() const {
    return _shadowing.isEnabled;
}

ghoul::opengl::Texture& ProjectionComponent::depthTexture() {
    return *_shadowing.texture;
}

void ProjectionComponent::depthMapRenderBegin() {
    ghoul_assert(_shadowing.isEnabled, "Shadowing is not enabled");

    // keep handle to the current bound FBO
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &_defaultFBO);
    glGetIntegerv(GL_VIEWPORT, _viewport);

    glBindFramebuffer(GL_FRAMEBUFFER, _depthFboID);
    glEnable(GL_DEPTH_TEST);

    glViewport(
        0, 0,
        static_cast<GLsizei>(_shadowing.texture->width()),
        static_cast<GLsizei>(_shadowing.texture->height())
        );

    glClear(GL_DEPTH_BUFFER_BIT);
}

void ProjectionComponent::depthMapRenderEnd() {
    glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
    glViewport(_viewport[0], _viewport[1], _viewport[2], _viewport[3]);
}

void ProjectionComponent::imageProjectEnd() {
    if (_dilation.isEnabled) {
        glBindFramebuffer(GL_FRAMEBUFFER, _dilation.fbo);

        glDisable(GL_BLEND);

        ghoul::opengl::TextureUnit unit[2];
        unit[0].activate();
        _projectionTexture->bind();

        unit[1].activate();
        _dilation.stencilTexture->bind();

        _dilation.program->activate();
        _dilation.program->setUniform("tex", unit[0]);
        _dilation.program->setUniform("stencil", unit[1]);

        glBindVertexArray(_dilation.vao);
        glDrawArrays(GL_TRIANGLES, 0, 6);

        _dilation.program->deactivate();

        glEnable(GL_BLEND);
    }

    glBindFramebuffer(GL_FRAMEBUFFER, _defaultFBO);
    glViewport(_viewport[0], _viewport[1], _viewport[2], _viewport[3]);

    _mipMapDirty = true;
}

void ProjectionComponent::update() {
    if (_dilation.isEnabled && _dilation.program->isDirty()) {
        _dilation.program->rebuildFromFile();
    }
}

bool ProjectionComponent::depthRendertarget() {
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    // setup FBO
    glGenFramebuffers(1, &_depthFboID);
    glBindFramebuffer(GL_FRAMEBUFFER, _depthFboID);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        GL_TEXTURE_2D,
        *_shadowing.texture,
        0);

    glDrawBuffer(GL_NONE);

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        return false;
    }

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    return true;
}

bool ProjectionComponent::auxiliaryRendertarget() {
    bool completeSuccess = true;

    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    // setup FBO
    glGenFramebuffers(1, &_fboID);
    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *_projectionTexture,
        0
    );
    // check FBO status
    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Main Framebuffer incomplete");
        completeSuccess &= false;
    }


    if (_dilation.isEnabled) {
        // We only need the stencil texture if we need to dilate
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT1,
            GL_TEXTURE_2D,
            *_dilation.stencilTexture,
            0
        );

        // check FBO status
        status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Main Framebuffer incomplete");
            completeSuccess &= false;
        }

        glGenFramebuffers(1, &_dilation.fbo);
        glBindFramebuffer(GL_FRAMEBUFFER, _dilation.fbo);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D,
            *_dilation.texture,
            0
        );

        // check FBO status
        status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Dilation Framebuffer incomplete");
            completeSuccess &= false;
        }
    }

    // switch back to window-system-provided framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

    return completeSuccess;
}

glm::mat4 ProjectionComponent::computeProjectorMatrix(const glm::vec3 loc, glm::dvec3 aim,
                                                      const glm::vec3 up,
                                                      const glm::dmat3& instrumentMatrix,
                                                      float fieldOfViewY,
                                                      float aspectRatio,
                                                      float nearPlane, float farPlane,
                                                      glm::vec3& boreSight)
{

    //rotate boresight into correct alignment
    boreSight = instrumentMatrix*aim;
    glm::vec3 uptmp(instrumentMatrix*glm::dvec3(up));

    // create view matrix
    glm::vec3 e3 = glm::normalize(-boreSight);
    glm::vec3 e1 = glm::normalize(glm::cross(uptmp, e3));
    glm::vec3 e2 = glm::normalize(glm::cross(e3, e1));

    glm::mat4 projViewMatrix = glm::mat4(
        e1.x, e2.x, e3.x, 0.f,
        e1.y, e2.y, e3.y, 0.f,
        e1.z, e2.z, e3.z, 0.f,
        glm::dot(e1, -loc), glm::dot(e2, -loc), glm::dot(e3, -loc), 1.f
    );
    // create perspective projection matrix
    glm::mat4 projProjectionMatrix = glm::perspective(
        glm::radians(fieldOfViewY), aspectRatio, nearPlane, farPlane
    );

    return projProjectionMatrix*projViewMatrix;
}

bool ProjectionComponent::doesPerformProjection() const {
    return _performProjection;
}

bool ProjectionComponent::needsClearProjection() const {
    return _clearAllProjections;
}

bool ProjectionComponent::needsMipMapGeneration() const {
    return _mipMapDirty;
}

float ProjectionComponent::projectionFading() const {
    return _projectionFading;
}

ghoul::opengl::Texture& ProjectionComponent::projectionTexture() const {
    if (_dilation.isEnabled) {
        return *_dilation.texture;
    }
    else {
        return *_projectionTexture;
    }
}

std::string ProjectionComponent::projectorId() const {
    return _projectorID;
}

std::string ProjectionComponent::projecteeId() const {
    return _projecteeID;
}

std::string ProjectionComponent::instrumentId() const {
    return _instrumentID;
}

SpiceManager::AberrationCorrection ProjectionComponent::aberration() const {
    return _aberration;
}

float ProjectionComponent::fieldOfViewY() const {
    return _fovy;
}

float ProjectionComponent::aspectRatio() const {
    return _aspectRatio;
}

void ProjectionComponent::clearAllProjections() {
    // keep handle to the current bound FBO
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);
    //counter = 0;
    glViewport(
        0,
        0,
        static_cast<GLsizei>(_projectionTexture->width()),
        static_cast<GLsizei>(_projectionTexture->height())
    );

    glBindFramebuffer(GL_FRAMEBUFFER, _fboID);

    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);

    if (_dilation.isEnabled) {
        glBindFramebuffer(GL_FRAMEBUFFER, _dilation.fbo);
        glClear(GL_COLOR_BUFFER_BIT);
    }

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(m_viewport[0], m_viewport[1],
               m_viewport[2], m_viewport[3]);

    _clearAllProjections = false;
    _mipMapDirty = true;
}

void ProjectionComponent::generateMipMap() {

    _projectionTexture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
    _mipMapDirty = false;
}

std::shared_ptr<ghoul::opengl::Texture> ProjectionComponent::loadProjectionTexture(
                                                           const std::string& texturePath,
                                                           bool isPlaceholder)
{
    using std::unique_ptr;
    using ghoul::opengl::Texture;
    using ghoul::io::TextureReader;


    if (isPlaceholder) {
        return _placeholderTexture;
    }


    unique_ptr<Texture> texture = TextureReader::ref().loadTexture(absPath(texturePath));
    if (texture) {
        if (texture->format() == Texture::Format::Red) {
            ghoul::opengl::convertTextureFormat(*texture, Texture::Format::RGB);
        }
        texture->uploadTexture();
        texture->setWrapping(
            { Texture::WrappingMode::Repeat, Texture::WrappingMode::MirroredRepeat }
        );
        texture->setFilter(Texture::FilterMode::LinearMipMap);
    }
    return texture;
}

bool ProjectionComponent::generateProjectionLayerTexture(const glm::ivec2& size) {
    LINFO(fmt::format("Creating projection texture of size '{}, {}'", size.x, size.y));

    using namespace ghoul::opengl;
    _projectionTexture = std::make_unique<Texture>(
        glm::uvec3(size, 1),
        Texture::Format::RGBA
    );
    if (_projectionTexture) {
        _projectionTexture->uploadTexture();
    }

    if (_dilation.isEnabled) {
        _dilation.texture = std::make_unique<ghoul::opengl::Texture>(
            glm::uvec3(size, 1),
            ghoul::opengl::Texture::Format::RGBA
        );

        if (_dilation.texture) {
            _dilation.texture->uploadTexture();
            //_dilation.texture->setFilter(
            //    ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
            //);
        }

        _dilation.stencilTexture = std::make_unique<ghoul::opengl::Texture>(
            glm::uvec3(size, 1),
            ghoul::opengl::Texture::Format::Red,
            // @TODO: Remove the static cast ---abock
            static_cast<GLenum>(ghoul::opengl::Texture::Format::Red)
        );

        if (_dilation.stencilTexture) {
            _dilation.stencilTexture->uploadTexture();
            //_dilation.texture->setFilter(
            //    ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
            //);
        }
    }

    return _projectionTexture != nullptr;
}

bool ProjectionComponent::generateDepthTexture(const glm::ivec2& size) {
    LINFO(fmt::format("Creating depth texture of size '{}, {}'", size.x, size.y));

    _shadowing.texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(size, 1),
        ghoul::opengl::Texture::Format::DepthComponent,
        GL_DEPTH_COMPONENT32F
    );

    if (_shadowing.texture) {
        _shadowing.texture->uploadTexture();
    }

    return _shadowing.texture != nullptr;

}

} // namespace openspace
