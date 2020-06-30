/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

namespace {
    constexpr const char* keyPotentialTargets = "PotentialTargets";

    constexpr const char* keyInstrument = "Instrument.Name";
    constexpr const char* keyInstrumentFovy = "Instrument.Fovy";
    constexpr const char* keyInstrumentAspect = "Instrument.Aspect";

    constexpr const char* keyTranslation = "DataInputTranslation";
    constexpr const char* keyTimesTranslation = "TimesDataInputTranslation";

    constexpr const char* keyProjObserver = "Observer";
    constexpr const char* keyProjTarget = "Target";
    constexpr const char* keyProjAberration = "Aberration";

    constexpr const char* keySequenceDir = "Sequence";
    constexpr const char* keyTimesSequenceDir = "TimesSequence";
    constexpr const char* keySequenceType = "SequenceType";

    constexpr const char* keyNeedsTextureMapDilation = "TextureMap";
    constexpr const char* keyNeedsShadowing = "ShadowMap";
    constexpr const char* keyTextureMapAspectRatio = "AspectRatio";

    constexpr const char* sequenceTypeImage = "image-sequence";
    constexpr const char* sequenceTypePlaybook = "playbook";
    constexpr const char* sequenceTypeHybrid = "hybrid";
    constexpr const char* sequenceTypeInstrumentTimes = "instrument-times";
    constexpr const char* sequenceTypeImageAndInstrumentTimes =
        "image-and-instrument-times";

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
} // namespace

namespace openspace {

documentation::Documentation ProjectionComponent::Documentation() {
    using namespace documentation;
    return {
        "Projection Component",
        "newhorizons_projectioncomponent",
        {
            {
                keySequenceDir,
                new OrVerifier({ new StringVerifier, new StringListVerifier }),
                Optional::Yes,
                "This value specifies one or more directories from which images are "
                "being used for image projections. If the sequence type is set to "
                "'playbook', this value is ignored"
            },
            {
                keyInstrument,
                new StringAnnotationVerifier("A SPICE name of an instrument"),
                Optional::No,
                "The instrument that is used to perform the projections"
            },
            {
                keyInstrumentFovy,
                new DoubleVerifier,
                Optional::No,
                "The field of view in degrees along the y axis"
            },
            {
                keyInstrumentAspect,
                new DoubleVerifier,
                Optional::No,
                "The aspect ratio of the instrument in relation between x and y axis"
            },
            {
                keySequenceType,
                new StringInListVerifier(
                    { sequenceTypeImage, sequenceTypePlaybook, sequenceTypeHybrid,
                      sequenceTypeInstrumentTimes, sequenceTypeImageAndInstrumentTimes }
                ),
                Optional::Yes,
                "This value determines which type of sequencer is used for generating "
                "image schedules. The 'playbook' is using a custom format designed by "
                "the New Horizons team, the 'image-sequence' uses lbl files from a "
                "directory, and the 'hybrid' uses both methods."
            },
            {
                keyProjObserver,
                new StringAnnotationVerifier("A SPICE name of the observing object"),
                Optional::No,
                "The observer that is doing the projection. This has to be a valid SPICE "
                "name or SPICE integer."
            },
            {
                keyProjTarget,
                new StringAnnotationVerifier("A SPICE name of the observed object"),
                Optional::No,
                "The observed object that is projected on. This has to be a valid SPICE "
                "name or SPICE integer."
            },
            {
                keyProjAberration,
                new StringInListVerifier({
                    // from SpiceManager::AberrationCorrection::AberrationCorrection
                    "NONE", "LT", "LT+S", "CN", "CN+S", "XLT", "XLT+S", "XCN", "XCN+S"
                }),
                Optional::No,
                "The aberration correction that is supposed to be used for the "
                "projection. The values for the correction correspond to the SPICE "
                "definition as described in "
                "ftp://naif.jpl.nasa.gov/pub/naif/toolkit_docs/IDL/cspice/spkezr_c.html"
            },
            {
                keyPotentialTargets,
                new StringListVerifier,
                Optional::Yes,
                "The list of potential targets that are involved with the image "
                "projection"
            },
            {
                keyNeedsTextureMapDilation,
                new BoolVerifier,
                Optional::Yes,
                "Determines whether a dilation step of the texture map has to be "
                "performed after each projection. This is necessary if the texture of "
                "the projected object is a texture map where the borders are not "
                "touching. The default value is 'false'."
            },
            {
                keyNeedsShadowing,
                new BoolVerifier,
                Optional::Yes,
                "Determines whether the object requires a self-shadowing algorithm. This "
                "is necessary if the object is concave and might cast a shadow on itself "
                "during presentation. The default value is 'false'."
            },
            {
                keyTextureMapAspectRatio,
                new DoubleVerifier,
                Optional::Yes,
                "Sets the desired aspect ratio of the projected texture. This might be "
                "necessary as planets usually have 2x1 aspect ratios, whereas this does "
                "not hold for non-planet objects (comets, asteroids, etc). The default "
                "value is '1.0'."
            }
        }
    };
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
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ProjectionComponent"
    );
    _instrumentID = dictionary.value<std::string>(keyInstrument);
    _projectorID = dictionary.value<std::string>(keyProjObserver);
    _projecteeID = dictionary.value<std::string>(keyProjTarget);
    _fovy = static_cast<float>(dictionary.value<double>(keyInstrumentFovy));
    _aspectRatio = static_cast<float>(dictionary.value<double>(keyInstrumentAspect));

    _aberration = SpiceManager::AberrationCorrection(
        dictionary.value<std::string>(keyProjAberration)
    );

    if (dictionary.hasKeyAndValue<ghoul::Dictionary>(keyPotentialTargets)) {
        const ghoul::Dictionary& potentialTargets = dictionary.value<ghoul::Dictionary>(
            keyPotentialTargets
        );

        _potentialTargets.reserve(potentialTargets.size());
        for (size_t i = 1; i <= potentialTargets.size(); ++i) {
            _potentialTargets.emplace_back(
                potentialTargets.value<std::string>(std::to_string(i))
            );
        }
    }

    if (dictionary.hasKeyAndValue<bool>(keyNeedsTextureMapDilation)) {
        _dilation.isEnabled = dictionary.value<bool>(keyNeedsTextureMapDilation);
    }

    if (dictionary.hasKeyAndValue<bool>(keyNeedsShadowing)) {
        _shadowing.isEnabled = dictionary.value<bool>(keyNeedsShadowing);
    }

    if (dictionary.hasKeyAndValue<double>(keyTextureMapAspectRatio)) {
        _projectionTextureAspectRatio =
            static_cast<float>(dictionary.value<double>(keyTextureMapAspectRatio));
    }


    if (!dictionary.hasKey(keySequenceDir)) {
        return;
    }

    std::vector<std::string> sequenceSources;
    // Due to the documentation check above it must either be one or the other
    if (dictionary.hasValue<std::string>(keySequenceDir)) {
        sequenceSources.push_back(absPath(dictionary.value<std::string>(keySequenceDir)));
    }
    else {
        ghoul::Dictionary sourcesDict = dictionary.value<ghoul::Dictionary>(
            keySequenceDir
        );
        for (int i = 1; i <= static_cast<int>(sourcesDict.size()); ++i) {
            sequenceSources.push_back(
                absPath(sourcesDict.value<std::string>(std::to_string(i)))
            );
        }
    }

    const std::string& sequenceType = dictionary.value<std::string>(keySequenceType);
    //Important: client must define translation-list in mod file IFF playbook
    if (!dictionary.hasKey(keyTranslation)) {
        LWARNING("No playbook translation provided, spice calls must match playbook!");
        return;
    }

    ghoul::Dictionary translationDictionary;
    dictionary.getValue(keyTranslation, translationDictionary);

    std::vector<std::unique_ptr<SequenceParser>> parsers;
    for (std::string& sequenceSource : sequenceSources) {
        if (sequenceType == sequenceTypePlaybook) {
            parsers.push_back(
                std::make_unique<HongKangParser>(
                    identifier,
                    std::move(sequenceSource),
                    _projectorID,
                    translationDictionary,
                    _potentialTargets
                )
            );
        }
        else if (sequenceType == sequenceTypeImage) {
            parsers.push_back(
                std::make_unique<LabelParser>(
                    identifier,
                    std::move(sequenceSource),
                    translationDictionary
                )
            );
        }
        else if (sequenceType == sequenceTypeHybrid) {
            //first read labels
            parsers.push_back(
                std::make_unique<LabelParser>(
                    identifier,
                    std::move(sequenceSource),
                    translationDictionary
                )
            );

            if (dictionary.hasKey("EventFile")) {
                std::string eventFile = dictionary.value<std::string>("EventFile");
                parsers.push_back(
                    std::make_unique<HongKangParser>(
                        identifier,
                        absPath(eventFile),
                        _projectorID,
                        translationDictionary,
                        _potentialTargets
                    )
                );
            }
            else {
                LWARNING("No eventfile has been provided, please check modfiles");
            }
        }
        else if (sequenceType == sequenceTypeInstrumentTimes) {
            parsers.push_back(
                std::make_unique<InstrumentTimesParser>(
                    identifier,
                    std::move(sequenceSource),
                    translationDictionary
                )
            );
        }
        else if (sequenceType == sequenceTypeImageAndInstrumentTimes) {
            parsers.push_back(
                std::make_unique<LabelParser>(
                    identifier,
                    std::move(sequenceSource),
                    translationDictionary
                    )
            );

            std::string timesSequenceSource = absPath(
                dictionary.value<std::string>(keyTimesSequenceDir)
            );
            ghoul::Dictionary timesTranslationDictionary;
            dictionary.getValue(keyTimesTranslation, timesTranslationDictionary);

            parsers.push_back(
                std::make_unique<InstrumentTimesParser>(
                    identifier,
                    std::move(timesSequenceSource),
                    timesTranslationDictionary
                    )
            );
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
    return std::move(texture);
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
