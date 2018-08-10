/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2018                                                               *
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

#include <modules/space/rendering/renderablestars.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <array>
#include <cstdint>
#include <fstream>
#include <stdint.h>
#include <limits>

//#define USING_STELLAR_TEST_GRID

namespace {
    constexpr const char* _loggerCat = "RenderableStars";

    constexpr const char* KeyFile = "File";

    constexpr const std::array<const char*, 17> UniformNamesSpencer = {
        "renderingMethod", "psfMethod",
        // New Method
        "modelMatrix", "cameraUp", "cameraViewProjectionMatrix",
        "colorOption", "magnitudeExponent", "colorContribution", 
        "eyePosition",
        "psfParamConf", "lumCent", "radiusCent", "brightnessCent",
        "p0Param", "p1Param", "p2Param", "alphaConst"
    };

    constexpr const std::array<const char*, 3> UniformNamesOld = {
        // Textured
        "colorTexture", "alphaValue", "psfTexture"
    };

    constexpr const std::array<const char*, 2> UniformNamesMoffat = {
        // Moffat
        "FWHM", "betaConstant"
    };

    constexpr int8_t CurrentCacheVersion = 1;

    struct ColorVBOLayout {
        std::array<float, 4> position; // (x,y,z,e)

        float bvColor; // B-V color value
        float luminance;
        float absoluteMagnitude;
    };

    struct VelocityVBOLayout {
        std::array<float, 4> position; // (x,y,z,e)

        float bvColor; // B-V color value
        float luminance;
        float absoluteMagnitude;

        float vx; // v_x
        float vy; // v_y
        float vz; // v_z
    };

    struct SpeedVBOLayout {
        std::array<float, 4> position; // (x,y,z,e)

        float bvColor; // B-V color value
        float luminance;
        float absoluteMagnitude;

        float speed;
    };

    static const openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "ColorBV Texture",
        "The path to the texture that is used to convert from the B-V value of the star "
        "to its color. The texture is used as a one dimensional lookup function."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which quantity is used for determining the color of the "
        "stars."
    };

    // Old Method
    constexpr openspace::properties::Property::PropertyInfo PsfTextureInfo = {
        "Texture",
        "Point Spread Function Texture",
        "The path to the texture that should be used as a point spread function for the "
        "stars."
    };

    constexpr openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all stars."
    };

    // PSF
    constexpr openspace::properties::Property::PropertyInfo MagnitudeExponentInfo = {
        "MagnitudeExponent",
        "Magnitude Exponent",
        "Adjust star magnitude by 10^MagnitudeExponent. "
        "Stars closer than this distance are given full opacity. "
        "Farther away, stars dim proportionally to the logarithm of their distance."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorContributionInfo = {
        "ColorContribution",
        "Color Contribution",
        "Adjust the color intensity of the stars"
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOptio",
        "Render Option",
        "Debug option for different rendering methods for the stars."
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo OldMethodOptionInfo = {
        "OldMethodOption",
        "Old Method",
        ""
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo PSFParametersOwnerInfo = {
        "PSFParametersOwner",
        "Parameters",
        ""
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo MoffatMethodOptionInfo = {
        "MoffatMethodOption",
        "Moffat Method",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo PSFMethodOptionInfo = {
        "PSFMethodOptionInfo",
        "PSF Method Option",
        "Debug option for PSF main function: Spencer or Moffat."
    };

    constexpr openspace::properties::Property::PropertyInfo PSFMultiplyOptionInfo = {
        "SizeOption",
        "Size Composition Option",
        "Debug option for the base star's size."
    };

    constexpr openspace::properties::Property::PropertyInfo LumPercentInfo = {
        "LumPercent",
        "Luminosity Contribution",
        "Luminosity Contribution."        
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusPercentInfo = {
        "RadiusPercent",
        "Radius Contribution",
        "Radius Contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo BrightnessPercentInfo = {
        "BrightnessPercen",
        "App Brightness Contribution",
        "App Brightness Contribution."
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo SpencerPSFParamOwnerInfo = {
        "SpencerPSFParamOwner",
        "Spencer PSF Paramameters",
        "PSF parameters for Spencer"
    };

    constexpr openspace::properties::Property::PropertyInfo P0ParamInfo = {
        "P0Param",
        "P0",
        "P0 parameter contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo P1ParamInfo = {
        "P1Param",
        "P1",
        "P1 parameter contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo P2ParamInfo = {
        "P2Param",
        "P2",
        "P2 parameter contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo AlphaConstInfo = {
        "AlphaConst",
        "Alpha",
        "Empirical Alpha Constant."
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo MoffatPSFParamOwnerInfo = {
        "MoffatPSFParam",
        "Moffat PSF Parameters",
        "PSF parameters for Moffat"
    };

    constexpr openspace::properties::Property::PropertyInfo FWHMInfo = {
        "FWHM",
        "FWHM",
        "Moffat's FWHM"
    };

    constexpr openspace::properties::Property::PropertyInfo BetaInfo = {
        "Beta",
        "Beta",
        "Moffat's Beta Constant."
    };
}  // namespace

namespace openspace {

    documentation::Documentation RenderableStars::Documentation() {
        using namespace documentation;
        return {
            "RenderableStars",
            "space_renderablestars",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableStars"),
                Optional::No
            },
            {
                KeyFile,
                new StringVerifier,
                Optional::No,
                "The path to the SPECK file that contains information about the stars "
                "being rendered."
            },
            {
                ColorTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                ColorTextureInfo.description
            },
            {
                ColorOptionInfo.identifier,
                new StringInListVerifier({
                    "Color", "Velocity", "Speed"
                }),
                Optional::Yes,
                ColorOptionInfo.description
            },
            {
                MagnitudeExponentInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                MagnitudeExponentInfo.description
            },
            {
                ColorContributionInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ColorContributionInfo.description
            },
        }
        };
    }

    RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _colorTexturePath(ColorTextureInfo)
        , _colorTexture(nullptr)
        , _colorTextureIsDirty(true)
        , _colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
        , _dataIsDirty(true)

        // Old Method
        , _pointSpreadFunctionTexturePath(PsfTextureInfo)
        , _pointSpreadFunctionTexture(nullptr)
        , _pointSpreadFunctionTextureIsDirty(true)
        , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
        // PSF
        , _psfMethodOption(PSFMethodOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
        , _psfMultiplyOption(PSFMultiplyOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
        , _lumCent(LumPercentInfo, 0.5f, 0.f, 3.f)
        , _radiusCent(RadiusPercentInfo, 0.5f, 0.f, 3.f)
        , _brightnessCent(BrightnessPercentInfo, 0.5f, 0.f, 3.f)
        , _magnitudeExponent(MagnitudeExponentInfo, 4.f, 0.f, 8.f)
        , _colorContribution(ColorContributionInfo, 2.f, 0.f, 10.f)
        , _spencerPSFParamOwner(SpencerPSFParamOwnerInfo)
        , _p0Param(P0ParamInfo, 0.384f, 0.f, 1.f)
        , _p1Param(P1ParamInfo, 0.478f, 0.f, 1.f)
        , _p2Param(P2ParamInfo, 0.138f, 0.f, 1.f)
        , _spencerAlphaConst(AlphaConstInfo, 0.02f, 0.000001f, 5.f)
        , _moffatPSFParamOwner(MoffatPSFParamOwnerInfo)
        , _FWHMConst(FWHMInfo, 10.4f, 0.f, 1000.f)
        , _moffatBetaConst(BetaInfo, 4.765f, 0.f, 10.f)

        , _renderingMethodOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
        , _oldMethodOwner(OldMethodOptionInfo)
        , _psfParamOwner(PSFParametersOwnerInfo)
        , _moffatMethodOwner(MoffatMethodOptionInfo)
        , _program(nullptr)
        , _speckFile("")
        , _nValuesPerStar(0)
        , _vao(0)
        , _vbo(0)
    {
        using File = ghoul::filesystem::File;

        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableStars"
        );

        _colorTexturePath = absPath(dictionary.value<std::string>(
            ColorTextureInfo.identifier
            ));
        _colorTextureFile = std::make_unique<File>(_colorTexturePath);

        _speckFile = absPath(dictionary.value<std::string>(KeyFile));

        _colorOption.addOptions({
            { ColorOption::Color, "Color" },
            { ColorOption::Velocity, "Velocity" },
            { ColorOption::Speed, "Speed" }
            });
        if (dictionary.hasKey(ColorOptionInfo.identifier)) {
            const std::string colorOption = dictionary.value<std::string>(
                ColorOptionInfo.identifier
                );
            if (colorOption == "Color") {
                _colorOption = ColorOption::Color;
            }
            else if (colorOption == "Velocity") {
                _colorOption = ColorOption::Velocity;
            }
            else {
                _colorOption = ColorOption::Speed;
            }
        }
        _colorOption.onChange([&] { _dataIsDirty = true; });
        addProperty(_colorOption);

        _colorTexturePath.onChange([&] { _colorTextureIsDirty = true; });
        _colorTextureFile->setCallback(
            [&](const File&) { _colorTextureIsDirty = true; }
        );
        addProperty(_colorTexturePath);

        // DEBUG GUI for Carter:
        _renderingMethodOption.addOption(0, "Point Spread Function Based");
        _renderingMethodOption.addOption(1, "Textured Based");
        addProperty(_renderingMethodOption);
        _renderingMethodOption.set(0);        

        // Old Method
        _pointSpreadFunctionTexturePath = absPath(dictionary.value<std::string>(
            PsfTextureInfo.identifier
            ));
        _pointSpreadFunctionFile = std::make_unique<File>(_pointSpreadFunctionTexturePath);
        _pointSpreadFunctionTexturePath.onChange(
            [&] { _pointSpreadFunctionTextureIsDirty = true; }
        );
        _pointSpreadFunctionFile->setCallback(
            [&](const File&) { _pointSpreadFunctionTextureIsDirty = true; }
        );
        _oldMethodOwner.addProperty(_pointSpreadFunctionTexturePath);
        
        if (dictionary.hasKey(TransparencyInfo.identifier)) {
            _alphaValue = static_cast<float>(
                dictionary.value<double>(TransparencyInfo.identifier)
                );
        }
        _oldMethodOwner.addProperty(_alphaValue);

        // PSF based
        _psfMethodOption.addOption(0, "Spencer's Function");
        _psfMethodOption.addOption(1, "Moffat's Function");
        _psfMethodOption.set(0);
        _psfParamOwner.addProperty(_psfMethodOption);
        _psfMultiplyOption.addOption(0, "Use Star's Apparent Brightness");
        _psfMultiplyOption.addOption(1, "Use Star's Luminosity and Size");
        _psfMultiplyOption.addOption(2, "Luminosity, Size, App Brightness");
        _psfMultiplyOption.addOption(3, "Absolute Magnitude");
        _psfMultiplyOption.addOption(4, "Apparent Magnitude");
        _psfMultiplyOption.addOption(5, "Old Magical Scale");
        _psfMultiplyOption.set(1);
        _psfParamOwner.addProperty(_psfMultiplyOption);
        _psfParamOwner.addProperty(_lumCent);
        _psfParamOwner.addProperty(_radiusCent);
        _psfParamOwner.addProperty(_brightnessCent);

        if (dictionary.hasKey(MagnitudeExponentInfo.identifier)) {
            _magnitudeExponent = static_cast<float>(
                dictionary.value<double>(MagnitudeExponentInfo.identifier)
                );
        }
        _psfParamOwner.addProperty(_magnitudeExponent);

        if (dictionary.hasKey(ColorContributionInfo.identifier)) {
            _colorContribution = static_cast<float>(
                dictionary.value<double>(ColorContributionInfo.identifier)
                );
        }
        _psfParamOwner.addProperty(_colorContribution);

        _spencerPSFParamOwner.addProperty(_p0Param);
        _spencerPSFParamOwner.addProperty(_p1Param);
        _spencerPSFParamOwner.addProperty(_p2Param);
        _spencerPSFParamOwner.addProperty(_spencerAlphaConst);

        _moffatPSFParamOwner.addProperty(_FWHMConst);
        _moffatPSFParamOwner.addProperty(_moffatBetaConst);
        
        _psfParamOwner.addPropertySubOwner(_spencerPSFParamOwner);
        _psfParamOwner.addPropertySubOwner(_moffatPSFParamOwner);


        // DEBUG GUI for Carter:
        this->addPropertySubOwner(_oldMethodOwner);
        this->addPropertySubOwner(_psfParamOwner);
        this->addPropertySubOwner(_moffatMethodOwner);
    }

    RenderableStars::~RenderableStars() {}

    bool RenderableStars::isReady() const {
        return (_program != nullptr) && (!_fullData.empty());
    }

    void RenderableStars::initializeGL() {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _program = renderEngine.buildRenderProgram("Star",
            absPath("${MODULE_SPACE}/shaders/star_vs.glsl"),
            absPath("${MODULE_SPACE}/shaders/star_fs.glsl"),
            absPath("${MODULE_SPACE}/shaders/star_ge.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_program, _uniformCacheSpencer, UniformNamesSpencer);
        ghoul::opengl::updateUniformLocations(*_program, _uniformCacheOld, UniformNamesOld);
        ghoul::opengl::updateUniformLocations(*_program, _uniformCacheMoffat, UniformNamesMoffat);

        bool success = loadData();
        if (!success) {
            throw ghoul::RuntimeError("Error loading data");
        }
    }

    void RenderableStars::deinitializeGL() {
        glDeleteBuffers(1, &_vbo);
        _vbo = 0;
        glDeleteVertexArrays(1, &_vao);
        _vao = 0;

        _colorTexture = nullptr;

        RenderEngine& renderEngine = OsEng.renderEngine();
        if (_program) {
            renderEngine.removeRenderProgram(_program.get());
            _program = nullptr;
        }
    }

    void RenderableStars::render(const RenderData& data, RendererTasks&) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        glDepthMask(false);

        _program->activate();

        _program->setUniform(_uniformCacheSpencer.renderingMethod, _renderingMethodOption.value());
        ghoul::opengl::TextureUnit psfUnit;

        glm::dvec3 eyePosition = glm::dvec3(
            glm::inverse(data.camera.combinedViewMatrix()) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
        );
        _program->setUniform(_uniformCacheSpencer.eyePosition, eyePosition);

        glm::dvec3 cameraUp = data.camera.lookUpVectorWorldSpace();
        _program->setUniform(_uniformCacheSpencer.cameraUp, cameraUp);

        glm::dmat4 modelMatrix =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            glm::dmat4(data.modelTransform.rotation) *
            glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

        glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
        glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());

        glm::dmat4 cameraViewProjectionMatrix = projectionMatrix * data.camera.combinedViewMatrix();

        _program->setUniform(_uniformCacheSpencer.modelMatrix, modelMatrix);
        _program->setUniform(_uniformCacheSpencer.cameraViewProjectionMatrix, cameraViewProjectionMatrix);
        _program->setUniform(_uniformCacheSpencer.colorOption, _colorOption);
        _program->setUniform(_uniformCacheSpencer.magnitudeExponent, _magnitudeExponent);
        
        
        /*
        _program->setUniform(_uniformCacheSpencer.billboardSize, _billboardSize);
        _program->setUniform(
            _uniformCacheSpencer.screenSize,
            glm::vec2(OsEng.windowWrapper().getCurrentViewportSize())
        );
        */
        /*
        if (_renderingMethodOption.value() == 0) { // Old Method
            glm::vec2 scaling = glm::vec2(1, -19);

            _program->setUniform(_uniformCacheOld.view, data.camera.viewMatrix());
            _program->setUniform(_uniformCacheOld.projection, data.camera.projectionMatrix());

            _program->setUniform(_uniformCacheSpencer.colorOption, _colorOption);
            _program->setUniform(_uniformCacheOld.alphaValue, _alphaValue);
            _program->setUniform(_uniformCacheOld.scaleFactor, _scaleFactor);
            _program->setUniform(_uniformCacheOld.minBillboardSize, _minBillboardSize);
            _program->setUniform(
                _uniformCacheSpencer.screenSize,
                glm::vec2(OsEng.renderEngine().renderingResolution())
            );

            setPscUniforms(*_program.get(), data.camera, data.position);
            _program->setUniform(_uniformCacheOld.scaling, scaling);

            psfUnit.activate();
            _pointSpreadFunctionTexture->bind();
            _program->setUniform(_uniformCacheOld.psfTexture, psfUnit);
        }
        else
        */

        _program->setUniform(_uniformCacheSpencer.psfParamConf, _psfMultiplyOption.value());
        _program->setUniform(_uniformCacheSpencer.lumCent, _lumCent);
        _program->setUniform(_uniformCacheSpencer.radiusCent, _radiusCent);
        _program->setUniform(_uniformCacheSpencer.brightnessCent, _brightnessCent);

        if (_renderingMethodOption.value() == 0) { // PSF Based Methods
            _program->setUniform(_uniformCacheSpencer.colorContribution, _colorContribution);
            _program->setUniform(_uniformCacheSpencer.psfMethod, _psfMethodOption.value());            
            _program->setUniform(_uniformCacheSpencer.p0Param, _p0Param);
            _program->setUniform(_uniformCacheSpencer.p1Param, _p1Param);
            _program->setUniform(_uniformCacheSpencer.p2Param, _p2Param);
            _program->setUniform(_uniformCacheSpencer.alphaConst, _spencerAlphaConst);
            _program->setUniform(_uniformCacheMoffat.FWHM, _FWHMConst);
            _program->setUniform(_uniformCacheMoffat.betaConstant, _moffatBetaConst);
        }
        else if (_renderingMethodOption.value() == 1) { // Textured based Method
            
            _program->setUniform(_uniformCacheOld.alphaValue, _alphaValue);

            psfUnit.activate();
            _pointSpreadFunctionTexture->bind();
            _program->setUniform(_uniformCacheOld.psfTexture, psfUnit);
        }

        ghoul::opengl::TextureUnit colorUnit;
        colorUnit.activate();
        _colorTexture->bind();
        _program->setUniform(_uniformCacheOld.colorTexture, colorUnit);

        glBindVertexArray(_vao);
        const GLsizei nStars = static_cast<GLsizei>(_fullData.size() / _nValuesPerStar);
        glDrawArrays(GL_POINTS, 0, nStars);

        glBindVertexArray(0);
        _program->deactivate();

        glDepthMask(true);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }

    void RenderableStars::update(const UpdateData&) {
        if (_dataIsDirty) {
            const int value = _colorOption;
            LDEBUG("Regenerating data");

            createDataSlice(ColorOption(value));

            int size = static_cast<int>(_slicedData.size());

            if (_vao == 0) {
                glGenVertexArrays(1, &_vao);
            }
            if (_vbo == 0) {
                glGenBuffers(1, &_vbo);
            }
            glBindVertexArray(_vao);
            glBindBuffer(GL_ARRAY_BUFFER, _vbo);
            glBufferData(
                GL_ARRAY_BUFFER,
                size * sizeof(GLfloat),
                &_slicedData[0],
                GL_STATIC_DRAW
            );

            GLint positionAttrib = _program->attributeLocation("in_position");
            GLint brightnessDataAttrib = _program->attributeLocation("in_bvLumAbsMag");

            const size_t nStars = _fullData.size() / _nValuesPerStar;
            const size_t nValues = _slicedData.size() / nStars;

            GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

            glEnableVertexAttribArray(positionAttrib);
            glEnableVertexAttribArray(brightnessDataAttrib);
            const int colorOption = _colorOption;
            switch (colorOption) {
            case ColorOption::Color:
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(ColorVBOLayout, position)
                );
                glVertexAttribPointer(
                    brightnessDataAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(ColorVBOLayout, bvColor))
                );

                break;
            case ColorOption::Velocity:
            {
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(VelocityVBOLayout, position)
                );
                glVertexAttribPointer(
                    brightnessDataAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(VelocityVBOLayout, bvColor))
                );

            GLint velocityAttrib = _program->attributeLocation("in_velocity");
            glEnableVertexAttribArray(velocityAttrib);
            glVertexAttribPointer(
                velocityAttrib,
                3,
                GL_FLOAT,
                GL_TRUE,
                stride,
                reinterpret_cast<void*>(offsetof(VelocityVBOLayout, vx)) // NOLINT
            );

                break;
            }
            case ColorOption::Speed:
            {
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(SpeedVBOLayout, position)
                );
                glVertexAttribPointer(
                    brightnessDataAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(SpeedVBOLayout, bvColor))
                );

                GLint speedAttrib = _program->attributeLocation("in_speed");
                glEnableVertexAttribArray(speedAttrib);
                glVertexAttribPointer(
                    speedAttrib,
                    1,
                    GL_FLOAT,
                    GL_TRUE,
                    stride,
                    reinterpret_cast<void*>(offsetof(SpeedVBOLayout, speed))
                );
            }
            }

            glBindBuffer(GL_ARRAY_BUFFER, 0);
            glBindVertexArray(0);

            _dataIsDirty = false;
        }

        if (_pointSpreadFunctionTextureIsDirty) {
            LDEBUG("Reloading Point Spread Function texture");
            _pointSpreadFunctionTexture = nullptr;
            if (_pointSpreadFunctionTexturePath.value() != "") {
                _pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(
                    absPath(_pointSpreadFunctionTexturePath)
                );

                if (_pointSpreadFunctionTexture) {
                    LDEBUG(fmt::format(
                        "Loaded texture from '{}'",
                        absPath(_pointSpreadFunctionTexturePath)
                    ));
                    _pointSpreadFunctionTexture->uploadTexture();
                }
                _pointSpreadFunctionTexture->setFilter(
                    ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
                );

                _pointSpreadFunctionFile = std::make_unique<ghoul::filesystem::File>(
                    _pointSpreadFunctionTexturePath
                    );
                _pointSpreadFunctionFile->setCallback(
                    [&](const ghoul::filesystem::File&) {
                    _pointSpreadFunctionTextureIsDirty = true;
                }
                );
            }
            _pointSpreadFunctionTextureIsDirty = false;
        }

        if (_colorTextureIsDirty) {
            LDEBUG("Reloading Color Texture");
            _colorTexture = nullptr;
            if (_colorTexturePath.value() != "") {
                _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                    absPath(_colorTexturePath)
                );
                if (_colorTexture) {
                    LDEBUG(fmt::format(
                        "Loaded texture from '{}'",
                        absPath(_colorTexturePath)
                    ));
                    _colorTexture->uploadTexture();
                }

                _colorTextureFile = std::make_unique<ghoul::filesystem::File>(
                    _colorTexturePath
                    );
                _colorTextureFile->setCallback(
                    [&](const ghoul::filesystem::File&) { _colorTextureIsDirty = true; }
                );
            }
            _colorTextureIsDirty = false;
        }

        if (_program->isDirty()) {
            _program->rebuildFromFile();
            ghoul::opengl::updateUniformLocations(*_program, _uniformCacheSpencer, UniformNamesSpencer);
            ghoul::opengl::updateUniformLocations(*_program, _uniformCacheOld, UniformNamesOld);
            ghoul::opengl::updateUniformLocations(*_program, _uniformCacheMoffat, UniformNamesMoffat);
        }
    }

    bool RenderableStars::loadData() {
        std::string _file = _speckFile;
        std::string cachedFile = FileSys.cacheManager()->cachedFilename(
            _file,
            ghoul::filesystem::CacheManager::Persistent::Yes
        );

        bool hasCachedFile = FileSys.fileExists(cachedFile);
        if (hasCachedFile) {
            LINFO(fmt::format(
                "Cached file '{}' used for Speck file '{}'",
                cachedFile,
                _file
            ));

            bool success = loadCachedFile(cachedFile);
            if (success) {
                return true;
            }
            else {
                FileSys.cacheManager()->removeCacheFile(_file);
                // Intentional fall-through to the 'else' computation to generate the cache
                // file for the next run
            }
        }
        else {
            LINFO(fmt::format("Cache for Speck file '{}' not found", _file));
        }
        LINFO(fmt::format("Loading Speck file '{}'", _file));

        bool success = readSpeckFile();
        if (!success) {
            return false;
        }

        LINFO("Saving cache");
        success = saveCachedFile(cachedFile);

        return success;
    }

    bool RenderableStars::readSpeckFile() {
        std::string _file = _speckFile;
        std::ifstream file(_file);
        if (!file.good()) {
            LERROR(fmt::format("Failed to open Speck file '{}'", _file));
            return false;
        }

        _nValuesPerStar = 0;

        // The beginning of the speck file has a header that either contains comments
        // (signaled by a preceding '#') or information about the structure of the file
        // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
        std::string line = "";
        while (true) {
            std::streampos position = file.tellg();
            std::getline(file, line);

            if (line[0] == '#' || line.empty()) {
                continue;
            }

            if (line.substr(0, 7) != "datavar" &&
                line.substr(0, 10) != "texturevar" &&
                line.substr(0, 7) != "texture")
            {
                // we read a line that doesn't belong to the header, so we have to jump back
                // before the beginning of the current line
                file.seekg(position);
                break;
            }

            if (line.substr(0, 7) == "datavar") {
                // datavar lines are structured as follows:
                // datavar # description
                // where # is the index of the data variable; so if we repeatedly overwrite
                // the 'nValues' variable with the latest index, we will end up with the total
                // number of values (+3 since X Y Z are not counted in the Speck file index)
                std::stringstream str(line);

                std::string dummy;
                str >> dummy;
                str >> _nValuesPerStar;
                _nValuesPerStar += 1; // We want the number, but the index is 0 based
            }
        }

        _nValuesPerStar += 3; // X Y Z are not counted in the Speck file indices

        float minLumValue = std::numeric_limits<float>::max();
        float maxLumValue = std::numeric_limits<float>::min();

        do {
            std::vector<float> values(_nValuesPerStar);

            std::getline(file, line);
            std::stringstream str(line);

            for (int i = 0; i < _nValuesPerStar; ++i) {
                str >> values[i];
            }
            bool nullArray = true;
            for (size_t i = 0; i < values.size(); ++i) {
                if (values[i] != 0.0) {
                    nullArray = false;
                    break;
                }
            }
            minLumValue = values[4] < minLumValue ? values[4] : minLumValue;
            maxLumValue = values[4] > maxLumValue ? values[4] : maxLumValue;
            if (!nullArray) {
                _fullData.insert(_fullData.end(), values.begin(), values.end());
            }
        } while (!file.eof());

        // Normalize Luminosity:
        for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
            _fullData[i + 4] = (_fullData[i + 4] - minLumValue) / (maxLumValue - minLumValue);
        }

        return true;
    }

    bool RenderableStars::loadCachedFile(const std::string& file) {
        std::ifstream fileStream(file, std::ifstream::binary);
        if (fileStream.good()) {
            int8_t version = 0;
            fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
            if (version != CurrentCacheVersion) {
                LINFO("The format of the cached file has changed: deleting old cache");
                fileStream.close();
                FileSys.deleteFile(file);
                return false;
            }

            int32_t nValues = 0;
            fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
            fileStream.read(reinterpret_cast<char*>(&_nValuesPerStar), sizeof(int32_t));

            _fullData.resize(nValues);
            fileStream.read(reinterpret_cast<char*>(&_fullData[0]),
                nValues * sizeof(_fullData[0]));

            bool success = fileStream.good();
            return success;
        }
        else {
            LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
            return false;
        }
    }

    bool RenderableStars::saveCachedFile(const std::string& file) const {
        std::ofstream fileStream(file, std::ofstream::binary);
        if (fileStream.good()) {
            fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
                sizeof(int8_t));

            int32_t nValues = static_cast<int32_t>(_fullData.size());
            if (nValues == 0) {
                LERROR("Error writing cache: No values were loaded");
                return false;
            }
            fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

            int32_t nValuesPerStar = static_cast<int32_t>(_nValuesPerStar);
            fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));

            size_t nBytes = nValues * sizeof(_fullData[0]);
            fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

            bool success = fileStream.good();
            return success;
        }
        else {
            LERROR(fmt::format("Error opening file '{}' for save cache file", file));
            return false;
        }
    }

    void RenderableStars::createDataSlice(ColorOption option) {
        _slicedData.clear();

        for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
            glm::vec3 p = glm::vec3(_fullData[i + 0], _fullData[i + 1], _fullData[i + 2]);
            p *= openspace::distanceconstants::Parsec;

            switch (option) {
            case ColorOption::Color:
            {
                union {
                    ColorVBOLayout value;
                    std::array<float, sizeof(ColorVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { {
                        p[0], p[1], p[2], 1.0
                    } };

#ifdef USING_STELLAR_TEST_GRID
                layout.value.bvColor = 0.650;// _fullData[i + 3];
                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 3];
#else
                layout.value.bvColor = _fullData[i + 3];
                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 5];
#endif

                _slicedData.insert(_slicedData.end(),
                    layout.data.begin(),
                    layout.data.end());

                break;
            }
            case ColorOption::Velocity:
            {
                union {
                    VelocityVBOLayout value;
                    std::array<float, sizeof(VelocityVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { {
                        p[0], p[1], p[2], 1.0
                    } };

                layout.value.bvColor = _fullData[i + 3];
                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 5];

                layout.value.vx = _fullData[i + 12];
                layout.value.vy = _fullData[i + 13];
                layout.value.vz = _fullData[i + 14];

                _slicedData.insert(_slicedData.end(),
                    layout.data.begin(),
                    layout.data.end());
                break;
            }
            case ColorOption::Speed:
            {
                union {
                    SpeedVBOLayout value;
                    std::array<float, sizeof(SpeedVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { {
                        p[0], p[1], p[2], 1.0
                    } };

                layout.value.bvColor = _fullData[i + 3];
                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 5];

                layout.value.speed = _fullData[i + 15];

                _slicedData.insert(_slicedData.end(),
                    layout.data.begin(),
                    layout.data.end());
                break;
            }
            }
        }
    }

} // namespace openspace
