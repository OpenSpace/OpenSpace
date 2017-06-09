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

#include <modules/space/rendering/renderableplanet.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <modules/space/rendering/planetgeometry.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/misc/invariants.h>

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
#include <openspace/rendering/deferredcastermanager.h>
#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#endif

#include <memory>
#include <fstream>

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
    const char* KeyGeometry      = "Geometry";
    const char* KeyRadius        = "Radius";
    const char* KeyColorTexture  = "Textures.Color";
    const char* KeyNightTexture  = "Textures.Night";
    const char* KeyHeightTexture = "Textures.Height";
    const char* KeyShading       = "PerformShading";
       
    static const std::string _loggerCat = "RenderablePlanet";

    const char* keyFrame                         = "Frame";
    const char* keyShadowGroup                   = "Shadow_Group";
    const char* keyShadowSource                  = "Source";
    const char* keyShadowCaster                  = "Caster";
    const char* keyBody                          = "Body";

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    const std::string keyAtmosphere               = "Atmosphere";
    const std::string keyAtmosphereRadius         = "AtmoshereRadius";
    const std::string keyPlanetRadius             = "PlanetRadius";
    const std::string keyAverageGroundReflectance = "PlanetAverageGroundReflectance";
    const std::string keyRayleigh                 = "Rayleigh";
    const std::string keyRayleighHeightScale      = "H_R";
    const std::string keyOzone                    = "Ozone";
    const std::string keyOzoneHeightScale         = "H_O";
    const std::string keyMie                      = "Mie";
    const std::string keyMieHeightScale           = "H_M";
    const std::string keyMiePhaseConstant         = "G";
    const std::string keyImage                    = "Image";
    const std::string keyToneMappingOp            = "ToneMapping";
    const std::string keyExposure                 = "Exposure";
    const std::string keyGamma                    = "Gamma";
    const std::string keyATMDebug                 = "Debug";
    const std::string keyTextureScale             = "PreCalculatedTextureScale";
    const std::string keySaveTextures             = "SaveCalculatedTextures";
#endif
} // namespace

namespace openspace {

documentation::Documentation RenderablePlanet::Documentation() {
    using namespace documentation;
    return {
        "RenderablePlanet",
        "space_renderable_planet",
        {
            {
                KeyGeometry,
                new ReferencingVerifier("space_geometry_planet"),
                "Specifies the planet geometry that is used for this RenderablePlanet.",
                Optional::No
            },
            {
                KeyRadius,
                new DoubleVerifier,
                "Specifies the radius of the planet. If this value is not specified, it "
                "will try to query the SPICE library for radius values.",
                Optional::Yes
            },
            {
                KeyColorTexture,
                new StringVerifier,
                "Specifies the color texture that is used for this RenderablePlanet.",
                Optional::Yes
            },
            {
                KeyHeightTexture,
                new StringVerifier,
                "Specifies the height texture that is used for this RenderablePlanet.",
                Optional::Yes
            },
            {
                KeyNightTexture,
                new StringVerifier,
                "Specifies the texture that is used for the night side of this "
                "RenderablePlanet.",
                Optional::Yes
            },
            {
                KeyShading,
                new BoolVerifier,
                "Specifies whether the planet should be rendered shaded by the Sun. If "
                "this value is 'false', any existing night texture will not be used. "
                "This value defaults to 'true'.",
                Optional::Yes
            }
        }
    };
}

RenderablePlanet::RenderablePlanet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _nightTexturePath("nightTexture", "Night Texture")
    , _heightMapTexturePath("heightMap", "Heightmap Texture")
    , _heightExaggeration("heightExaggeration", "Height Exaggeration", 1.f, 0.f, 10.f)
    , _performShading("performShading", "Perform Shading", true)
    , _programObject(nullptr)
    , _texture(nullptr)
    , _nightTexture(nullptr)
    , _heightMapTexture(nullptr)
    , _geometry(nullptr)
#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    , _atmosphereHeightP("atmmosphereHeight", "Atmosphere Height (KM)", 60.0f, 0.1f, 99.0f)
    , _groundAverageReflectanceP("averageGroundReflectance", "Average Ground Reflectance (%)", 0.1f, 0.0f, 1.0f)
    , _rayleighHeightScaleP("rayleighHeightScale", "Rayleigh Height Scale (KM)", 8.0f, 0.1f, 20.0f)
    , _rayleighScatteringCoeffXP("rayleighScatteringCoeffX", "Rayleigh Scattering Coeff X (x10e-3)", 1.0f, 0.01f, 100.0f)
    , _rayleighScatteringCoeffYP("rayleighScatteringCoeffY", "Rayleigh Scattering Coeff Y (x10e-3)", 1.0f, 0.01f, 100.0f)
    , _rayleighScatteringCoeffZP("rayleighScatteringCoeffZ", "Rayleigh Scattering Coeff Z (x10e-3)", 1.0f, 0.01f, 100.0f)
    , _ozoneEnabledP("ozone", "Ozone Layer Enabled", true)
    , _ozoneHeightScaleP("ozoneLayerHeightScale", "Ozone Height Scale (KM)", 8.0f, 0.1f, 20.0f)
    , _ozoneCoeffXP("ozoneLayerCoeffX", "Ozone Layer Extinction Coeff X (x10e-5)", 3.426f, 0.01f, 100.0f)
    , _ozoneCoeffYP("ozoneLayerCoeffY", "Ozone Layer Extinction Coeff Y (x10e-5)", 8.298f, 0.01f, 100.0f)
    , _ozoneCoeffZP("ozoneLayerCoeffZ", "Ozone Layer Extinction Coeff Z (x10e-5)", 0.356f, 0.01f, 100.0f)
    , _mieHeightScaleP("mieHeightScale", "Mie Height Scale (KM)", 1.2f, 0.1f, 20.0f)
    , _mieScatteringCoefficientP("mieScatteringCoefficient", "Mie Scattering Coefficient (x10e-3)", 4.0f, 0.01f, 1000.0f)
    , _mieScatteringExtinctionPropCoefficientP("mieScatteringExtinctionPropCoefficient",
        "Mie Scattering/Extinction Proportion Coefficient (%)", 0.9f, 0.01f, 1.0f)    
    , _mieAsymmetricFactorGP("mieAsymmetricFactorG", "Mie Asymmetric Factor G", 0.85f, -1.0f, 1.0f)
    , _sunIntensityP("sunIntensity", "Sun Intensity", 50.0f, 0.1f, 1000.0f)
    , _hdrExpositionP("hdrExposition", "HDR", 0.4f, 0.01f, 5.0f)
    , _gammaConstantP("gamma", "Gamma Correction", 1.8f, 0.1f, 3.0f)
    , _atmosphereEnabled(false)
    , _ozoneLayerEnabled(false)
    , _atmosphereRadius(0.f)
    , _atmospherePlanetRadius(0.f)
    , _planetAverageGroundReflectance(0.f)
    , _rayleighHeightScale(0.f)
    , _ozoneHeightScale(0.f)
    , _mieHeightScale(0.f)
    , _miePhaseConstant(0.f)
    , _sunRadianceIntensity(50.f)
    , _hdrConstant(0.f)
    , _gammaConstant(1.8f)
    , _mieExtinctionCoeff(glm::vec3(0.f))
    , _rayleighScatteringCoeff(glm::vec3(0.f))
    , _ozoneExtinctionCoeff(glm::vec3(0.f))
    , _mieScatteringCoeff(glm::vec3(0.f))
    , _saveCalculationsToTexture(false)
    , _preCalculatedTexturesScale(1.0)
#endif
    , _alpha(1.f)
    , _planetRadius(0.f)
    , _hasNightTexture(false)
    , _hasHeightTexture(false)
    , _shadowEnabled(false)
    , _time(0.f)
{
    ghoul_precondition(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        "RenderablePlanet needs the name to be specified"
    );

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePlanet"
    );

    const std::string name = dictionary.value<std::string>(SceneGraphNode::KeyName);

    ghoul::Dictionary geomDict = dictionary.value<ghoul::Dictionary>(KeyGeometry);

    if (dictionary.hasKey(KeyRadius)) {
        // If the user specified a radius, we want to use this
        _planetRadius = dictionary.value<float>(KeyRadius);
    }
    else if (SpiceManager::ref().hasValue(name, "RADII") ) {
        // If the user didn't specfify a radius, but Spice has a radius, we can use this
        glm::dvec3 radius;
        SpiceManager::ref().getValue(name, "RADII", radius);
        radius *= 1000.0; // Spice gives radii in KM.
        std::swap(radius[1], radius[2]); // z is equivalent to y in our coordinate system
        geomDict.setValue(KeyRadius, radius);

        _planetRadius = static_cast<float>((radius.x + radius.y + radius.z) / 3.0);
    }
    else {
        LERRORC("RenderablePlanet", "Missing radius specification");
    }

    _geometry = planetgeometry::PlanetGeometry::createFromDictionary(geomDict);

    if (dictionary.hasKey(KeyColorTexture)) {
        _colorTexturePath = absPath(dictionary.value<std::string>(KeyColorTexture));
    }

    if (dictionary.hasKey(KeyNightTexture)) {
        _hasNightTexture = true;
        _nightTexturePath = absPath(dictionary.value<std::string>(KeyNightTexture));
    }

    if (dictionary.hasKey(KeyHeightTexture)) {
        _hasHeightTexture = true;
        _heightMapTexturePath = absPath(dictionary.value<std::string>(KeyHeightTexture));
    }

    if (dictionary.hasKey(KeyShading)) {
        _performShading = dictionary.value<bool>(KeyShading);
    }

    addPropertySubOwner(_geometry.get());

    auto loadTextureCallback = [this]() {loadTexture(); };
    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(loadTextureCallback);

    addProperty(_nightTexturePath);
    _nightTexturePath.onChange(loadTextureCallback);

    addProperty(_heightMapTexturePath);
    _heightMapTexturePath.onChange(loadTextureCallback);

    addProperty(_heightExaggeration);
    addProperty(_performShading);

    //================================================================
    //======== Reads Shadow (Eclipses) Entries in mod file ===========
    //================================================================
    ghoul::Dictionary shadowDictionary;
    bool success = dictionary.getValue(keyShadowGroup, shadowDictionary);
    bool disableShadows = false;
    if (success) {
        std::vector< std::pair<std::string, float > > sourceArray;
        unsigned int sourceCounter = 1;
        while (success) {
            std::string sourceName;
            std::stringstream ss;
            ss << keyShadowSource << sourceCounter << ".Name";
            success = shadowDictionary.getValue(ss.str(), sourceName);
            if (success) {
                float sourceRadius;
                ss.str(std::string());
                ss << keyShadowSource << sourceCounter << ".Radius";
                success = shadowDictionary.getValue(ss.str(), sourceRadius);
                if (success) {
                    sourceArray.push_back(std::pair< std::string, float>(
                        sourceName, sourceRadius));
                }
                else {
                    LWARNING("No Radius value expecified for Shadow Source Name " 
                        << sourceName << " from " << name 
                        << " planet.\nDisabling shadows for this planet.");
                    disableShadows = true;
                    break;
                }
            }
            sourceCounter++;
        }

        if (!disableShadows && !sourceArray.empty()) {
            success = true;
            std::vector< std::pair<std::string, float > > casterArray;
            unsigned int casterCounter = 1;
            while (success) {
                std::string casterName;
                std::stringstream ss;
                ss << keyShadowCaster << casterCounter << ".Name";
                success = shadowDictionary.getValue(ss.str(), casterName);
                if (success) {
                    float casterRadius;
                    ss.str(std::string());
                    ss << keyShadowCaster << casterCounter << ".Radius";
                    success = shadowDictionary.getValue(ss.str(), casterRadius);
                    if (success) {
                        casterArray.push_back(std::pair< std::string, float>(
                            casterName, casterRadius));
                    }
                    else {
                        LWARNING("No Radius value expecified for Shadow Caster Name "
                            << casterName << " from " << name
                            << " planet.\nDisabling shadows for this planet.");
                        disableShadows = true;
                        break;
                    }
                }

                casterCounter++;
            }

            if (!disableShadows && (!sourceArray.empty() && !casterArray.empty())) {
                for (const auto & source : sourceArray)
                    for (const auto & caster : casterArray) {
                        ShadowConf sc;
                        sc.source = source;
                        sc.caster = caster;
                        _shadowConfArray.push_back(sc);
                    }
                _shadowEnabled = true;
            }
        }
    }

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    //================================================================
    //========== Reads Atmosphere Entries from mod file ==============
    //================================================================
    bool errorReadingAtmosphereData = false;
    ghoul::Dictionary atmosphereDictionary;
    success = dictionary.getValue(keyAtmosphere, atmosphereDictionary);
    if (success) {
        if (!atmosphereDictionary.getValue(keyAtmosphereRadius, _atmosphereRadius)) {
            errorReadingAtmosphereData = true;
            LWARNING("No Atmosphere Radius value expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        if (!atmosphereDictionary.getValue(keyPlanetRadius, _atmospherePlanetRadius)) {
            errorReadingAtmosphereData = true;
            LWARNING("No Planet Radius value expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        if (!atmosphereDictionary.getValue(keyAverageGroundReflectance, _planetAverageGroundReflectance)) {
            errorReadingAtmosphereData = true;
            LWARNING("No Average Atmosphere Ground Reflectance value expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary rayleighDictionary;
        success = atmosphereDictionary.getValue(keyRayleigh, rayleighDictionary);

        if (success) {
            // Not using right now.
            glm::vec3 rayleighWavelengths;
            success = rayleighDictionary.getValue("Coefficients.Wavelengths", rayleighWavelengths);

            if (!rayleighDictionary.getValue("Coefficients.Scattering", _rayleighScatteringCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Rayleigh Scattering parameters expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!rayleighDictionary.getValue(keyRayleighHeightScale, _rayleighHeightScale)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Rayleigh Height Scale value expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }
        }
        else {
            errorReadingAtmosphereData = true;
            LWARNING("No Rayleigh parameters expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary ozoneDictionary;
        success = atmosphereDictionary.getValue(keyOzone, ozoneDictionary);
        if (success) {
            _ozoneLayerEnabled = true;
            if (!ozoneDictionary.getValue(keyOzoneHeightScale, _ozoneHeightScale)) {
                _ozoneLayerEnabled = false;
            }

            if (!ozoneDictionary.getValue("Coefficients.Extinction", _ozoneExtinctionCoeff)) {
                _ozoneLayerEnabled = false;
            }
        }
        else {
            _ozoneLayerEnabled = false;
        }

        ghoul::Dictionary mieDictionary;
        success = atmosphereDictionary.getValue(keyMie, mieDictionary);
        if (success) {
            if (!mieDictionary.getValue(keyMieHeightScale, _mieHeightScale)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Height Scale value expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue("Coefficients.Scattering", _mieScatteringCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Scattering parameters expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue("Coefficients.Extinction", _mieExtinctionCoeff)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Extinction parameters expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue(keyMiePhaseConstant, _miePhaseConstant)) {
                errorReadingAtmosphereData = true;
                LWARNING("No Mie Phase Constant value expecified for Atmosphere Effects of "
                    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }
        }
        else {
            errorReadingAtmosphereData = true;
            LWARNING("No Mie parameters expecified for Atmosphere Effects of "
                << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary ImageDictionary;
        success = atmosphereDictionary.getValue(keyImage, ImageDictionary);
        if (success) {
            if (ImageDictionary.getValue(keyToneMappingOp, _preCalculatedTexturesScale)) {
                LDEBUG("Atmosphere Texture Scaled to " << _preCalculatedTexturesScale);
            }

            if (ImageDictionary.getValue(keyExposure, _hdrConstant)) {
                LDEBUG("Saving Precalculated Atmosphere Textures.");
            }

            if (ImageDictionary.getValue(keyGamma, _gammaConstant)) {
                LDEBUG("Saving Precalculated Atmosphere Textures.");
            }
        }

        ghoul::Dictionary debugDictionary;
        success = atmosphereDictionary.getValue(keyATMDebug, debugDictionary);
        if (success) {
            if (debugDictionary.getValue(keyTextureScale, _preCalculatedTexturesScale)) {
                LDEBUG("Atmosphere Texture Scaled to " << _preCalculatedTexturesScale);
            }

            if (debugDictionary.getValue(keySaveTextures, _saveCalculationsToTexture)) {
                LDEBUG("Saving Precalculated Atmosphere Textures.");
            }

        }

        if (!errorReadingAtmosphereData) {
            _atmosphereEnabled = true;

            //========================================================
            //============== Atmosphere Properties ===================
            //========================================================

            _atmosphereHeightP.set(_atmosphereRadius - _atmospherePlanetRadius);
            _atmosphereHeightP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_atmosphereHeightP);

            _groundAverageReflectanceP.set(_planetAverageGroundReflectance);
            _groundAverageReflectanceP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_groundAverageReflectanceP);

            _rayleighHeightScaleP.set(_rayleighHeightScale);
            _rayleighHeightScaleP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_rayleighHeightScaleP);

            _rayleighScatteringCoeffXP.set(_rayleighScatteringCoeff.x * 1000.0f);
            _rayleighScatteringCoeffXP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_rayleighScatteringCoeffXP);

            _rayleighScatteringCoeffYP.set(_rayleighScatteringCoeff.y * 1000.0f);
            _rayleighScatteringCoeffYP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_rayleighScatteringCoeffYP);

            _rayleighScatteringCoeffZP.set(_rayleighScatteringCoeff.z * 1000.0f);
            _rayleighScatteringCoeffZP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_rayleighScatteringCoeffZP);

            _ozoneEnabledP.set(_ozoneLayerEnabled);
            _ozoneEnabledP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_ozoneEnabledP);

            _ozoneHeightScaleP.set(_ozoneHeightScale);
            _ozoneHeightScaleP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_ozoneHeightScaleP);

            _ozoneCoeffXP.set(_ozoneExtinctionCoeff.x * 100000.0f);
            _ozoneCoeffXP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_ozoneCoeffXP);

            _ozoneCoeffYP.set(_ozoneExtinctionCoeff.y * 100000.0f);
            _ozoneCoeffYP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_ozoneCoeffYP);

            _ozoneCoeffZP.set(_ozoneExtinctionCoeff.z * 100000.0f);
            _ozoneCoeffZP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_ozoneCoeffZP);

            _mieHeightScaleP.set(_mieHeightScale);
            _mieHeightScaleP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_mieHeightScaleP);

            _mieScatteringCoefficientP.set(_mieScatteringCoeff.r * 1000.0f);
            _mieScatteringCoefficientP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_mieScatteringCoefficientP);

            _mieScatteringExtinctionPropCoefficientP.set(_mieScatteringCoeff.r / _mieExtinctionCoeff.r);
            _mieScatteringExtinctionPropCoefficientP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_mieScatteringExtinctionPropCoefficientP);

            _mieAsymmetricFactorGP.set(_miePhaseConstant);
            _mieAsymmetricFactorGP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_mieAsymmetricFactorGP);

            _sunIntensityP.set(_sunRadianceIntensity);
            _sunIntensityP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_sunIntensityP);

            _hdrExpositionP.set(_hdrConstant);
            _hdrExpositionP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_hdrExpositionP);

            _gammaConstantP.set(_gammaConstant);
            _gammaConstantP.onChange(std::bind(&RenderablePlanet::updateAtmosphereParameters, this));
            addProperty(_gammaConstantP);
        }
    }
#endif
}

bool RenderablePlanet::initialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Checking System State before initialization. OpenGL error: " << errString);
    }

    //===================================================================
    //=========== Defines the shading program to be executed ============
    //===================================================================
    if (_programObject == nullptr && _shadowEnabled && _hasNightTexture) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowNightProgram",
            "${MODULE_SPACE}/shaders/shadow_nighttexture_vs.glsl",
            "${MODULE_SPACE}/shaders/shadow_nighttexture_fs.glsl");
        std::cout << "--------- Using shadow_nighttexture program. ----------" << std::endl;
    } 
    else if (_programObject == nullptr && _shadowEnabled) {
        // shadow program
        _programObject = renderEngine.buildRenderProgram(
            "shadowProgram",
            "${MODULE_SPACE}/shaders/shadow_vs.glsl",
            "${MODULE_SPACE}/shaders/shadow_fs.glsl");
        std::cout << "--------- Using shadow program. ----------" << std::endl;
    } 
    else if (_programObject == nullptr && _hasNightTexture) {
        // Night texture program
        _programObject = renderEngine.buildRenderProgram(
            "nightTextureProgram",
            "${MODULE_SPACE}/shaders/nighttexture_vs.glsl",
            "${MODULE_SPACE}/shaders/nighttexture_fs.glsl");
        std::cout << "--------- Using nighttexture program. ----------" << std::endl;
    }
    else if (_programObject == nullptr) {
        // pscstandard
        _programObject = renderEngine.buildRenderProgram(
            "pscstandard",
            "${MODULE_SPACE}/shaders/renderableplanet_vs.glsl",
            "${MODULE_SPACE}/shaders/renderableplanet_fs.glsl");
        std::cout << "--------- Using renderableplanet program. ----------" << std::endl;
    }

    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    _programObject->setIgnoreUniformLocationError(IgnoreError::Yes);

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after load shading programs. OpenGL error: " << errString);
    }

    _geometry->initialize(this);

    // Deactivate any previously activated shader program.
    _programObject->deactivate();

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Shader Programs Creation. OpenGL error: " << errString);
    }
    
    loadTexture();

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_atmosphereEnabled) {
        _deferredcaster = std::make_unique<AtmosphereDeferredcaster>();
        if (_deferredcaster) {
            _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
            _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
            _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
            _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
            _deferredcaster->enableOzone(_ozoneLayerEnabled);
            _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
            _deferredcaster->setMieHeightScale(_mieHeightScale);
            _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
            _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
            _deferredcaster->setHDRConstant(_hdrConstant);
            _deferredcaster->setGammaConstant(_gammaConstant);
            _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
            _deferredcaster->setOzoneExtinctionCoefficients(_ozoneExtinctionCoeff);
            _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
            _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
            // TODO: Fix the ellipsoid nature of the renderable globe (JCC)
            //_deferredcaster->setEllipsoidRadii(_ellipsoid.radii());
            _deferredcaster->setRenderableClass(AtmosphereDeferredcaster::RenderablePlanet);

            _deferredcaster->setPrecalculationTextureScale(_preCalculatedTexturesScale);
            if (_saveCalculationsToTexture)
                _deferredcaster->enablePrecalculationTexturesSaving();

            _deferredcaster->initialize();
        }

        OsEng.renderEngine().deferredcasterManager().attachDeferredcaster(*_deferredcaster.get());

        std::function<void(bool)> onChange = [&](bool enabled) {
            if (enabled) {
                OsEng.renderEngine().deferredcasterManager().attachDeferredcaster(*_deferredcaster.get());
            }
            else {
                OsEng.renderEngine().deferredcasterManager().detachDeferredcaster(*_deferredcaster.get());
            }
        };

        onEnabledChange(onChange);
    }
#endif

    return isReady();
}

bool RenderablePlanet::deinitialize() {
    if (_geometry) {
        _geometry->deinitialize();
        _geometry = nullptr;
    }

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    _geometry = nullptr;
    _texture = nullptr;
    _nightTexture = nullptr;

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_deferredcaster) {
        OsEng.renderEngine().deferredcasterManager().detachDeferredcaster(*_deferredcaster.get());
        _deferredcaster = nullptr;
    }
#endif

    return true;
}

bool RenderablePlanet::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_texture != nullptr);
    ready &= (_geometry != nullptr);
    return ready;
}

void RenderablePlanet::computeModelTransformMatrix(const openspace::TransformData & transformData, glm::dmat4 * modelTransform) {
    // scale the planet to appropriate size since the planet is a unit sphere    
    *modelTransform =
        glm::translate(glm::dmat4(1.0), transformData.translation) * // Translation
        glm::dmat4(transformData.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(transformData.scale)));


    //earth needs to be rotated for that to work.
    glm::dmat4 rot = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(1, 0, 0));
    glm::dmat4 roty = glm::rotate(glm::dmat4(1.0), M_PI_2, glm::dvec3(0, -1, 0));
    //glm::dmat4 rotProp = glm::rotate(glm::dmat4(1.0), glm::radians(static_cast<double>(_rotation)), glm::dvec3(0, 1, 0));
    *modelTransform = *modelTransform * rot * roty /** rotProp*/;
}

void RenderablePlanet::render(const RenderData& data, RendererTasks& tasks) {
    // activate shader
    _programObject->activate();
    
    glm::dmat4 modelTransform = glm::dmat4(1.0);
    computeModelTransformMatrix(data.modelTransform, &modelTransform);

    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
    
    _programObject->setUniform("transparency", _alpha);
    _programObject->setUniform("modelViewTransform", modelViewTransform);
    _programObject->setUniform("modelViewProjectionTransform", 
        data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewTransform)
    );
    _programObject->setUniform("ModelTransform", glm::mat4(modelTransform));

    // Normal Transformation
    /*glm::mat4 translateObjTrans = glm::translate(glm::mat4(1.0), data.position.vec3());
    glm::mat4 translateCamTrans = glm::translate(glm::mat4(1.0), -data.camera.positionVec3());
    float scaleFactor = data.camera.scaling().x * powf(10.0, data.camera.scaling().y);
    glm::mat4 scaleCamTrans = glm::scale(glm::mat4(1.0), glm::vec3(scaleFactor));*/

//    glm::mat4 ModelViewTrans = data.camera.viewMatrix() * scaleCamTrans *
//        translateCamTrans * translateObjTrans * glm::mat4(modelTransform);
    
    setPscUniforms(*_programObject.get(), data.camera, data.position);
    
    _programObject->setUniform("_performShading", _performShading);
    _programObject->setUniform("_hasHeightMap", _hasHeightTexture);
    _programObject->setUniform("_heightExaggeration", _heightExaggeration);

    // Bind texture
    ghoul::opengl::TextureUnit dayUnit;
    ghoul::opengl::TextureUnit nightUnit;
    ghoul::opengl::TextureUnit heightUnit;

    dayUnit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", dayUnit);

    // Bind possible night texture
    if (_hasNightTexture && _nightTexture) {
        nightUnit.activate();
        _nightTexture->bind();
        _programObject->setUniform("nightTex", nightUnit);
    }

    if (_hasHeightTexture && _heightMapTexture) {
        heightUnit.activate();
        _heightMapTexture->bind();
        _programObject->setUniform("heightTex", heightUnit);
    }

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    //=============================================================================
    //============= Eclipse Shadow Calculations and Uniforms Loading ==============
    //=============================================================================
    // TODO: Move Calculations to VIEW SPACE (let's avoid precision problems...)
    double lt;
    if (!_shadowConfArray.empty()) {
        std::vector<ShadowRenderingStruct> shadowDataArray;
        shadowDataArray.reserve(_shadowConfArray.size());

        for (const auto & shadowConf : _shadowConfArray) {
            // TO REMEMBER: all distances and lengths in world coordinates are in meters!!! We need to move this to view space...
            // Getting source and caster:
            glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(shadowConf.source.first, "SUN", "GALACTIC", {}, _time, lt);
            sourcePos           *= 1000.0; // converting to meters
            glm::dvec3 casterPos = SpiceManager::ref().targetPosition(shadowConf.caster.first, "SUN", "GALACTIC", {}, _time, lt);
            casterPos           *= 1000.0; // converting to meters
            psc caster_pos       = PowerScaledCoordinate::CreatePowerScaledCoordinate(casterPos.x, casterPos.y, casterPos.z);
            
            // First we determine if the caster is shadowing the current planet (all calculations in World Coordinates):
            glm::vec3 planetCasterVec   = (caster_pos - data.position).vec3();
            glm::vec3 sourceCasterVec   = glm::vec3(casterPos - sourcePos);
            float sc_length             = glm::length(sourceCasterVec);
            glm::vec3 planetCaster_proj = (glm::dot(planetCasterVec, sourceCasterVec) / (sc_length*sc_length)) * sourceCasterVec;
            float d_test                = glm::length(planetCasterVec - planetCaster_proj);
            float xp_test               = shadowConf.caster.second * sc_length / (shadowConf.source.second + shadowConf.caster.second);
            float rp_test               = shadowConf.caster.second * (glm::length(planetCaster_proj) + xp_test) / xp_test;
                        
            double casterDistSun = glm::length(casterPos);
            float planetDistSun = glm::length(data.position.vec3());

            ShadowRenderingStruct shadowData;
            shadowData.isShadowing = false;

            if ( ((d_test - rp_test) < _planetRadius) &&
                 (casterDistSun < planetDistSun) ) {
                // The current caster is shadowing the current planet
                shadowData.isShadowing       = true;
                shadowData.rs                = shadowConf.source.second;
                shadowData.rc                = shadowConf.caster.second;
                shadowData.sourceCasterVec   = sourceCasterVec;
                shadowData.xp                = xp_test;
                shadowData.xu                = shadowData.rc * sc_length / (shadowData.rs - shadowData.rc);
                shadowData.casterPositionVec = glm::vec3(casterPos);
            }
            shadowDataArray.push_back(shadowData);
        }

        const std::string uniformVarName("shadowDataArray[");
        unsigned int counter = 0;
        for (const auto & sd : shadowDataArray) {
            std::stringstream ss;
            ss << uniformVarName << counter << "].isShadowing";
            _programObject->setUniform(ss.str(), sd.isShadowing);
            if (sd.isShadowing) {
                ss.str(std::string());
                ss << uniformVarName << counter << "].xp";
                _programObject->setUniform(ss.str(), sd.xp);
                ss.str(std::string());
                ss << uniformVarName << counter << "].xu";
                _programObject->setUniform(ss.str(), sd.xu);
                /*ss.str(std::string());
                ss << uniformVarName << counter << "].rs";
                _programObject->setUniform(ss.str(), sd.rs);*/
                ss.str(std::string());
                ss << uniformVarName << counter << "].rc";
                _programObject->setUniform(ss.str(), sd.rc);
                ss.str(std::string());
                ss << uniformVarName << counter << "].sourceCasterVec";
                _programObject->setUniform(ss.str(), sd.sourceCasterVec);
                ss.str(std::string());
                ss << uniformVarName << counter << "].casterPositionVec";
                _programObject->setUniform(ss.str(), sd.casterPositionVec);
            }
            counter++;
        }
    }

    // render
    _geometry->render();

    // disable shader
    _programObject->deactivate();

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_atmosphereEnabled) {
        DeferredcasterTask task{ _deferredcaster.get(), data };
        tasks.deferredcasterTasks.push_back(task);
    }    
#endif 

}

void RenderablePlanet::update(const UpdateData& data) {
    // set spice-orientation in accordance to timestamp
    _stateMatrix = data.modelTransform.rotation;
    //_stateMatrix = SpiceManager::ref().positionTransformMatrix(_frame, "GALACTIC", data.time);
    _time = data.time.j2000Seconds();

    if (_programObject && _programObject->isDirty())
        _programObject->rebuildFromFile();

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_deferredcaster) {
        _deferredcaster->setTime(data.time.j2000Seconds());
        glm::dmat4 modelTransform;
        computeModelTransformMatrix(data.modelTransform, &modelTransform);
        _deferredcaster->setModelTransform(modelTransform);
    }
#endif
}

void RenderablePlanet::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            if (_texture->numberOfChannels() == 1) {
                _texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_RED });
            }

            LDEBUG("Loaded texture from '" << _colorTexturePath << "'");
            _texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //_texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        }
    }

    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after reading color texture. OpenGL error: " << errString);
    }

    if (_hasNightTexture) {
        _nightTexture = nullptr;
        if (_nightTexturePath.value() != "") {
            _nightTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_nightTexturePath));
            if (_nightTexture) {
                LDEBUG("Loaded texture from '" << _nightTexturePath << "'");
                _nightTexture->uploadTexture();
                _nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after reading night texture. OpenGL error: " << errString);
    }
    
    if (_hasHeightTexture) {
        _heightMapTexture = nullptr;
        if (_heightMapTexturePath.value() != "") {
            _heightMapTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_heightMapTexturePath));
            if (_heightMapTexture) {
                LDEBUG("Loaded texture from '" << _heightMapTexturePath << "'");
                _heightMapTexture->uploadTexture();
                _heightMapTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                //_nightTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            }
        }
    }

    while ((err = glGetError()) != GL_NO_ERROR) {
        const GLubyte * errString = gluErrorString(err);
        LERROR("Error after reading height mapping texture. OpenGL error: " << errString);
    }
}

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED

void RenderablePlanet::updateAtmosphereParameters() {
    bool executeComputation = true;
    
    if (_sunRadianceIntensity != _sunIntensityP ||
        _hdrConstant != _hdrExpositionP ||
        _gammaConstant != _gammaConstantP)
        executeComputation = false;

    _atmosphereRadius = _atmospherePlanetRadius + _atmosphereHeightP;
    _planetAverageGroundReflectance = _groundAverageReflectanceP;
    _rayleighHeightScale = _rayleighHeightScaleP;
    _rayleighScatteringCoeff = glm::vec3(_rayleighScatteringCoeffXP * 0.001f, _rayleighScatteringCoeffYP * 0.001f,
        _rayleighScatteringCoeffZP * 0.001f);
    _ozoneLayerEnabled = _ozoneEnabledP.value();
    _ozoneHeightScale = _ozoneHeightScaleP.value();
    _ozoneExtinctionCoeff = glm::vec3(_ozoneCoeffXP.value() * 0.00001f,
        _ozoneCoeffYP.value() * 0.00001f,
        _ozoneCoeffZP.value() * 0.00001f);
    _mieHeightScale = _mieHeightScaleP;
    _mieScatteringCoeff = glm::vec3(_mieScatteringCoefficientP * 0.001f);
    _mieExtinctionCoeff = _mieScatteringCoeff * (1.0f / static_cast<float>(_mieScatteringExtinctionPropCoefficientP));
    _miePhaseConstant = _mieAsymmetricFactorGP;
    _sunRadianceIntensity = _sunIntensityP;
    _hdrConstant = _hdrExpositionP;
    _gammaConstant = _gammaConstantP.value();

    if (_deferredcaster) {
        _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
        _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
        _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
        _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
        _deferredcaster->enableOzone(_ozoneLayerEnabled);
        _deferredcaster->setOzoneHeightScale(_ozoneHeightScale);
        _deferredcaster->setMieHeightScale(_mieHeightScale);
        _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
        _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
        _deferredcaster->setHDRConstant(_hdrConstant);
        _deferredcaster->setGammaConstant(_gammaConstant);
        _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
        _deferredcaster->setOzoneExtinctionCoefficients(_ozoneExtinctionCoeff);
        _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
        _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
        _deferredcaster->setRenderableClass(AtmosphereDeferredcaster::RenderablePlanet);

        if (executeComputation)
            _deferredcaster->preCalculateAtmosphereParam();
    }
}
#endif
}  // namespace openspace