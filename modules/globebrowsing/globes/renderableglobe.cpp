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

#include <modules/globebrowsing/globes/renderableglobe.h>
 
#include <modules/debugging/rendering/debugrenderer.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/pointglobe.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
#include <openspace/rendering/deferredcastermanager.h>
#include <modules/atmosphere/rendering/atmospheredeferredcaster.h>
#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderer.h>
#endif


namespace {
    const char* keyFrame = "Frame";
    const char* keyRadii = "Radii";
    const char* keySegmentsPerPatch = "SegmentsPerPatch";
    const char* keyLayers = "Layers";
    const char* keyShadowGroup = "Shadow_Group";
    const char* keyShadowSource = "Source";
    const char* keyShadowCaster = "Caster";
#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    const char* keyATMDebug = "Debug";
    const char* keyTextureScale = "PreCalculatedTextureScale";
    const char* keySaveTextures = "SaveCalculatedTextures";
    const char* keyAtmosphere = "Atmosphere";
    const char* keyAtmosphereRadius = "AtmosphereRadius";
    const char* keyPlanetRadius = "PlanetRadius";
    const char* keyAverageGroundReflectance = "PlanetAverageGroundReflectance";
    const char* keyRayleigh = "Rayleigh";
    const char* keyRayleighHeightScale = "H_R";
    const char* keyOzone = "Ozone";
    const char* keyOzoneHeightScale = "H_O";
    const char* keyMie = "Mie";
    const char* keyMieHeightScale = "H_M";
    const char* keyMiePhaseConstant = "G";
    const char* keyImage = "Image";
    const char* keyToneMappingOp = "ToneMapping";
    const char* keyExposure = "Exposure";
    const char* keyBackground = "Background";
    const char* keyGamma = "Gamma";    
#endif
}

namespace openspace {

using namespace properties;

namespace globebrowsing {
    
RenderableGlobe::RenderableGlobe(const ghoul::Dictionary& dictionary)
    : _debugProperties({
        BoolProperty("saveOrThrowCamera", "save or throw camera", false),
        BoolProperty("showChunkEdges", "show chunk edges", false),
        BoolProperty("showChunkBounds", "show chunk bounds", false),
        BoolProperty("showChunkAABB", "show chunk AABB", false),
        BoolProperty("showHeightResolution", "show height resolution", false),
        BoolProperty("showHeightIntensities", "show height intensities", false),
        BoolProperty("performFrustumCulling", "perform frustum culling", true),
        BoolProperty("performHorizonCulling", "perform horizon culling", true),
        BoolProperty("levelByProjectedAreaElseDistance", "level by projected area (else distance)", true),
        BoolProperty("resetTileProviders", "reset tile providers", false),
        BoolProperty("toggleEnabledEveryFrame", "toggle enabled every frame", false),
        BoolProperty("collectStats", "collect stats", false),
        BoolProperty("limitLevelByAvailableData", "Limit level by available data", true),
        IntProperty("modelSpaceRenderingCutoffLevel", "Model Space Rendering Cutoff Level", 10, 1, 22)
    })
    , _generalProperties({
        BoolProperty("enabled", "Enabled", true),
        BoolProperty("performShading", "perform shading", true),
        BoolProperty("atmosphere", "atmosphere", false),
        BoolProperty("useAccurateNormals", "useAccurateNormals", false),
        FloatProperty("lodScaleFactor", "lodScaleFactor",10.0f, 1.0f, 50.0f),
        FloatProperty("cameraMinHeight", "cameraMinHeight", 100.0f, 0.0f, 1000.0f),
        FloatProperty("orenNayarRoughness", "orenNayarRoughness", 0.0f, 0.0f, 1.0f)
    })
    , _debugPropertyOwner("Debug")
    , _texturePropertyOwner("Textures")
    , _shadowEnabled(false)
#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    , _atmosphereProperties({
        FloatProperty("atmosphereHeight", "Atmosphere Height (KM)", 60.0f, 0.1f, 99.0f),
        FloatProperty("averageGroundReflectance", "Average Ground Reflectance (%)", 0.1f, 0.0f, 1.0f),
        FloatProperty("rayleighHeightScale", "Rayleigh Height Scale (KM)", 8.0f, 0.1f, 20.0f),
        FloatProperty("rayleighScatteringCoeffX", "Rayleigh Scattering Coeff X (x10e-3)", 1.0f, 0.01f, 100.0f),
        FloatProperty("rayleighScatteringCoeffY", "Rayleigh Scattering Coeff Y (x10e-3)", 1.0f, 0.01f, 100.0f),
        FloatProperty("rayleighScatteringCoeffZ", "Rayleigh Scattering Coeff Z (x10e-3)", 1.0f, 0.01f, 100.0f),        
        FloatProperty("ozoneLayerHeightScale", "Ozone Height Scale (KM)", 8.0f, 0.1f, 20.0f),
        FloatProperty("ozoneLayerCoeffX", "Ozone Layer Extinction Coeff X", 3.426f, 0.01f, 100.0f),
        FloatProperty("ozoneLayerCoeffY", "Ozone Layer Extinction Coeff Y", 8.298f, 0.01f, 100.0f),
        FloatProperty("ozoneLayerCoeffZ", "Ozone Layer Extinction Coeff Z", 0.356f, 0.01f, 100.0f),
        FloatProperty("mieHeightScale", "Mie Height Scale (KM)", 1.2f, 0.1f, 20.0f),
        FloatProperty("mieScatteringCoeffX", "Mie Scattering Coeff X (x10e-3)", 4.0f, 0.01f, 1000.0f),
        FloatProperty("mieScatteringCoeffY", "Mie Scattering Coeff Y (x10e-3)", 4.0f, 0.01f, 1000.0f),
        FloatProperty("mieScatteringCoeffZ", "Mie Scattering Coeff Z (x10e-3)", 4.0f, 0.01f, 1000.0f),
        FloatProperty("mieScatteringExtinctionPropCoefficient",
            "Mie Scattering/Extinction Proportion Coefficient (%)", 0.9f, 0.01f, 1.0f),
        FloatProperty("mieAsymmetricFactorG", "Mie Asymmetric Factor G", 0.85f, -1.0f, 1.0f),
        FloatProperty("sunIntensity", "Sun Intensity", 50.0f, 0.1f, 1000.0f),
        FloatProperty("hdrExposition", "HDR Exposition", 0.4f, 0.01f, 5.0f),
        FloatProperty("gamma", "Gamma Correction", 1.8f, 0.1f, 3.0f ),
        BoolProperty("ozone", "Ozone Layer Enabled", true)
    })
    , _atmospherePropertyOwner("Atmosphere")
    , _atmosphereRadius(0.f)
    , _atmospherePlanetRadius(0.f)
    , _planetAverageGroundReflectance(0.f)
    , _rayleighHeightScale(0.f)
    , _ozoneLayerHeightScale(0.f)
    , _mieHeightScale(0.f)
    , _miePhaseConstant(0.f)    
    , _rayleighScatteringCoeff(glm::vec3(0.f))
    , _ozoneLayerExtinctionCoeff(glm::vec3(0.f))
    , _mieScatteringCoeff(glm::vec3(0.f))
    , _mieExtinctionCoeff(glm::vec3(0.f))
    , _sunRadianceIntensity(50.0f)
    , _exposureConstant(0.4f)
    , _exposureBackgroundConstant(2.8f)
    , _gammaConstant(1.8f)
    , _atmosphereEnabled(false)
    , _saveCalculationsToTexture(false)
    , _preCalculatedTexturesScale(1.0)
#endif
{
    setName("RenderableGlobe");
        
    dictionary.getValue(keyFrame, _frame);

    // Read the radii in to its own dictionary
    glm::dvec3 radii;
    dictionary.getValue(keyRadii, radii);
    _ellipsoid = Ellipsoid(radii);
    setBoundingSphere(_ellipsoid.maximumRadius());

    // Ghoul can't read ints from lua dictionaries...
    double patchSegmentsd;
    dictionary.getValue(keySegmentsPerPatch, patchSegmentsd);
    int patchSegments = patchSegmentsd;

    // Init layer manager
    ghoul::Dictionary layersDictionary;
    if (!dictionary.getValue(keyLayers, layersDictionary)) {
        throw ghoul::RuntimeError(
            std::string(keyLayers) + " must be specified specified!");
    }

    _layerManager = std::make_shared<LayerManager>(layersDictionary);

    _chunkedLodGlobe = std::make_shared<ChunkedLodGlobe>(
        *this, patchSegments, _layerManager);
    //_pointGlobe = std::make_shared<PointGlobe>(*this);
        
    _distanceSwitch.addSwitchValue(_chunkedLodGlobe);
    //_distanceSwitch.addSwitchValue(_pointGlobe);
        
    addProperty(_generalProperties.isEnabled);
    addProperty(_generalProperties.atmosphereEnabled);
    addProperty(_generalProperties.performShading);
    addProperty(_generalProperties.useAccurateNormals);
    addProperty(_generalProperties.lodScaleFactor);
    addProperty(_generalProperties.cameraMinHeight);
    addProperty(_generalProperties.orenNayarRoughness);
        
    _debugPropertyOwner.addProperty(_debugProperties.saveOrThrowCamera);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkEdges);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkBounds);
    _debugPropertyOwner.addProperty(_debugProperties.showChunkAABB);
    _debugPropertyOwner.addProperty(_debugProperties.showHeightResolution);
    _debugPropertyOwner.addProperty(_debugProperties.showHeightIntensities);
    _debugPropertyOwner.addProperty(_debugProperties.performFrustumCulling);
    _debugPropertyOwner.addProperty(_debugProperties.performHorizonCulling);
    _debugPropertyOwner.addProperty(
        _debugProperties.levelByProjectedAreaElseDistance
    );
    _debugPropertyOwner.addProperty(_debugProperties.resetTileProviders);
    _debugPropertyOwner.addProperty(_debugProperties.toggleEnabledEveryFrame);
    _debugPropertyOwner.addProperty(_debugProperties.collectStats);
    _debugPropertyOwner.addProperty(_debugProperties.limitLevelByAvailableData);
    _debugPropertyOwner.addProperty(_debugProperties.modelSpaceRenderingCutoffLevel);
  
    auto notifyShaderRecompilation = [&](){
        _chunkedLodGlobe->notifyShaderRecompilation();
    };
    _generalProperties.atmosphereEnabled.onChange(notifyShaderRecompilation);
    _generalProperties.useAccurateNormals.onChange(notifyShaderRecompilation);
    _generalProperties.performShading.onChange(notifyShaderRecompilation);
    _debugProperties.showChunkEdges.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightResolution.onChange(notifyShaderRecompilation);
    _debugProperties.showHeightIntensities.onChange(notifyShaderRecompilation);

    _layerManager->onChange(notifyShaderRecompilation);

    addPropertySubOwner(_debugPropertyOwner);
    addPropertySubOwner(_layerManager.get());

    ghoul::Dictionary shadowDictionary;
    bool dicSuccess = dictionary.getValue(keyShadowGroup, shadowDictionary);
    bool disableShadows = false;
    if (dicSuccess) {
        std::vector< std::pair<std::string, float > > sourceArray;
        unsigned int sourceCounter = 1;
        while (dicSuccess) {
            std::string sourceName;
            std::stringstream ss;
            ss << keyShadowSource << sourceCounter << ".Name";
            dicSuccess = shadowDictionary.getValue(ss.str(), sourceName);
            if (dicSuccess) {
                float sourceRadius;
                ss.str(std::string());
                ss << keyShadowSource << sourceCounter << ".Radius";
                dicSuccess = shadowDictionary.getValue(ss.str(), sourceRadius);
                if (dicSuccess) {
                    sourceArray.push_back(std::pair< std::string, float>(
                        sourceName, sourceRadius));
                }
                else {
                    /*LWARNING("No Radius value expecified for Shadow Source Name "
                        << sourceName << " from " << name
                        << " planet.\nDisabling shadows for this planet.");*/
                    disableShadows = true;
                    break;
                }
            }
            sourceCounter++;
        }

        if (!disableShadows && !sourceArray.empty()) {
            dicSuccess = true;
            std::vector< std::pair<std::string, float > > casterArray;
            unsigned int casterCounter = 1;
            while (dicSuccess) {
                std::string casterName;
                std::stringstream ss;
                ss << keyShadowCaster << casterCounter << ".Name";
                dicSuccess = shadowDictionary.getValue(ss.str(), casterName);
                if (dicSuccess) {
                    float casterRadius;
                    ss.str(std::string());
                    ss << keyShadowCaster << casterCounter << ".Radius";
                    dicSuccess = shadowDictionary.getValue(ss.str(), casterRadius);
                    if (dicSuccess) {
                        casterArray.push_back(std::pair< std::string, float>(
                            casterName, casterRadius));
                    }
                    else {
                        /*LWARNING("No Radius value expecified for Shadow Caster Name "
                            << casterName << " from " << name
                            << " planet.\nDisabling shadows for this planet.");*/
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
    bool success = dictionary.getValue(keyAtmosphere, atmosphereDictionary);
    if (success) {
        if (!atmosphereDictionary.getValue(keyAtmosphereRadius, _atmosphereRadius)) {
            errorReadingAtmosphereData = true;
            //LWARNING("No Atmosphere Radius value expecified for Atmosphere Effects of "
            //    << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        if (!atmosphereDictionary.getValue(keyPlanetRadius, _atmospherePlanetRadius)) {
            errorReadingAtmosphereData = true;
            //LWARNING("No Planet Radius value expecified for Atmosphere Effects of "
            //    << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        if (!atmosphereDictionary.getValue(keyAverageGroundReflectance, _planetAverageGroundReflectance)) {
            errorReadingAtmosphereData = true;
            //LWARNING("No Average Atmosphere Ground Reflectance value expecified for Atmosphere Effects of "
            //    << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary rayleighDictionary;
        success = atmosphereDictionary.getValue(keyRayleigh, rayleighDictionary);

        if (success) {
            // Not using right now.
            glm::vec3 rayleighWavelengths;
            success = rayleighDictionary.getValue("Coefficients.Wavelengths", rayleighWavelengths);

            if (!rayleighDictionary.getValue("Coefficients.Scattering", _rayleighScatteringCoeff)) {
                errorReadingAtmosphereData = true;
                //LWARNING("No Rayleigh Scattering parameters expecified for Atmosphere Effects of "
                //    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!rayleighDictionary.getValue(keyRayleighHeightScale, _rayleighHeightScale)) {
                errorReadingAtmosphereData = true;
                //LWARNING("No Rayleigh Height Scale value expecified for Atmosphere Effects of "
                //    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }
        }
        else {
            errorReadingAtmosphereData = true;
            //LWARNING("No Rayleigh parameters expecified for Atmosphere Effects of "
            //    << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary ozoneDictionary;
        success = atmosphereDictionary.getValue(keyOzone, ozoneDictionary);
        if (success) {
            _ozoneLayerEnabled = true;
            if (!ozoneDictionary.getValue(keyOzoneHeightScale, _ozoneLayerHeightScale)) {
                _ozoneLayerEnabled = false;
            }

            if (!ozoneDictionary.getValue("Coefficients.Extinction", _ozoneLayerExtinctionCoeff)) {
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
                //LWARNING("No Mie Height Scale value expecified for Atmosphere Effects of "
                //    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue("Coefficients.Scattering", _mieScatteringCoeff)) {
                errorReadingAtmosphereData = true;
                //LWARNING("No Mie Scattering parameters expecified for Atmosphere Effects of "
                //    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue("Coefficients.Extinction", _mieExtinctionCoeff)) {
                errorReadingAtmosphereData = true;
                //LWARNING("No Mie Extinction parameters expecified for Atmosphere Effects of "
                //    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }

            if (!mieDictionary.getValue(keyMiePhaseConstant, _miePhaseConstant)) {
                errorReadingAtmosphereData = true;
                //LWARNING("No Mie Phase Constant value expecified for Atmosphere Effects of "
                //    << name << " planet.\nDisabling atmosphere effects for this planet.");
            }
        }
        else {
            errorReadingAtmosphereData = true;
            //LWARNING("No Mie parameters expecified for Atmosphere Effects of "
            //    << name << " planet.\nDisabling atmosphere effects for this planet.");
        }

        ghoul::Dictionary ImageDictionary;
        success = atmosphereDictionary.getValue(keyImage, ImageDictionary);
        if (success) {
            if (ImageDictionary.getValue(keyToneMappingOp, _preCalculatedTexturesScale)) {
                //LDEBUG("Atmosphere Texture Scaled to " << _preCalculatedTexturesScale);
            }

            if (ImageDictionary.getValue(keyExposure, _exposureConstant)) {
                //LDEBUG("Saving Precalculated Atmosphere Textures.");
            }

            if (ImageDictionary.getValue(keyBackground, _exposureBackgroundConstant)) {
                //LDEBUG("Saving Precalculated Atmosphere Textures.");
            }

            if (ImageDictionary.getValue(keyGamma, _gammaConstant)) {
                //LDEBUG("Saving Precalculated Atmosphere Textures.");
            }
        }

        ghoul::Dictionary debugATMDictionary;
        success = atmosphereDictionary.getValue(keyATMDebug, debugATMDictionary);
        if (success) {
            if (debugATMDictionary.getValue(keyTextureScale, _preCalculatedTexturesScale)) {
                //LDEBUG("Atmosphere Texture Scaled to " << _preCalculatedTexturesScale);
            }

            if (debugATMDictionary.getValue(keySaveTextures, _saveCalculationsToTexture)) {
                //LDEBUG("Saving Precalculated Atmosphere Textures.");
            }

        }

        if (!errorReadingAtmosphereData) {
            _atmosphereEnabled = true;

            //========================================================
            //============== Atmosphere Properties ===================
            //========================================================

            _atmosphereProperties.atmosphereHeightP.set(_atmosphereRadius - _atmospherePlanetRadius);
            _atmosphereProperties.atmosphereHeightP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.atmosphereHeightP);

            _atmosphereProperties.groundAverageReflectanceP.set(_planetAverageGroundReflectance);
            _atmosphereProperties.groundAverageReflectanceP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.groundAverageReflectanceP);

            _atmosphereProperties.rayleighHeightScaleP.set(_rayleighHeightScale);
            _atmosphereProperties.rayleighHeightScaleP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.rayleighHeightScaleP);

            _atmosphereProperties.rayleighScatteringCoeffXP.set(_rayleighScatteringCoeff.x * 1000.0f);
            _atmosphereProperties.rayleighScatteringCoeffXP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.rayleighScatteringCoeffXP);

            _atmosphereProperties.rayleighScatteringCoeffYP.set(_rayleighScatteringCoeff.y * 1000.0f);
            _atmosphereProperties.rayleighScatteringCoeffYP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.rayleighScatteringCoeffYP);

            _atmosphereProperties.rayleighScatteringCoeffZP.set(_rayleighScatteringCoeff.z * 1000.0f);
            _atmosphereProperties.rayleighScatteringCoeffZP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.rayleighScatteringCoeffZP);

            _atmosphereProperties.ozoneLayerEnabledP.set(_ozoneLayerEnabled);
            _atmosphereProperties.ozoneLayerEnabledP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.ozoneLayerEnabledP);

            _atmosphereProperties.ozoneHeightScaleP.set(_ozoneLayerHeightScale);
            _atmosphereProperties.ozoneHeightScaleP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.ozoneHeightScaleP);

            _atmosphereProperties.ozoneLayerExtinctionCoeffXP.set(_ozoneLayerExtinctionCoeff.x);
            _atmosphereProperties.ozoneLayerExtinctionCoeffXP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.ozoneLayerExtinctionCoeffXP);

            _atmosphereProperties.ozoneLayerExtinctionCoeffYP.set(_ozoneLayerExtinctionCoeff.y);
            _atmosphereProperties.ozoneLayerExtinctionCoeffYP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.ozoneLayerExtinctionCoeffYP);

            _atmosphereProperties.ozoneLayerExtinctionCoeffZP.set(_ozoneLayerExtinctionCoeff.z);
            _atmosphereProperties.ozoneLayerExtinctionCoeffZP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.ozoneLayerExtinctionCoeffZP);


            _atmosphereProperties.mieHeightScaleP.set(_mieHeightScale);
            _atmosphereProperties.mieHeightScaleP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.mieHeightScaleP);

            _atmosphereProperties.mieScatteringCoeffXP.set(_mieScatteringCoeff.x * 1000.0f);
            _atmosphereProperties.mieScatteringCoeffXP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.mieScatteringCoeffXP);

            _atmosphereProperties.mieScatteringCoeffYP.set(_mieScatteringCoeff.y * 1000.0f);
            _atmosphereProperties.mieScatteringCoeffYP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.mieScatteringCoeffYP);

            _atmosphereProperties.mieScatteringCoeffZP.set(_mieScatteringCoeff.z * 1000.0f);
            _atmosphereProperties.mieScatteringCoeffZP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.mieScatteringCoeffZP);

            _atmosphereProperties.mieScatteringExtinctionPropCoefficientP.set(_mieScatteringCoeff.r / _mieExtinctionCoeff.r);
            _atmosphereProperties.mieScatteringExtinctionPropCoefficientP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.mieScatteringExtinctionPropCoefficientP);

            _atmosphereProperties.mieAsymmetricFactorGP.set(_miePhaseConstant);
            _atmosphereProperties.mieAsymmetricFactorGP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.mieAsymmetricFactorGP);

            _atmosphereProperties.sunIntensityP.set(_sunRadianceIntensity);
            _atmosphereProperties.sunIntensityP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.sunIntensityP);

            _atmosphereProperties.hdrExpositionP.set(_exposureConstant);
            _atmosphereProperties.hdrExpositionP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.hdrExpositionP);
            
            _atmosphereProperties.gammaConstantP.set(_gammaConstant);
            _atmosphereProperties.gammaConstantP.onChange(std::bind(&RenderableGlobe::updateAtmosphereParameters, this));
            _atmospherePropertyOwner.addProperty(_atmosphereProperties.gammaConstantP);

            addPropertySubOwner(_atmospherePropertyOwner);
        }
    }
#endif
}

bool RenderableGlobe::initialize() {
#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_atmosphereEnabled) {
        _deferredcaster = std::make_unique<AtmosphereDeferredcaster>();
        if (_deferredcaster) {
            _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
            _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
            _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
            _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
            _deferredcaster->enableOzone(_ozoneLayerEnabled);
            _deferredcaster->setOzoneHeightScale(_ozoneLayerHeightScale);
            _deferredcaster->setMieHeightScale(_mieHeightScale);
            _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
            _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
            _deferredcaster->setHDRConstant(_exposureConstant);
            _deferredcaster->setBackgroundConstant(_exposureBackgroundConstant);
            _deferredcaster->setGammaConstant(_gammaConstant);
            _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
            _deferredcaster->setOzoneExtinctionCoefficients(_ozoneLayerExtinctionCoeff);
            _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
            _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
            _deferredcaster->setEllipsoidRadii(_ellipsoid.radii());
            _deferredcaster->setRenderableClass(AtmosphereDeferredcaster::RenderableGlobe);

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

    return _distanceSwitch.initialize();
}

bool RenderableGlobe::deinitialize() {
#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_deferredcaster) {
        OsEng.renderEngine().deferredcasterManager().detachDeferredcaster(*_deferredcaster.get());
        _deferredcaster = nullptr;
    }
#endif

    return _distanceSwitch.deinitialize();
}

bool RenderableGlobe::isReady() const {
    return true;
}

void RenderableGlobe::render(const RenderData& data, RendererTasks& tasks) {
    bool statsEnabled = _debugProperties.collectStats.value();
    _chunkedLodGlobe->stats.setEnabled(statsEnabled);

    if (_debugProperties.toggleEnabledEveryFrame.value()) {
        _generalProperties.isEnabled.setValue(
            !_generalProperties.isEnabled.value()
        );
    }
    if (_generalProperties.isEnabled.value()) {
        if (_debugProperties.saveOrThrowCamera.value()) {
            _debugProperties.saveOrThrowCamera.setValue(false);

            if (savedCamera() == nullptr) { // save camera
                setSaveCamera(std::make_shared<Camera>(data.camera));
            }
            else { // throw camera
                setSaveCamera(nullptr);
            }
        }
        _distanceSwitch.render(data);
    }
    if (_savedCamera != nullptr) {
        DebugRenderer::ref().renderCameraFrustum(data, *_savedCamera);
    }

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_atmosphereEnabled) {
        DeferredcasterTask task{ _deferredcaster.get(), data };
        tasks.deferredcasterTasks.push_back(task);
    }
#endif
}

void RenderableGlobe::update(const UpdateData& data) {
    _time = data.time.j2000Seconds();
    _distanceSwitch.update(data);

    glm::dmat4 translation =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation);
    glm::dmat4 rotation = glm::dmat4(data.modelTransform.rotation);
    glm::dmat4 scaling =
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale,
            data.modelTransform.scale, data.modelTransform.scale));

    _cachedModelTransform = translation * rotation * scaling;
    _cachedInverseModelTransform = glm::inverse(_cachedModelTransform);

    if (_debugProperties.resetTileProviders) {
        _layerManager->reset();
        _debugProperties.resetTileProviders = false;
    }
    _layerManager->update();
    _chunkedLodGlobe->update(data);

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
    if (_deferredcaster) {
        _deferredcaster->setTime(data.time.j2000Seconds());
        _deferredcaster->setModelTransform(_cachedModelTransform);

        if (_exposureBackgroundConstant != OsEng.renderEngine().renderer()->hdrBackground())
            updateAtmosphereParameters();
    }
#endif
}

glm::dvec3 RenderableGlobe::projectOnEllipsoid(glm::dvec3 position) {
    return _ellipsoid.geodeticSurfaceProjection(position);
}

float RenderableGlobe::getHeight(glm::dvec3 position) {
    if (_chunkedLodGlobe) {
        return _chunkedLodGlobe->getHeight(position);
    }
    else {
        return 0;
    }
}

std::shared_ptr<ChunkedLodGlobe> RenderableGlobe::chunkedLodGlobe() const{
    return _chunkedLodGlobe;
}

LayerManager* RenderableGlobe::layerManager() const {
    return _layerManager.get();
}

const Ellipsoid& RenderableGlobe::ellipsoid() const{
    return _ellipsoid;
}

const glm::dmat4& RenderableGlobe::modelTransform() const{
    return _cachedModelTransform;
}

const glm::dmat4& RenderableGlobe::inverseModelTransform() const{
    return _cachedInverseModelTransform;
}

const RenderableGlobe::DebugProperties&
    RenderableGlobe::debugProperties() const{
    return _debugProperties;
}
    
const RenderableGlobe::GeneralProperties&
    RenderableGlobe::generalProperties() const{
    return _generalProperties;
}

const std::shared_ptr<const Camera> RenderableGlobe::savedCamera() const {
    return _savedCamera;
}

SurfacePositionHandle RenderableGlobe::calculateSurfacePositionHandle(
                                                       const glm::dvec3& targetModelSpace) 
{
    glm::dvec3 centerToEllipsoidSurface =
        _ellipsoid.geodeticSurfaceProjection(targetModelSpace);
    glm::dvec3 ellipsoidSurfaceToTarget = targetModelSpace - centerToEllipsoidSurface;
    // ellipsoidSurfaceOutDirection will point towards the target, we want the outward
    // direction. Therefore it must be flipped in case the target is under the reference
    // ellipsoid so that it always points outwards
    glm::dvec3 ellipsoidSurfaceOutDirection = glm::normalize(ellipsoidSurfaceToTarget);
    if (glm::dot(ellipsoidSurfaceOutDirection, centerToEllipsoidSurface) < 0) {
        ellipsoidSurfaceOutDirection *= -1.0;
    }

    double heightToSurface = getHeight(targetModelSpace);
    heightToSurface = glm::isnan(heightToSurface) ? 0.0 : heightToSurface;
    centerToEllipsoidSurface = glm::isnan(glm::length(centerToEllipsoidSurface)) ?
        (glm::dvec3(0.0, 1.0, 0.0) * static_cast<double>(boundingSphere())) :
        centerToEllipsoidSurface;
    ellipsoidSurfaceOutDirection = glm::isnan(glm::length(ellipsoidSurfaceOutDirection)) ?
        glm::dvec3(0.0, 1.0, 0.0) : ellipsoidSurfaceOutDirection;

    return {
        centerToEllipsoidSurface,
        ellipsoidSurfaceOutDirection,
        heightToSurface
    };
}

void RenderableGlobe::setSaveCamera(std::shared_ptr<Camera> camera) { 
    _savedCamera = camera;
}

#ifdef OPENSPACE_MODULE_ATMOSPHERE_ENABLED
void RenderableGlobe::updateAtmosphereParameters() {
    bool executeComputation = true;
    if (_sunRadianceIntensity != _atmosphereProperties.sunIntensityP.value() ||
        _exposureConstant != _atmosphereProperties.hdrExpositionP.value() ||
        _exposureBackgroundConstant != OsEng.renderEngine().renderer()->hdrBackground() ||
        _gammaConstant != _atmosphereProperties.gammaConstantP.value())
        executeComputation = false;

    _atmosphereRadius               = _atmospherePlanetRadius + _atmosphereProperties.atmosphereHeightP.value();
    _planetAverageGroundReflectance = _atmosphereProperties.groundAverageReflectanceP.value();
    _rayleighHeightScale            = _atmosphereProperties.rayleighHeightScaleP.value();
    _rayleighScatteringCoeff        = glm::vec3(_atmosphereProperties.rayleighScatteringCoeffXP.value() * 0.001f, 
        _atmosphereProperties.rayleighScatteringCoeffYP.value() * 0.001f,
        _atmosphereProperties.rayleighScatteringCoeffZP.value() * 0.001f);
    _ozoneLayerEnabled         = _atmosphereProperties.ozoneLayerEnabledP.value();
    _ozoneLayerHeightScale     = _atmosphereProperties.ozoneHeightScaleP.value();
    _ozoneLayerExtinctionCoeff = glm::vec3(_atmosphereProperties.ozoneLayerExtinctionCoeffXP.value(),
        _atmosphereProperties.ozoneLayerExtinctionCoeffYP.value(),
        _atmosphereProperties.ozoneLayerExtinctionCoeffZP.value());
    _mieHeightScale       = _atmosphereProperties.mieHeightScaleP.value();
    _mieScatteringCoeff   = glm::vec3(_atmosphereProperties.mieScatteringCoeffXP.value() * 0.001f,
        _atmosphereProperties.mieScatteringCoeffYP.value() * 0.001f,
        _atmosphereProperties.mieScatteringCoeffZP.value() * 0.001f);
    _mieExtinctionCoeff   = _mieScatteringCoeff * (1.0f / static_cast<float>(_atmosphereProperties.mieScatteringExtinctionPropCoefficientP.value()));
    _miePhaseConstant     = _atmosphereProperties.mieAsymmetricFactorGP.value();
    _sunRadianceIntensity = _atmosphereProperties.sunIntensityP.value();
    _exposureConstant     = _atmosphereProperties.hdrExpositionP.value();
    _exposureBackgroundConstant = OsEng.renderEngine().renderer()->hdrBackground();   
    _gammaConstant        = _atmosphereProperties.gammaConstantP.value();

    if (_deferredcaster) {
        _deferredcaster->setAtmosphereRadius(_atmosphereRadius);
        _deferredcaster->setPlanetRadius(_atmospherePlanetRadius);
        _deferredcaster->setPlanetAverageGroundReflectance(_planetAverageGroundReflectance);
        _deferredcaster->setRayleighHeightScale(_rayleighHeightScale);
        _deferredcaster->enableOzone(_ozoneLayerEnabled);
        _deferredcaster->setOzoneHeightScale(_ozoneLayerHeightScale);
        _deferredcaster->setMieHeightScale(_mieHeightScale);
        _deferredcaster->setMiePhaseConstant(_miePhaseConstant);
        _deferredcaster->setSunRadianceIntensity(_sunRadianceIntensity);
        _deferredcaster->setHDRConstant(_exposureConstant);
        _deferredcaster->setBackgroundConstant(_exposureBackgroundConstant);
        _deferredcaster->setGammaConstant(_gammaConstant);
        _deferredcaster->setRayleighScatteringCoefficients(_rayleighScatteringCoeff);
        _deferredcaster->setOzoneExtinctionCoefficients(_ozoneLayerExtinctionCoeff);
        _deferredcaster->setMieScatteringCoefficients(_mieScatteringCoeff);
        _deferredcaster->setMieExtinctionCoefficients(_mieExtinctionCoeff);
        _deferredcaster->setRenderableClass(AtmosphereDeferredcaster::RenderableGlobe);

        if (executeComputation)
            _deferredcaster->preCalculateAtmosphereParam();
    }
}
#endif

} // namespace globebrowsing
} // namespace openspace
