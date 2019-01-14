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

#include <modules/dsn/rendering/renderabledsnlabels.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/glm.h>

namespace {
    constexpr const char* _loggerCat = "RenderableDsnLabels";
    constexpr const char* ProgramObjectName = "RenderableLabelsProgram";
    constexpr const char* KeyUnitOut = "FadeOutDistanceUnit";
    constexpr const char* KeyUnitIn = "FadeInDistanceUnit";
    constexpr const char* KeyUnitSize = "SizeDistanceUnit";

    constexpr const char* KeyObjectIdentifier = "ObjectIdentifier";
    constexpr const char* KeyLabelText = "LabelText";
    constexpr const char* KeyTextColor = "TextColor";
    constexpr const char* keyTimeFrame = "TimeFrame";

    constexpr openspace::properties::Property::PropertyInfo LabelIdentifierMapInfo = {
        "LabelIdentifierMap",
        "Label Identifier Map",
        "The mapping of identifiers to text if we want to attach labels to scenegraphnodes "
        "instead of reading positions from file. "
    };

    /* This toggles the distance from the camera of the projection plane that the label is rendered on.
     * Necessary to combat precision errors for labels on astronomical objects that are far out in space */
    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that toggles the distance of " 
        "the label from the camera. "
    };

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the label of the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelSizeInfo = {
        "LabelSize",
        "Label Size",
        "The static label size if no LabelSizeRange interval are set. "
    };

    constexpr openspace::properties::Property::PropertyInfo LabelSizeRangeInfo = {
        "LabelSizeRange",
        "Label Size Range",
        "These values determine the min and max size of this label when it is "
        "scaled depending on distance. Also need to specify the distance range from "
        "the astronomical object the rescaling occurs. "
    };

    constexpr openspace::properties::Property::PropertyInfo SizeDistanceRangeInfo = {
        "SizeDistanceRange",
        "Size Distance Range",
        "The distance range where rescaling from min label size to max label size occurs. "
    };

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden."
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInDistanceRangeInfo = {
        "FadeInDistanceRange",
        "Fade-In Distance Range",
        "These values determine the start and end of the fade expressed in a distance "
        "from the astronomical object. {start, end}"
    };

    constexpr openspace::properties::Property::PropertyInfo FadeOutDistanceRangeInfo = {
        "FadeOutDistanceRange",
        "Fade-Out Distance Range",
        "These values determine the start and end of the fade expressed in a distance "
        "from the astronomical object. {start, end}"
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeDistancesInfo = {
        "DisableFadeDistances",
        "Disable Fade-in and Fade-out effects",
        "Enables/Disables the Fade effects."
    };

}  // namespace

namespace openspace {

    documentation::Documentation RenderableDsnLabels::Documentation() {
        using namespace documentation;
        return {
            "RenderableDsnLabels",
            "dsn_renderable_renderabledsnlabels",
            {
                {
                    "Type",
                    new StringEqualVerifier("RenderableDsnLabels"),
                    Optional::No
                },
                {
                    LabelIdentifierMapInfo.identifier,
                    new TableVerifier,
                    Optional::No,
                    LabelIdentifierMapInfo.description
                },
                {
                    ScaleFactorInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    ScaleFactorInfo.description
                },
                {
                    DrawLabelInfo.identifier,
                    new BoolVerifier,
                    Optional::Yes,
                    DrawLabelInfo.description
                },
                {
                    TextColorInfo.identifier,
                    new DoubleVector4Verifier,
                    Optional::Yes,
                    TextColorInfo.description
                },
                {
                    LabelSizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    LabelSizeInfo.description
                },
                {
                    SizeDistanceRangeInfo.identifier,
                    new Vector2Verifier<double>,
                    Optional::Yes,
                    SizeDistanceRangeInfo.description
                },
                {
                    FadeInDistanceRangeInfo.identifier,
                    new Vector2Verifier<double>,
                    Optional::Yes,
                    FadeInDistanceRangeInfo.description
                },
                {
                    DisableFadeDistancesInfo.identifier,
                    new BoolVerifier,
                    Optional::Yes,
                    DisableFadeDistancesInfo.description
                },
                {
                    TransformationMatrixInfo.identifier,
                    new Matrix4x4Verifier<double>,
                    Optional::Yes,
                    TransformationMatrixInfo.description
                }
            }
        };
    }

    RenderableDsnLabels::RenderableDsnLabels(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _scaleFactor(ScaleFactorInfo, 1.0f, 0.f, 10.f)
        , _textColorProperty(
            TextColorInfo,
            _defaultTextColor,
            glm::vec4(0.0),
            glm::vec4(1.0)
        )
        , _labelSize(LabelSizeInfo, 20.0, 0.0, 100.0)
        , _labelSizeRange(
            LabelSizeRangeInfo,
            glm::vec2(0.0f),
            glm::vec2(0.0),
            glm::vec2(100.0)
        )
        , _sizeDistanceRange(
            SizeDistanceRangeInfo,
            glm::vec2(0, 100.0f),
            glm::vec2(0.0),
            glm::vec2(100.0)
        )
        , _drawLabels(DrawLabelInfo, false)
        , _fadeOutDistance(
            FadeOutDistanceRangeInfo,
            glm::vec2(0.01f, 1.0f),
            glm::vec2(0.0),
            glm::vec2(10.0)
        )
        , _fadeInDistance(
            FadeInDistanceRangeInfo,
            glm::vec2(0.01f, 1.0f),
            glm::vec2(0.0),
            glm::vec2(10.0)
        )
        , _disableFadeDistances(DisableFadeDistancesInfo, false)
        , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    {
        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableDsnLabels"
        );

        // DEBUG:
        _renderOption.addOption(0, "Camera View Direction");
        _renderOption.addOption(1, "Camera Position Normal");
        _renderOption.set(1);

        addProperty(_renderOption);

        if (dictionary.hasKey(ScaleFactorInfo.identifier)) {
            _scaleFactor = static_cast<float>(
                dictionary.value<double>(ScaleFactorInfo.identifier)
                );
        }
        addProperty(_scaleFactor);

        if (dictionary.hasKey(LabelIdentifierMapInfo.identifier)) {

            ghoul::Dictionary tempLabelIdMap = dictionary.value<ghoul::Dictionary>(LabelIdentifierMapInfo.identifier);

            std::vector<std::string> labels = tempLabelIdMap.keys();

            for (int i = 0; i < labels.size(); i++) {
                ghoul::Dictionary labelInfoDictionary = tempLabelIdMap.value<ghoul::Dictionary>(labels.at(i));
                LabelInfo labelInfo;
                labelInfo.text = labelInfoDictionary.value<std::string>(KeyLabelText);
                
                if (labelInfoDictionary.hasKey(keyTimeFrame)) {
                    ghoul::Dictionary timeFrameDictionary = labelInfoDictionary.value<ghoul::Dictionary>(keyTimeFrame);

                    std::string startTime = timeFrameDictionary.value <std::string>("Start");
                    std::string endTime = timeFrameDictionary.value <std::string>("End");

                    labelInfo.startTime = Time::convertTime(startTime);
                    labelInfo.endTime = Time::convertTime(endTime);

                    labelInfo.hasKeyTimeFrame = true;
                }

                if (labelInfoDictionary.hasKey(KeyTextColor)) {
                    labelInfo.textColor = labelInfoDictionary.value<glm::vec4>(KeyTextColor);
                    labelInfo.hasIndividualColor = true;
                }
                else if (dictionary.hasKeyAndValue<glm::vec4>(TextColorInfo.identifier)) {
                    labelInfo.textColor = dictionary.value<glm::vec4>(KeyTextColor);
                }
                else {
                    labelInfo.textColor = _defaultTextColor;
                }

                labelInfo.attachedId = labelInfoDictionary.value<std::string>(KeyObjectIdentifier);
                labelDataInfo.push_back(labelInfo);
            }
            
            _hasLabel = true;
            _hasLabelIdMap = true;

            if (dictionary.hasKey(DrawLabelInfo.identifier)) {
                _drawLabels = dictionary.value<bool>(DrawLabelInfo.identifier);
            }
            addProperty(_drawLabels);

            if (dictionary.hasKeyAndValue<glm::vec4>(TextColorInfo.identifier)) {

                glm::vec4 labelMapTextColor = dictionary.value<glm::vec4>(KeyTextColor);
                _textColorProperty.setViewOption(properties::Property::ViewOptions::Color);
                _textColorProperty.setValue(labelMapTextColor);
                addProperty(_textColorProperty);
                _textColorProperty.onChange([&]() { _textColorIsDirty = true; });
            }

            // Can have either a static size or a scaled interval
            if (dictionary.hasKey(LabelSizeInfo.identifier)) {
                _labelSize = dictionary.value<float>(LabelSizeInfo.identifier);
                addProperty(_labelSize);
                _hasStaticLabelSize = true;

            }else if(dictionary.hasKey(LabelSizeRangeInfo.identifier) && dictionary.hasKey(SizeDistanceRangeInfo.identifier))
            {
                glm::vec2 labelsizeRange = dictionary.value<glm::vec2>(LabelSizeRangeInfo.identifier);
                _labelSizeRange.set(labelsizeRange);
                addProperty(_labelSizeRange);

                if (dictionary.hasKey(KeyUnitSize)) {
                    std::string unit = dictionary.value<std::string>(KeyUnitOut);
                    _sizeDistanceUnit = getUnitFactor(unit);
                }

                glm::vec2 sizeDistanceRange = dictionary.value<glm::vec2>(SizeDistanceRangeInfo.identifier);
                _sizeDistanceRange.set(sizeDistanceRange);
                addProperty(_sizeDistanceRange);

            }
            else {
                LERROR(fmt::format("Need to specify either a static '{}' or a scalable range '{}' with a distance range '{}'", 
                                    LabelSizeInfo.identifier, LabelSizeRangeInfo.identifier, SizeDistanceRangeInfo.identifier));
            }

        }
        else {
            LERROR(fmt::format("Needs a valid {}", LabelIdentifierMapInfo.identifier));
            _hasLabel = false;
        }

        if (dictionary.hasKey(TransformationMatrixInfo.identifier)) {
            _transformationMatrix = dictionary.value<glm::dmat4>(
                TransformationMatrixInfo.identifier
                );
        }

        if (dictionary.hasKey(FadeInDistanceRangeInfo.identifier) && dictionary.hasKey(FadeOutDistanceRangeInfo.identifier)) {
            
            glm::vec2 fadeInValue = dictionary.value<glm::vec2>(
                FadeInDistanceRangeInfo.identifier
                );
            _fadeInDistance.set(fadeInValue);

            if (dictionary.hasKey(KeyUnitIn)) {
                std::string unit = dictionary.value<std::string>(KeyUnitIn);
                _fadeInDistanceUnit = getUnitFactor(unit);
            }

            glm::vec2 fadeOutValue = dictionary.value<glm::vec2>(
                FadeOutDistanceRangeInfo.identifier
                );
            _fadeOutDistance.set(fadeOutValue);

            if (dictionary.hasKey(KeyUnitOut)) {
                std::string unit = dictionary.value<std::string>(KeyUnitOut);
                _fadeOutDistanceUnit = getUnitFactor(unit);
            }

            _disableFadeDistances.set(false);
            addProperty(_fadeInDistance);
            addProperty(_fadeOutDistance);
            addProperty(_disableFadeDistances);
        }

    }

    bool RenderableDsnLabels::isReady() const {
        return ( !_labelData.empty() );
    }

    void RenderableDsnLabels::initialize() {
        bool success = loadData(0);
        if (!success) {
            throw ghoul::RuntimeError("Error with identifiers for labels");
        }
    }

    void RenderableDsnLabels::initializeGL() {

        if (_hasLabel) {
            if (_font == nullptr) {
                size_t _fontSize = 50;
                _font = global::fontManager.font(
                    "Mono",
                    static_cast<float>(_fontSize),
                    ghoul::fontrendering::FontManager::Outline::Yes,
                    ghoul::fontrendering::FontManager::LoadGlyphs::No
                );
            }
        }
    }

    /* To combat precision errors when we approach a node very far out in space
     * we place the label on a set distance from the camera instead of at the node's 
     * actual world position */
    void RenderableDsnLabels::renderLabels(const RenderData& data,
        const glm::dmat4& modelViewProjectionMatrix,
        const glm::dvec3& orthoRight,
        const glm::dvec3& orthoUp)
    {

        if (_hasLabelIdMap) {
            _labelData.clear();
            loadLabelDataFromId(data.time);
        }
        for (const std::tuple<glm::dvec3, std::string, glm::vec4>& label : _labelData) {

            // The world position of the SceneGraphNode
            glm::dvec3 nodePos = std::get<0>(label);
            std::string labelText = std::get<1>(label);
            glm::vec4 labelTextColor = std::get<2>(label);

            glm::vec4 textColor = labelTextColor;

            // The distance from the camera to the SceneGraphNode of the label
            double distCamera = glm::distance(data.camera.positionVec3(), nodePos);

      
            if (!_disableFadeDistances) {
                // fade in when when we are close enough
                const glm::vec2 fadeInRange = _fadeInDistance;
                double fadeIn = maxMinNormalize(distCamera, glm::dvec2(1.0, 0.0), glm::dvec2(_fadeInDistanceUnit*fadeInRange.x, _fadeInDistanceUnit*fadeInRange.y));

                // fade out again when we get really close
                const glm::vec2 fadeOutRange = _fadeOutDistance;
                double fadeOut = maxMinNormalize(distCamera, glm::dvec2(0.0, 1.0), glm::dvec2(_fadeOutDistanceUnit*fadeOutRange.x, _fadeOutDistanceUnit*fadeOutRange.y));

                textColor.a *= glm::min(fadeIn, fadeOut);
            }


            double textSize = 0.0;
            if (!_hasStaticLabelSize) {
                const glm::vec2 labelSizeRange = _labelSizeRange;
                const glm::vec2 sizeRange = _sizeDistanceRange;

                // Pass in the labelSizeRanges in opposite order (max,min) since we want the largest value 
                // when the distance is the smallest
                textSize = maxMinNormalize(distCamera, glm::dvec2(labelSizeRange.y, labelSizeRange.x), glm::dvec2(_sizeDistanceUnit*sizeRange.x, _sizeDistanceUnit*sizeRange.y));
            }
            else {
                textSize = _labelSize;
            }
            double labelPosLength = pow(10, _scaleFactor);
            // The direction vector from the camera to the SceneGraphNode
            glm::dvec3 nodeDir = normalize(data.camera.positionVec3() - nodePos);
            // The new label position vector, calculated from the camera
            glm::dvec3 labelPos = data.camera.positionVec3() - (nodeDir * labelPosLength);


            double textScale = pow(10, textSize);

            ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
                *_font,
                labelPos,
                labelText,
                textColor,
                textScale,
                static_cast<int>(_textMinSize),
                static_cast<int>(_textMaxSize),
                modelViewProjectionMatrix,
                orthoRight,
                orthoUp,
                data.camera.positionVec3(),
                data.camera.lookUpVectorWorldSpace(),
                _renderOption.value(),
                glm::vec2(_offset)
            );
        }
    }

    void RenderableDsnLabels::updateTextColor()
    {
        for (int i = 0; i < labelDataInfo.size(); i++) {

            if (!labelDataInfo.at(i).hasIndividualColor) {
                labelDataInfo.at(i).textColor = _textColorProperty.value();       
            }
        }
        _textColorIsDirty = false;
    }

    void RenderableDsnLabels::render(const RenderData& data, RendererTasks&) {

        glm::dmat4 modelMatrix =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
            glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

        glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
        glm::mat4 projectionMatrix = data.camera.projectionMatrix();

        glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) * modelViewMatrix;

        glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
        glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
        glm::dvec3 orthoRight = glm::normalize(
            glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
        );
        if (orthoRight == glm::dvec3(0.0)) {
            glm::dvec3 otherVector(
                cameraUpDirectionWorld.y,
                cameraUpDirectionWorld.x,
                cameraUpDirectionWorld.z
            );
            orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
        }
        glm::dvec3 orthoUp = glm::normalize(
            glm::cross(cameraViewDirectionWorld, orthoRight)
        );

        if (_textColorIsDirty) {
        
            updateTextColor();
        }

        if (_drawLabels && _hasLabel) {
            renderLabels(
                data,
                modelViewProjectionMatrix,
                orthoRight,
                orthoUp
            );
        }
    }

    bool RenderableDsnLabels::loadData(const Time& time) {
        bool success = true;

        if (_hasLabelIdMap) {
            
            success &= loadLabelDataFromId(time);
        }
        return success;
    }

    bool RenderableDsnLabels::loadLabelDataFromId(const Time& time) {
        for (int i = 0; i < labelDataInfo.size(); i++) {
            LabelInfo labelinfo = labelDataInfo.at(i);
            double currentTime = time.j2000Seconds();

            if (!global::renderEngine.scene()->sceneGraphNode(labelinfo.attachedId)) {
                LERROR(fmt::format("No SceneGraphNode found with identifier {}", labelinfo.attachedId));
                return false;
            }
            // if the label is timeframe active
            bool labelIsInTimeFrame = (currentTime > labelinfo.startTime && currentTime < labelinfo.endTime);
            // if the node it is attached to is timeframe active
            bool nodeIsInTimeFrame = global::renderEngine.scene()->sceneGraphNode(labelinfo.attachedId)->isTimeFrameActive(time);

            if ( nodeIsInTimeFrame && 
                ((labelinfo.hasKeyTimeFrame && labelIsInTimeFrame) || !labelinfo.hasKeyTimeFrame )) {

                glm::dvec3 position = global::renderEngine.scene()->sceneGraphNode(labelinfo.attachedId)->worldPosition();

                glm::dvec3 transformedPos = glm::dvec3(
                    _transformationMatrix * glm::dvec4(position, 1.0)
                );
                _labelData.emplace_back(std::make_tuple(transformedPos, labelinfo.text, labelinfo.textColor));            
            }
        }

        return true;
    }

    double RenderableDsnLabels::maxMinNormalize(double value, glm::dvec2 newRange, glm::dvec2 oldRange)
    {

        double newMax = newRange.y;
        double newMin = newRange.x;

        double oldMax = glm::max(oldRange.y,oldRange.x);
        double oldMin = glm::min(oldRange.y, oldRange.x);

        if (value >= oldMax)
            return newMax;
        if (value <= oldMin)
            return newMin;

        double nominator = (newMax - newMin) * (value - oldMax);
        double denominator = oldMax - oldMin;

        double newValue = nominator / denominator + newMax;

        return newValue;
    }

    double RenderableDsnLabels::getUnitFactor(std::string unitString)
    {
        double unit = 0.0;

        std::transform(unitString.begin(), unitString.end(), unitString.begin(), ::tolower);
        
        // Units expressed in meters
        if (unitString == "meter" ||
            unitString == "meters" || 
            unitString == "m") {
            unit = 1.0;
        }
        else if (unitString == "kilometer" ||
            unitString == "kilometers" || 
            unitString == "km") {
            unit = 1E3;
        }
        else if (unitString == "lightsecond" ||
            unitString == "lightseconds") {
            unit = 2.998E8;
        }
        else if (unitString == "lightminute" || 
                 unitString == "lightminutes") {
            unit = 1.799E10;
        }
        else if (unitString == "au") {
            unit = 1.496E11;
        }
        else if (unitString == "lighthour" ||
            unitString == "lighthours") {
            unit = 1.079E12;
        }
        else if (unitString == "lightday" ||
                unitString == "lightdays") {
            unit = 2.59E13;
        }
        else {
            LWARNING(
                fmt::format("{} is not a specified unit. Using meters as units.", unitString)
            );
            unit = 1.0;
        }

        return unit;
    }

} // namespace openspace
