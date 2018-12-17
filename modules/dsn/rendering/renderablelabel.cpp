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

#include <modules/dsn/rendering/renderablelabel.h>

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
    constexpr const char* _loggerCat = "RenderableLabel";
    constexpr const char* ProgramObjectName = "RenderableLabel";

    constexpr openspace::properties::Property::PropertyInfo LabelIdentifierMapInfo = {
        "LabelIdentifierMap",
        "Label Identifier Map",
        "The mapping of identifiers to text if we want to attach labels to scenegraphnodes "
        "instead of reading positions from file. "
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point."
    };

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelSizeInfo = {
        "LabelSize",
        "Label Size",
        "The static label size if no LabelSizeRange interval are set "
    };

    //constexpr openspace::properties::Property::PropertyInfo TextMinSizeInfo = {
    //    "TextMinSize",
    //    "Text Min Size",
    //    "The minimal size (in pixels) of the text for the labels for the astronomical "
    //    "objects being rendered."
    //};

    //constexpr openspace::properties::Property::PropertyInfo TextMaxSizeInfo = {
    //    "TextMaxSize",
    //    "Text Max Size",
    //    "The maximum size (in pixels) of the text for the labels for the astronomical "
    //    "objects being rendered."
    //};
    constexpr openspace::properties::Property::PropertyInfo LabelSizeRangeInfo = {
        "LabelSizeRange",
        "Label Size Range",
        "These values determine the min and max size of this label when it is "
        "scaled depending on distance"
    };

        constexpr openspace::properties::Property::PropertyInfo TextMinSizeInfo = {
        "TextMinSize",
        "Text Min Size",
        "The minimal size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo TextMaxSizeInfo = {
        "TextMaxSize",
        "Text Max Size",
        "The maximum size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
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

    constexpr openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the astronomical object will start and end "
        "fading-in."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInInfo = {
        "DisableFadeIn",
        "Disable Fade-in effect",
        "Enables/Disables the Fade-in effect."
    };

}  // namespace

namespace openspace {

    documentation::Documentation RenderableLabel::Documentation() {
        using namespace documentation;
        return {
            "RenderableLabel",
            "dsn_renderable_renderablelabel",
            {
                {
                    "Type",
                    new StringEqualVerifier("RenderableLabel"),
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
                    TextMinSizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    TextMinSizeInfo.description
                },
                {
                    TextMaxSizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    TextMaxSizeInfo.description
                },
                {
                    FadeInDistancesInfo.identifier,
                    new Vector2Verifier<double>,
                    Optional::Yes,
                    FadeInDistancesInfo.description
                },
                {
                    DisableFadeInInfo.identifier,
                    new BoolVerifier,
                    Optional::Yes,
                    DisableFadeInInfo.description
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

    RenderableLabel::RenderableLabel(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary)
        , _scaleFactor(ScaleFactorInfo, 5.0f, 1.f, 100.f)
        , _textColor(
            TextColorInfo,
            glm::vec4(1.0f, 1.0, 1.0f, 1.f),
            glm::vec4(0.f),
            glm::vec4(1.f)
        )
        , _labelSize(LabelSizeInfo, 20.0, 0.0, 100.0)
        , _labelSizeRange(
            LabelSizeRangeInfo,
            glm::vec2(0.0f),
            glm::vec2(0.0),
            glm::vec2(100.0)
        )
       // , _textMinSize(TextMinSizeInfo, 5.f, 0.0f, 100.f)
       // , _textMaxSize(TextMaxSizeInfo, 50.f, 0.0f, 100.f)
        , _drawLabels(DrawLabelInfo, false)
        , _fadeInDistance(
            FadeInDistancesInfo,
            glm::vec2(0.0f),
            glm::vec2(0.0),
            glm::vec2(100.0)
        )
        , _disableFadeInDistance(DisableFadeInInfo, true)
        , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    {
        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableLabel"
        );

        // DEBUG:
        _renderOption.addOption(0, "Camera View Direction");
        _renderOption.addOption(1, "Camera Position Normal");
        _renderOption.set(1);
        if (global::windowDelegate.isFisheyeRendering()) {
            _renderOption.set(1);
        }
        else {
            _renderOption.set(0);
        }
        addProperty(_renderOption);

        if (dictionary.hasKey(ScaleFactorInfo.identifier)) {
            _scaleFactor = static_cast<float>(
                dictionary.value<double>(ScaleFactorInfo.identifier)
                );
        }
        addProperty(_scaleFactor);

        if (dictionary.hasKey(LabelIdentifierMapInfo.identifier)) {

            _labelIdMap = dictionary.value<ghoul::Dictionary>(LabelIdentifierMapInfo.identifier);

            _hasLabel = true;
            _hasLabelIdMap = true;

            if (dictionary.hasKey(DrawLabelInfo.identifier)) {
                _drawLabels = dictionary.value<bool>(DrawLabelInfo.identifier);
            }
            addProperty(_drawLabels);

            if (dictionary.hasKey(TextColorInfo.identifier)) {
                _textColor = dictionary.value<glm::vec4>(TextColorInfo.identifier);
            }
            _textColor.setViewOption(properties::Property::ViewOptions::Color);
            addProperty(_textColor);
            _textColor.onChange([&]() { _textColorIsDirty = true; });

            // Can have either a static size or a scaled interval
            if (dictionary.hasKey(LabelSizeInfo.identifier)) {
                _labelSize = dictionary.value<float>(LabelSizeInfo.identifier);
                addProperty(_labelSize);
                _hasStaticLabelSize = true;

            }else if(dictionary.hasKey(LabelSizeRangeInfo.identifier)) 
            {
                glm::vec2 labelsizeRange = dictionary.value<glm::vec2>(LabelSizeRangeInfo.identifier);
                _labelSizeRange.set(labelsizeRange);
                addProperty(_labelSizeRange);
            }

            //if (dictionary.hasKey(TextMinSizeInfo.identifier)) {
            //    _textMinSize = dictionary.value<float>(TextMinSizeInfo.identifier);
            //}
            //addProperty(_textMinSize);

            //if (dictionary.hasKey(TextMaxSizeInfo.identifier)) {
            //    _textMaxSize = dictionary.value<float>(TextMaxSizeInfo.identifier);
            //}
            //addProperty(_textMaxSize);

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

        if (dictionary.hasKey(FadeInDistancesInfo.identifier)) {
            glm::vec2 fadeInValue = dictionary.value<glm::vec2>(
                FadeInDistancesInfo.identifier
                );
            _fadeInDistance.set(fadeInValue);
            _disableFadeInDistance.set(false);
        }
        addProperty(_fadeInDistance);
        addProperty(_disableFadeInDistance);
    }

    bool RenderableLabel::isReady() const {
        return ( !_labelData.empty() );
    }

    void RenderableLabel::initialize() {
        bool success = loadData();
        if (!success) {
            throw ghoul::RuntimeError("Error with identifiers for labels");
        }
    }

    void RenderableLabel::initializeGL() {

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
    void RenderableLabel::renderLabels(const RenderData& data,
        const glm::dmat4& modelViewProjectionMatrix,
        const glm::dvec3& orthoRight,
        const glm::dvec3& orthoUp)
    {

        if (_hasLabelIdMap) {
            _labelData.clear();
            loadLabelDataFromId();
        }
        glm::vec4 textColor = _textColor;
       
        //float fadeInVariable = 1.f;
        //if (!_disableFadeInDistance) {
        //    float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        //    const glm::vec2 fadeRange = _fadeInDistance;
        //    const float a = 1.f / (fadeRange.y - fadeRange.x);
        //    const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        //    const float funcValue = a * distCamera + b;
        //    fadeInVariable *= funcValue > 1.f ? 1.f : funcValue;

        //    if (funcValue < 0.01f) {
        //        return;
        //    }
        //}

        for (const std::pair<glm::dvec3, std::string>& pair : _labelData) {

            // The world position of the SceneGraphNode
            glm::dvec3 nodePos = pair.first;
            std::string labelText = pair.second;

            double distCamera = glm::distance(data.camera.positionVec3(), nodePos);

            double textSize = 0.0;
            if (!_hasStaticLabelSize) {
                const glm::vec2 labelSizeRange = _labelSizeRange;
                // Pass in the labelSizeRanges in opposite order (max,min) since we want the largest value 
                // when the distance is the smallest
                textSize = maxMinNormalize(distCamera, glm::dvec2(labelSizeRange.y, labelSizeRange.x), glm::dvec2(20000.0, _maxDistanceUnit));
            }
            else {
                textSize = _labelSize;
            }
            double labelPosLength = (1.0 / _scaleFactor) * _maxDistanceUnit;
            // The direction vector from the camera to the SceneGraphNode
            glm::dvec3 nodeDir = normalize(data.camera.positionVec3() - nodePos);
            // The new label position vector, calculated from the camera
            glm::dvec3 labelPos = data.camera.positionVec3() - (nodeDir * labelPosLength);

            //textColor.a *= fadeInVariable;
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
                _renderOption.value()
            );
        }
    }

    double RenderableLabel::maxMinNormalize(double value, glm::dvec2 newRange, glm::dvec2 oldRange)
    {   
        double newMax = newRange.y;
        double newMin = newRange.x;

        double oldMax = oldRange.y;
        double oldMin = oldRange.x;

        if (value >= oldMax)
            return newMax;

        double nominator = (newMax - newMin) * (value - oldMax);
        double denominator = oldMax - oldMin;

        double newValue = nominator / denominator + newMax;

        return newValue;
    }

    void RenderableLabel::render(const RenderData& data, RendererTasks&) {

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

        if (_drawLabels && _hasLabel) {
            renderLabels(
                data,
                modelViewProjectionMatrix,
                orthoRight,
                orthoUp
            );
        }
    }

    bool RenderableLabel::loadData() {
        bool success = true;

        if (_hasLabelIdMap) {

            success &= loadLabelDataFromId();
        }
        return success;
    }


    bool RenderableLabel::loadLabelDataFromId() {

        std::vector<std::string> keys = _labelIdMap.keys();

        std::string id = "";

        for (int i = 0; i < keys.size(); i++)
        {
            id = keys.at(i);
            std::string label = _labelIdMap.value<std::string>(keys.at(i));

            if (global::renderEngine.scene()->sceneGraphNode(id)) {
                glm::dvec3 position = global::renderEngine.scene()->sceneGraphNode(id)->worldPosition();

                glm::dvec3 transformedPos = glm::dvec3(
                    _transformationMatrix * glm::dvec4(position, 1.0)
                );
                _labelData.emplace_back(std::make_pair(transformedPos, label));

            }
            else {
                LERROR(fmt::format("No SceneGraphNode found with identifier {}", id));
                return false;
            }

        }

        return true;
    }

} // namespace openspace
