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

    constexpr openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMinSizeInfo = {
        "TextMinSize",
        "Text Min Size",
        "The minimal size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMaxSizeInfo = {
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
                    TextSizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    TextSizeInfo.description
                },
                {
                    LabelMinSizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    LabelMinSizeInfo.description
                },
                {
                    LabelMaxSizeInfo.identifier,
                    new DoubleVerifier,
                    Optional::Yes,
                    LabelMaxSizeInfo.description
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
        , _scaleFactor(ScaleFactorInfo, 10.f, 0.f, 600.f)
        , _textColor(
            TextColorInfo,
            glm::vec4(1.0f, 1.0, 1.0f, 1.f),
            glm::vec4(0.f),
            glm::vec4(1.f)
        )
        , _textSize(TextSizeInfo, 8.0, 0.5, 24.0)
        , _textMinSize(LabelMinSizeInfo, 8.f, 0.5f, 24.f)
        , _textMaxSize(LabelMaxSizeInfo, 20.f, 0.5f, 100.f)
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

            if (dictionary.hasKey(TextSizeInfo.identifier)) {
                _textSize = dictionary.value<float>(TextSizeInfo.identifier);
            }
            addProperty(_textSize);

            if (dictionary.hasKey(LabelMinSizeInfo.identifier)) {
                _textMinSize = dictionary.value<float>(LabelMinSizeInfo.identifier);
            }
            addProperty(_textMinSize);

            if (dictionary.hasKey(LabelMaxSizeInfo.identifier)) {
                _textMaxSize = dictionary.value<float>(LabelMaxSizeInfo.identifier);
            }
            addProperty(_textMaxSize);
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
            throw ghoul::RuntimeError("Error loading data");
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

    void RenderableLabel::renderLabels(const RenderData& data,
        const glm::dmat4& modelViewProjectionMatrix,
        const glm::dvec3& orthoRight,
        const glm::dvec3& orthoUp,
        float fadeInVariable)
    {
        float scale = 1.f;

        if (_hasLabelIdMap) {
            _labelData.clear();
            loadLabelDataFromId();
        }
        glm::vec4 textColor = _textColor;
        textColor.a *= fadeInVariable;

        for (const std::pair<glm::dvec3, std::string>& pair : _labelData) {
            //glm::vec3 scaledPos(_transformationMatrix * glm::dvec4(pair.first, 1.0));
            glm::vec3 scaledPos(pair.first);
            scaledPos *= scale;
            ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
                *_font,
                scaledPos,
                pair.second,
                textColor,
                pow(_scaleFactor, _textSize.value()),
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

    void RenderableLabel::render(const RenderData& data, RendererTasks&) {

        float scale = 1.f;

        float fadeInVariable = 1.f;
        if (!_disableFadeInDistance) {
            float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
            const glm::vec2 fadeRange = _fadeInDistance;
            const float a = 1.f / ((fadeRange.y - fadeRange.x) * scale);
            const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
            const float funcValue = a * distCamera + b;
            fadeInVariable *= funcValue > 1.f ? 1.f : funcValue;

            if (funcValue < 0.01f) {
                return;
            }
        }

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
                orthoUp,
                fadeInVariable
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
