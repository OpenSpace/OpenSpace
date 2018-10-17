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

#include <modules/dsn/rendering/renderablecommunicationpackage.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>


#include <openspace/scene/scene.h>
//#include <openspace/scene/scenegraphnode.h>

namespace {
    constexpr const char* ProgramName = "CommunicationPackageProgram";
    constexpr const char* KeyTranslation = "Translation";
    constexpr const char* _loggerCat = "RenderableCommmunicationPackage";

    constexpr const std::array<const char*, 4> UniformNames = {
        "modelViewTransformStation","modelViewTransformSpacecraft", "projectionTransform", "color"
    };

    constexpr openspace::properties::Property::PropertyInfo MadridColorInfo = {
        "MadridColor",
        "MadridColor",
        "This value determines the RGB main color for the lines "
        "of communication to and from Madrid."
    };

    constexpr openspace::properties::Property::PropertyInfo GoldstoneColorInfo = {
        "GoldstoneColor",
        "GoldstoneColor",
        "This value determines the RGB main color for the lines "
        "of communication to and from Goldstone."
    };
    constexpr openspace::properties::Property::PropertyInfo CanberraColorInfo = {
        "CanberraColor",
        "CanberraColor",
        "This value determines the RGB main color for the lines "
        "of communication to and from Canberra."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the communication package. "
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableCommunicationPackage::Documentation() {
    using namespace documentation;
    return {
        "Renderable Communication Package",
        "dsn_renderable_renderablecommunicationpackage",
        {
            {
                KeyTranslation,
                new ReferencingVerifier("core_transform_translation"),
                Optional::No,
                "This object is used to compute locations along the path. Any "
                "Translation object can be used here."
            },
            {
                MadridColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                MadridColorInfo.description
            },
            {
                GoldstoneColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                GoldstoneColorInfo.description
            },
            {
                CanberraColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                GoldstoneColorInfo.description
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            }
        }
    };
}

RenderableCommunicationPackage::RenderableCommunicationPackage(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _madridLineColor(MadridColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _goldstoneLineColor(GoldstoneColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _canberraLineColor(CanberraColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 20.f)
{
    //addProperty(_opacity);
    //registerUpdateRenderBinFromOpacity();

    _translation = Translation::createFromDictionary(
        dictionary.value<ghoul::Dictionary>(KeyTranslation)
    );
    addPropertySubOwner(_translation.get());

    if (dictionary.hasKeyAndValue<glm::vec3>(MadridColorInfo.identifier)) {
        _madridLineColor = dictionary.value<glm::vec3>(MadridColorInfo.identifier);
    }
    else {
        _madridLineColor = glm::vec3(1.f, 0.f, 0.f);
    }
    addProperty(_madridLineColor);

    if (dictionary.hasKeyAndValue<glm::vec3>(CanberraColorInfo.identifier)) {
        _canberraLineColor = dictionary.value<glm::vec3>(CanberraColorInfo.identifier);
    }
    else {
        _canberraLineColor = glm::vec3(0.f, 1.f, 0.f);
    }
    addProperty(_canberraLineColor);

    if (dictionary.hasKeyAndValue<glm::vec3>(GoldstoneColorInfo.identifier)) {
        _goldstoneLineColor = dictionary.value<glm::vec3>(GoldstoneColorInfo.identifier);
    }
    else {
        _goldstoneLineColor = glm::vec3(0.f, 0.f, 1.f);
    }
    addProperty(_goldstoneLineColor);

    if (dictionary.hasKeyAndValue<double>(LineWidthInfo.identifier)) {
        _lineWidth = static_cast<float>(dictionary.value<double>(
            LineWidthInfo.identifier
        ));
    }
    addProperty(_lineWidth);
}

void RenderableCommunicationPackage::initializeGL() {
    _programObject = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_DSN}/shaders/renderablecommunicationpackage_vs.glsl"),
                absPath("${MODULE_DSN}/shaders/renderablecommunicationpackage_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_programObject, _uniformCache, UniformNames);

    setRenderBin(Renderable::RenderBin::Overlay);
}

void RenderableCommunicationPackage::deinitializeGL() {
    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

bool RenderableCommunicationPackage::isReady() const {
    return _programObject != nullptr;
}

void RenderableCommunicationPackage::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();

    SceneGraphNode* earthNode = global::renderEngine.scene()->sceneGraphNode("Earth");
    SceneGraphNode* spacecraftNode = global::renderEngine.scene()->sceneGraphNode("Voyager_1");

    glm::dmat4 modelTransformSpacecraft =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelTransformStation = earthNode->modelTransform();

    _programObject->setUniform(_uniformCache.projection, data.camera.projectionMatrix());

    _programObject->setUniform(_uniformCache.color, _lineColor);

    const bool usingFramebufferRenderer =
        global::renderEngine.rendererImplementation() ==
        RenderEngine::RendererImplementation::Framebuffer;

    if (usingFramebufferRenderer) {
        glDepthMask(false);
        //glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }
    glLineWidth(_lineWidth);

    // We pass in the model view transformation matrix as double in order to maintain
    // high precision for vertices; especially for the lines, a high vertex precision
    // is necessary as they are usually far away from their reference
    _programObject->setUniform( _uniformCache.modelViewStation,
        data.camera.combinedViewMatrix() * modelTransformStation * _mainRenderInformation._localTransform);
    
    _programObject->setUniform(_uniformCache.modelViewSpacecraft,
        data.camera.combinedViewMatrix() * modelTransformSpacecraft * _mainRenderInformation._localTransform);

    glBindVertexArray(_mainRenderInformation._vaoID);

    glDrawArrays(
        GL_LINE_STRIP,
        _mainRenderInformation.first,
        _mainRenderInformation.count
    );

    //unbind vertex array
    glBindVertexArray(0);

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _programObject->deactivate();
}

glm::vec3 RenderableCommunicationPackage::GetSiteColor(std::string dishidentifier) {
    
    glm::vec3 color(0.0f,0.0f,0.0f);
    SiteEnum site;

    try {
        site = StationToSiteConversion.at(dishidentifier);
    }
    catch (const std::exception& e) {
        LERROR(fmt::format("Station {} has no site location.", dishidentifier));
    }

    glm::vec3 color1 = _goldstoneLineColor;
    glm::vec3 color2 = _madridLineColor;
    glm::vec3 color3 = _canberraLineColor;


    switch (site) {
        case 0: 
            color = _goldstoneLineColor; 
            break;
        case 1: 
            color = _madridLineColor;
            break;
        case 2: 
            color = _canberraLineColor;
            break;
    }
    return color;
}


} // namespace openspace
