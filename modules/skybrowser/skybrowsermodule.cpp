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

#include <modules/skybrowser/skybrowsermodule.h>

 //#include <modules/webbrowser/webbrowsermodule.h>
 //#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/rendering/renderable.h>

#include <openspace/engine/moduleengine.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <thread>
#include <chrono>
#include "skybrowsermodule_lua.inl"

#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <cmath> // For atan2
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <glm/gtx/string_cast.hpp>
#include <glm/gtx/rotate_vector.hpp >

namespace {
    constexpr const openspace::properties::Property::PropertyInfo TestInfo = 
    {
        "Test",
        "Test Info",
        "tjobidabidobidabidopp plopp"
    };
    constexpr const openspace::properties::Property::PropertyInfo ZoomInfo =
    {
        "Zoom",
        "Zoom Info",
        "tjobidabidobidabidopp plupp"
    };

    struct [[codegen::Dictionary(SkyBrowserModule)]] Parameters {

        // [[codegen::verbatim(TestInfo.description)]]
        std::optional<std::string> test;

        // [[codegen::verbatim(ZoomInfo.description)]]
        std::optional<float> zoom;
    };
    
    #include "skybrowsermodule_codegen.cpp"
    
    
} // namespace

namespace openspace {

SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
    , _testProperty(TestInfo)
    , _fieldOfView(ZoomInfo, 50.f ,0.1f ,70.f)
    , _skyBrowser(nullptr)
    , _skyTarget(nullptr)
    , _camIsSyncedWWT(true)
    , currentlyDraggingBrowser(false)
    , currentlyDraggingTarget(false)
    , currentlyResizingBrowser(false)
    , _listenForInteractions(true)
    , mouseIsOnBrowser(false)
    , mouseIsOnTarget(false)

{
    addProperty(_testProperty);

    _fieldOfView.onChange([&]() {
        if (_skyTarget) {
            _skyTarget->updateFOV(_fieldOfView);
        }
    });
    addProperty(_fieldOfView);

    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {      
            glm::vec2 pos = glm::vec2(static_cast<float>(x), static_cast<float>(y));

            _mousePosition = getMousePositionInScreenSpaceCoords(pos);

            if (_skyTarget) {
                mouseIsOnTarget = _skyTarget->coordIsInsideCornersScreenSpace(_mousePosition);          
            }
            else {
                mouseIsOnTarget = false;
            }
            if (_skyBrowser) {
                mouseIsOnBrowser = _skyBrowser->coordIsInsideCornersScreenSpace(_mousePosition);       
            }
            else {
                mouseIsOnBrowser = false;
            }
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double scroll) -> bool {
            if (mouseIsOnBrowser) {
                float zoom = scroll > 0.0 ? -log(_fieldOfView + 1.1f) : log(_fieldOfView + 1.1f);
                _fieldOfView = std::clamp(_fieldOfView + zoom, 0.001f, 70.0f);
                return true;
            }
          
            return false;
        }
    );

    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (action == MouseAction::Press) {
                
                if (mouseIsOnBrowser && button == MouseButton::Left) {
                    
                    startDragMousePosBrowser = _mousePosition;
                    startDragObjectPosBrowser = _skyBrowser->getScreenSpacePosition();
                    // Resize browser if mouse is over resize button
                    resizeVector = _skyBrowser->coordIsOnResizeArea(_mousePosition);

                    if (resizeVector != glm::vec2{0}) {
                        _skyBrowser->saveResizeStartSize();
                        startResizeBrowserSize = _skyBrowser->getScreenSpaceDimensions();
                        currentlyResizingBrowser = true;
                    }
                    else {
                        currentlyDraggingBrowser = true;
                    }
                    
                    return true;
                }
                else if (mouseIsOnTarget && button == MouseButton::Left) {
                    
                    startDragMousePosTarget = _mousePosition;
                    startDragObjectPosTarget = _skyTarget->getScreenSpacePosition();
                    currentlyDraggingTarget = true;
                    return true;
                }
                else if (mouseIsOnBrowser && button == MouseButton::Right) {

                    startDragMousePosTarget = _mousePosition;
                    startDragObjectPosTarget = _skyTarget->getScreenSpacePosition();
                    currentlyDraggingTarget = true;
                    return true;
                }
            }
            else if (action == MouseAction::Release) {
                if (currentlyDraggingBrowser) {
                    currentlyDraggingBrowser = false;
                    return true;
                }
                if (currentlyDraggingTarget) {
                    currentlyDraggingTarget = false;
                    return true;
                }
                if (currentlyResizingBrowser) {
                    currentlyResizingBrowser = false;
                    _skyBrowser->_browserDimIsDirty = false;
                    _skyBrowser->updateBrowserSize();
                    return true;
                }
            }

            return false;
        }
    );
} 

void SkyBrowserModule::internalDeinitialize() {
        // Set flag to false so the thread can exit
    _camIsSyncedWWT = false;
    if (_threadWWTMessages.joinable()) {
        _threadWWTMessages.join();
        LINFO("Joined thread");
    }
    if (_threadHandleInteractions.joinable()) {
        _threadHandleInteractions.join();
        LINFO("Joined thread");
    }
}

scripting::LuaLibrary SkyBrowserModule::luaLibrary() const {

    scripting::LuaLibrary res;
    res.name = "skybrowser";
    res.functions = {
        {
            "create",
            &skybrowser::luascriptfunctions::createBrowser,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "move",
            &skybrowser::luascriptfunctions::moveBrowser,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "follow",
            &skybrowser::luascriptfunctions::followCamera,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        },
        {
            "loacImgCollection",
            &skybrowser::luascriptfunctions::loadImgCollection,
            {},
            "string or list of strings",
            "Add one or multiple exoplanet systems to the scene, as specified by the "
            "input. An input string should be the name of the system host star"
        }
    };

    return res;
}

float SkyBrowserModule::fieldOfView() const{
    return _fieldOfView;
}

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    
    const Parameters p = codegen::bake<Parameters>(dict);
    _testProperty = p.test.value_or(_testProperty);
    _fieldOfView = p.zoom.value_or(_fieldOfView);

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

    // register ScreenSpaceTarget
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyTarget>("ScreenSpaceSkyTarget");

}

bool SkyBrowserModule::sendMessageToWWT(const ghoul::Dictionary& msg) {
    if (_skyBrowser) {
        std::string script = "sendMessageToWWT(" + ghoul::formatJson(msg) + ");";
        _skyBrowser->executeJavascript(script);
        return true;
    }
    else {
        LERROR("No sky browser added! Can't send message.");
        return false;
    }
}

void SkyBrowserModule::handleInteractions() {
    /*       
    CefStructBase<CefMouseEventTraits> event;
    _skyBrowser->sendMouseEvent(event, scroll, scroll);
   */
    _threadHandleInteractions = std::thread([&] {
        while (_listenForInteractions) {

            if (currentlyDraggingBrowser) {
                  _skyBrowser->translate(_mousePosition - startDragMousePosBrowser, startDragObjectPosBrowser);
            }         
            if (currentlyDraggingTarget) {
                 _skyTarget->translate(_mousePosition - startDragMousePosTarget, startDragObjectPosTarget);
            }
            if (currentlyResizingBrowser) {
                // Calculate scaling factor
                glm::vec2 mouseDragVector = (_mousePosition - startDragMousePosBrowser);
                glm::vec2 scalingVector = mouseDragVector * resizeVector;

                glm::vec2 newSizeRelToOld = (startResizeBrowserSize + (scalingVector)) / startResizeBrowserSize;
                _skyBrowser->scale(newSizeRelToOld);

                // Make sure the browser doesn't move in directions it's not supposed to 
                _skyBrowser->translate(mouseDragVector * abs(resizeVector) /2.f, startDragObjectPosBrowser);

                _skyTarget->setScreenSpaceTargetDimension(_skyBrowser->getScreenSpaceBrowserDimension());

            }
        }
    });
}

glm::vec2 SkyBrowserModule::getMousePositionInScreenSpaceCoords(glm::vec2& mousePos) {
    glm::vec2 size = global::windowDelegate->currentWindowSize();
    // Change origin to middle of the window
    glm::vec2 screenSpacePos = glm::vec2((mousePos - (size / 2.0f)));
    // Ensure the upper right corner is positive on the y axis
    screenSpacePos *= glm::vec2(1.0f, -1.0f);
    // Transform pixel coordinates to screen space coordinates [-1,1][-ratio, ratio]
    screenSpacePos /= (0.5f*size.y);
    return screenSpacePos;
}

void SkyBrowserModule::WWTfollowCamera() {
    
    // Start a thread to enable user interaction while sending the calls to WWT
    _threadWWTMessages = std::thread([&] {
        while (_camIsSyncedWWT) {

            // Get camera view direction and orthogonal coordinate system of camera view direction
            glm::vec3 viewDirection = global::navigationHandler->camera()->viewDirectionWorldSpace();
            glm::vec3 upDirection = global::navigationHandler->camera()->lookUpVectorWorldSpace();
            glm::vec3 sideDirection = glm::cross(upDirection, viewDirection);

            glm::vec2 angleOffset = _skyTarget ? _skyTarget->getAnglePosition() : glm::vec2(0);
            // Change view if target is moved
            glm::vec3 targetDirection = glm::rotate(viewDirection, angleOffset.x, upDirection);
            targetDirection = glm::rotate(targetDirection, angleOffset.y, sideDirection);
           

            // Convert to celestial coordinates
            glm::dvec2 celestCoords = convertGalacticToCelestial(targetDirection);
            ghoul::Dictionary message = createMessageForMovingWWTCamera(celestCoords, _fieldOfView);

            // Sleep so we don't bombard WWT with too many messages
            std::this_thread::sleep_for(std::chrono::milliseconds(50));
            sendMessageToWWT(message);
            
        }
    });
    
}

ghoul::Dictionary SkyBrowserModule::createMessageForMovingWWTCamera(const glm::dvec2 celestCoords, const float fov,  const bool moveInstantly) const {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "center_on_coordinates"s);
    msg.setValue("ra", static_cast<double>(celestCoords[0]));
    msg.setValue("dec", static_cast<double>(celestCoords[1]));
    msg.setValue("fov", static_cast<double>(fov));
    msg.setValue("instant", moveInstantly);
    
    return msg;
}

ghoul::Dictionary SkyBrowserModule::createMessageForLoadingWWTImgColl(const std::string& url) const {
    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "center_on_coordinates"s);
    msg.setValue("url", url);

    return msg;
}

ghoul::Dictionary SkyBrowserModule::createMessageForPausingWWTTime() const {

    using namespace std::string_literals;
    ghoul::Dictionary msg;
    msg.setValue("event", "pause_time"s);

    return msg;
}



void SkyBrowserModule::initializeBrowser(ScreenSpaceSkyBrowser* skyBrowser, ScreenSpaceSkyTarget* skyTarget) {

    _skyBrowser = skyBrowser;
    _skyTarget = skyTarget;
}

ScreenSpaceSkyBrowser* SkyBrowserModule::skyBrowser() {
    return _skyBrowser;
}

glm::dvec2 SkyBrowserModule::convertGalacticToCelestial(glm::dvec3 rGal) const {
    
    // Used the math from this website: https://gea.esac.esa.int/archive/documentation/GD -->
    // R2/Data_processing/chap_cu3ast/sec_cu3ast_intro/ssec_cu3ast_intro_tansforms.html#SSS1
    const glm::dmat3 conversionMatrix = glm::dmat3({
      -0.0548755604162154,  0.4941094278755837, -0.8676661490190047, // col 0
      -0.8734370902348850, -0.4448296299600112, -0.1980763734312015, // col 1
      -0.4838350155487132,  0.7469822444972189,  0.4559837761750669  // col 2
        });
   
    glm::dvec3 rICRS = glm::transpose(conversionMatrix) * rGal;
    float ra = atan2(rICRS[1], rICRS[0]);
    float dec = atan2(rICRS[2], glm::sqrt((rICRS[0] * rICRS[0]) + (rICRS[1] * rICRS[1])));

    ra = ra > 0 ? ra : ra + (2 * glm::pi<float>());

    return glm::dvec2(glm::degrees(ra), glm::degrees(dec));
}

void SkyBrowserModule::createTarget(glm::ivec2 dimension) {

    std::string browserDim = fmt::format("{{{},{}}}", dimension.x, dimension.y);

    LINFO(browserDim);
    using namespace std::string_literals;


    std::string node = "{"
        "Type = 'ScreenSpaceSkyTarget',"
        "Identifier = 'ScreenSpaceTarget',"
        "Name = 'Screen Space Target',"
        "FaceCamera = false,"
        "TargetDimensions = " + browserDim + ""
        "}";

    openspace::global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + node + ")",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}


void SkyBrowserModule::createBrowser() {

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    using namespace std::string_literals;

    std::string node = "{"
        "Type = 'ScreenSpaceSkyBrowser',"
        "Identifier = 'ScreenSpaceBowser',"
        "Name = 'Screen Space Bowser',"
        "Url = 'http://localhost:8000/',"
        "FaceCamera = false"
        "}";

    /*
    ghoul::Dictionary node;
    node.setValue("Type", "ScreenSpaceBrowser"s);
    node.setValue("Identifier", "ScreenSpaceBowser"s);
    node.setValue("Name", "Screen Space Bowser"s);
    node.setValue("Url", "http://localhost:8000/"s);
    */
    openspace::global::scriptEngine->queueScript(
        "openspace.addScreenSpaceRenderable(" + node + ")",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

}


/*
std::vector<documentation::Documentation> SkyBrowserModule::documentations() const {
    return {
        ExoplanetsDataPreparationTask::documentation(),
        RenderableOrbitDisc::Documentation()
    };
}
*/
} // namespace openspace
