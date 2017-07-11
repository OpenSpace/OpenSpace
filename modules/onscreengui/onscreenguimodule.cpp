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

#include <modules/onscreengui/onscreenguimodule.h>

#include <modules/onscreengui/include/gui.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/virtualpropertymanager.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/luaconsole.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/assetloader.h>

#include <ghoul/logging/logmanager.h>

namespace openspace {

gui::GUI OnScreenGUIModule::gui;
    
OnScreenGUIModule::OnScreenGUIModule() 
    : OpenSpaceModule("OnScreenGUI")
{
    addPropertySubOwner(gui);

    // TODO: Remove dependency on OsEng.
    // Instead, make this class implement an interface that OsEng depends on.
    // Do not try to register module callbacks if OsEng does not exist,
    // for example in the TaskRunner.

    if (!OpenSpaceEngine::isCreated()) {
        return;
    }

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Initialize,
        [](){
            LDEBUGC("OnScreenGUIModule", "Initializing GUI");
            gui.initialize();
            
            gui._globalProperty.setSource(
                []() {
                    std::vector<properties::PropertyOwner*> res = {
                        &(OsEng.windowWrapper()),
                        &(OsEng.settingsEngine()),
                        &(OsEng.interactionHandler()),
                        &(OsEng.renderEngine()),
                        &(OsEng.parallelConnection()),
                        &(OsEng.console()),
                        OsEng.assetLoader().rootAsset()
                    };
                    return res;
                }
            );
            
            gui._screenSpaceProperty.setSource(
                []() {
                   const std::vector<ScreenSpaceRenderable*>& ssr =
                       OsEng.renderEngine().screenSpaceRenderables();
                   return std::vector<properties::PropertyOwner*>(ssr.begin(), ssr.end());
                }
            );
            
            gui._property.setSource(
                []() {
                    const Scene* scene = OsEng.renderEngine().scene();
                    const std::vector<SceneGraphNode*>& nodes = scene ?
                        scene->allSceneGraphNodes() :
                        std::vector<SceneGraphNode*>();
                    return std::vector<properties::PropertyOwner*>(nodes.begin(), nodes.end());
                }
            );

            gui._virtualProperty.setSource(
                []() {
                    std::vector<properties::PropertyOwner*> res = {
                        &(OsEng.virtualPropertyManager())
                    };

                    return res;
                }
            );
        }
    );
    
    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Deinitialize,
        [](){
            LDEBUGC("OnScreenGui", "Deinitialize GUI");
            gui.deinitialize();
        }
    );
    
    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::InitializeGL,
        [](){
            LDEBUGC("OnScreenGui", "Initializing GUI OpenGL");
            gui.initializeGL();
        }
    );
    
    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::DeinitializeGL,
        [](){
            LDEBUGC("OnScreenGui", "Deinitialize GUI OpenGL");
            gui.deinitializeGL();
        }
    );

    OsEng.registerModuleCallback(
        // This is done in the PostDraw phase so that it will render it on top of
        // everything else in the case of fisheyes. With this being in the Render callback
        // the GUI would be rendered on top of each of the cube faces
        OpenSpaceEngine::CallbackOption::PostDraw,
        [](){
            WindowWrapper& wrapper = OsEng.windowWrapper();
            bool showGui = wrapper.hasGuiWindow() ? wrapper.isGuiWindow() : true;
            if (wrapper.isMaster() && showGui ) {
                glm::vec2 mousePosition = wrapper.mousePosition();
                //glm::ivec2 drawBufferResolution = _windowWrapper->currentDrawBufferResolution();
                glm::ivec2 windowSize = wrapper.currentWindowSize();
                uint32_t mouseButtons = wrapper.mouseButtons(2);
                
                double dt = std::max(wrapper.averageDeltaTime(), 0.0);
                
                // We don't do any collection of immediate mode user interface, so it is
                // fine to open and close a frame immediately
                gui.startFrame(
                    static_cast<float>(dt),
                    glm::vec2(windowSize),
                    wrapper.dpiScaling(),
                    mousePosition,
                    mouseButtons
                );

                gui.endFrame();
            }
        }
    );
    
    OsEng.registerModuleKeyboardCallback(
        [](Key key, KeyModifier mod, KeyAction action) -> bool {
            if (gui.isEnabled()) {
                return gui.keyCallback(key, mod, action);
            }
            else {
                return false;
            }
        }
    );
    
    OsEng.registerModuleCharCallback(
        [](unsigned int codepoint, KeyModifier modifier) -> bool {
            if (gui.isEnabled()) {
                return gui.charCallback(codepoint, modifier);
            }
            else {
                return false;
            }
        }
    );
    
    OsEng.registerModuleMouseButtonCallback(
        [](MouseButton button, MouseAction action) -> bool {
            if (gui.isEnabled()) {
                return gui.mouseButtonCallback(button, action);
            }
            else {
                return false;
            }
        }
    );
    
    OsEng.registerModuleMouseScrollWheelCallback(
        [](double pos) -> bool {
            if (gui.isEnabled()) {
                return gui.mouseWheelCallback(pos);
            }
            else {
                return false;
            }
        }
    );
}
    
} // namespace openspace
