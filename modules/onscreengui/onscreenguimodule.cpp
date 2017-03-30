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
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>

#include <ghoul/logging/logmanager.h>

namespace openspace {

gui::GUI OnScreenGUIModule::gui;
    
OnScreenGUIModule::OnScreenGUIModule() 
    : OpenSpaceModule("OnScreenGUI")
{
    addPropertySubOwner(gui);
    
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
                        &(OsEng.renderEngine())
                    };
                    return res;
                }
            );
            
            gui._screenSpaceProperty.setSource(
                []() {
                   const auto& ssr = OsEng.renderEngine().screenSpaceRenderables();
                   return std::vector<properties::PropertyOwner*>(ssr.begin(), ssr.end());
                }
            );
            
            gui._property.setSource(
                []() {
                    const auto& nodes = OsEng.renderEngine().scene()->allSceneGraphNodes();
                    return std::vector<properties::PropertyOwner*>(nodes.begin(), nodes.end());
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
        OpenSpaceEngine::CallbackOption::Render,
        [](){
            WindowWrapper& wrapper = OsEng.windowWrapper();
            bool showGui = wrapper.hasGuiWindow() ? wrapper.isGuiWindow() : true;
            if (wrapper.isMaster() && wrapper.isRegularRendering() && showGui ) {
                glm::vec2 mousePosition = wrapper.mousePosition();
                //glm::ivec2 drawBufferResolution = _windowWrapper->currentDrawBufferResolution();
                glm::ivec2 windowSize = wrapper.currentWindowSize();
                glm::ivec2 renderingSize = wrapper.currentWindowResolution();
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
