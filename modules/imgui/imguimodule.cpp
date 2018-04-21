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

#include <modules/imgui/imguimodule.h>

#include <modules/imgui/include/gui.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/virtualpropertymanager.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/assetloader.h>

#include <ghoul/logging/logmanager.h>

namespace openspace {

ImGUIModule::ImGUIModule() : OpenSpaceModule(Name) {
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
        [&](){
            LDEBUGC("ImGUIModule", "Initializing GUI");
            gui.initialize();

            gui._globalProperty.setSource(
                []() {
                    std::vector<properties::PropertyOwner*> res = {
                        &(OsEng.windowWrapper()),
                        &(OsEng.navigationHandler()),
                        &(OsEng.renderEngine()),
                        &(OsEng.parallelPeer()),
                        &(OsEng.console()),
                        &(OsEng.dashboard())
                    };
                    return res;
                }
            );

            gui._screenSpaceProperty.setSource(
                []() {
                    return OsEng.renderEngine().screenSpaceOwner().propertySubOwners();
                }
            );

            gui._moduleProperty.setSource(
                []() {
                    std::vector<properties::PropertyOwner*> v;
                    v.push_back(&(OsEng.moduleEngine()));
                    return v;
                }
            );

            gui._sceneProperty.setSource(
                []() {
                    const Scene* scene = OsEng.renderEngine().scene();
                    const std::vector<SceneGraphNode*>& nodes = scene ?
                        scene->allSceneGraphNodes() :
                        std::vector<SceneGraphNode*>();
                    return std::vector<properties::PropertyOwner*>(
                        nodes.begin(),
                        nodes.end()
                    );
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

            gui._featuredProperties.setSource(
                [](){
                    std::vector<SceneGraphNode*> nodes =
                        OsEng.renderEngine().scene()->allSceneGraphNodes();

                    nodes.erase(
                        std::remove_if(
                            nodes.begin(),
                            nodes.end(),
                            [](SceneGraphNode* n) {
                                const std::vector<std::string>& tags = n->tags();
                                auto it = std::find(
                                    tags.begin(),
                                    tags.end(),
                                    "GUI.Interesting"
                                );
                                return it == tags.end();
                            }
                        ),
                        nodes.end()
                    );
                    return std::vector<properties::PropertyOwner*>(
                        nodes.begin(),
                        nodes.end()
                    );
                }
            );
        }
    );

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Deinitialize,
        [&](){
            LDEBUGC("ImGui", "Deinitialize GUI");
            gui.deinitialize();
        }
    );

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::InitializeGL,
        [&](){
            LDEBUGC("ImGui", "Initializing GUI OpenGL");
            gui.initializeGL();
        }
    );

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::DeinitializeGL,
        [&](){
            LDEBUGC("ImGui", "Deinitialize GUI OpenGL");
            gui.deinitializeGL();
        }
    );

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Draw2D,
        [&]() {
            // TODO emiax: Make sure this is only called for one of the eyes, in the case
            // of side-by-side / top-bottom stereo.

            WindowWrapper& wrapper = OsEng.windowWrapper();
            bool showGui = wrapper.hasGuiWindow() ? wrapper.isGuiWindow() : true;
            if (wrapper.isMaster() && showGui) {
                const glm::ivec2 windowSize = wrapper.currentWindowSize();
                const glm::ivec2 resolution = wrapper.currentWindowResolution();

                glm::vec2 mousePosition = wrapper.mousePosition();
                uint32_t mouseButtons = wrapper.mouseButtons(2);

                double dt = std::max(wrapper.averageDeltaTime(), 0.0);
                if (touchInput.active && mouseButtons == 0) {
                    mouseButtons = touchInput.action;
                    mousePosition = touchInput.pos;
                }
                // We don't do any collection of immediate mode user interface, so it
                // is fine to open and close a frame immediately
                gui.startFrame(
                    static_cast<float>(dt),
                    glm::vec2(windowSize),
                    resolution / windowSize,
                    mousePosition,
                    mouseButtons
                );

                gui.endFrame();
            }
        }
    );

    OsEng.registerModuleKeyboardCallback(
        [&](Key key, KeyModifier mod, KeyAction action) -> bool {
            // A list of all the windows that can show up by themselves
            if (gui.isEnabled() || gui._performance.isEnabled() ||
                gui._sceneProperty.isEnabled())
            {
                return gui.keyCallback(key, mod, action);
            }
            else {
                return false;
            }
        }
    );

    OsEng.registerModuleCharCallback(
        [&](unsigned int codepoint, KeyModifier modifier) -> bool {
            // A list of all the windows that can show up by themselves
            if (gui.isEnabled() || gui._performance.isEnabled() ||
                gui._sceneProperty.isEnabled())
            {
                return gui.charCallback(codepoint, modifier);
            }
            else {
                return false;
            }
        }
    );

    OsEng.registerModuleMouseButtonCallback(
        [&](MouseButton button, MouseAction action) -> bool {
            // A list of all the windows that can show up by themselves
            if (gui.isEnabled() || gui._performance.isEnabled() ||
                gui._sceneProperty.isEnabled())
            {
                return gui.mouseButtonCallback(button, action);
            }
            else {
                return false;
            }
        }
    );

    OsEng.registerModuleMouseScrollWheelCallback(
        [&](double, double posY) -> bool {
            // A list of all the windows that can show up by themselves
            if (gui.isEnabled() || gui._performance.isEnabled() ||
                gui._sceneProperty.isEnabled())
            {
                return gui.mouseWheelCallback(posY);
            }
            else {
                return false;
            }
        }
    );
}

} // namespace openspace
