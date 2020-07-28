/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/virtualpropertymanager.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace openspace {

ImGUIModule::ImGUIModule() : OpenSpaceModule(Name) {
    addPropertySubOwner(gui);

    global::callback::initialize.emplace_back([&]() {
        LDEBUGC("ImGUIModule", "Initializing GUI");
        gui.initialize();

        gui._globalProperty.setSource(
            []() {
            std::vector<properties::PropertyOwner*> res = {
                &global::navigationHandler,
                &global::sessionRecording,
                &global::timeManager,
                &global::renderEngine,
                &global::parallelPeer,
                &global::luaConsole,
                &global::dashboard
            };
            return res;
        }
        );

        gui._screenSpaceProperty.setSource(
            []() {
                return global::screenSpaceRootPropertyOwner.propertySubOwners();
            }
        );

        gui._moduleProperty.setSource(
            []() {
                std::vector<properties::PropertyOwner*> v;
                v.push_back(&(global::moduleEngine));
                return v;
            }
        );

        gui._sceneProperty.setSource(
            []() {
                const Scene* scene = global::renderEngine.scene();
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
                    &global::virtualPropertyManager
                };

                return res;
            }
        );

        gui._featuredProperties.setSource(
            []() {
                std::vector<SceneGraphNode*> nodes =
                    global::renderEngine.scene()->allSceneGraphNodes();

                nodes.erase(
                    std::remove_if(
                        nodes.begin(),
                        nodes.end(),
                        [](SceneGraphNode* n) {
                            const std::vector<std::string>& tags = n->tags();
                            const auto it = std::find(
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
    });

    global::callback::deinitialize.emplace_back([&]() {
        ZoneScopedN("ImGUI")

        LDEBUGC("ImGui", "Deinitialize GUI");
        gui.deinitialize();
    });

    global::callback::initializeGL.emplace_back([&]() {
        ZoneScopedN("ImGUI")

        LDEBUGC("ImGui", "Initializing GUI OpenGL");
        gui.initializeGL();
    });

    global::callback::deinitializeGL.emplace_back([&]() {
        ZoneScopedN("ImGUI")

        LDEBUGC("ImGui", "Deinitialize GUI OpenGL");
        gui.deinitializeGL();
    });

    global::callback::draw2D.emplace_back([&]() {
        ZoneScopedN("ImGUI")

        WindowDelegate& delegate = global::windowDelegate;
        const bool showGui = delegate.hasGuiWindow() ? delegate.isGuiWindow() : true;
        if (delegate.isMaster() && showGui) {
            const glm::ivec2 windowSize = delegate.currentSubwindowSize();
            const glm::ivec2 resolution = delegate.currentDrawBufferResolution();

            if (windowSize.x <= 0 || windowSize.y <= 0) {
                return;
            }

            const double dt = std::max(delegate.averageDeltaTime(), 0.0);
            // We don't do any collection of immediate mode user interface, so it
            // is fine to open and close a frame immediately
            gui.startFrame(
                static_cast<float>(dt),
                glm::vec2(windowSize),
                resolution / windowSize,
                _mousePosition,
                _mouseButtons
            );

            gui.endFrame();
        }
    });

    global::callback::keyboard.emplace_back(
        [&](Key key, KeyModifier mod, KeyAction action) -> bool {
            ZoneScopedN("ImGUI")

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

    global::callback::character.emplace_back(
        [&](unsigned int codepoint, KeyModifier modifier) -> bool {
            ZoneScopedN("ImGUI")

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

    global::callback::mousePosition.emplace_back(
        [&](double x, double y) {
            _mousePosition = glm::vec2(static_cast<float>(x), static_cast<float>(y));
        }
    );

    global::callback::mouseButton.emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier) -> bool {
            ZoneScopedN("ImGUI")

            if (action == MouseAction::Press) {
                _mouseButtons |= (1 << static_cast<int>(button));
            }
            else if (action == MouseAction::Release) {
                _mouseButtons &= ~(1 << static_cast<int>(button));
            }

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

    global::callback::mouseScrollWheel.emplace_back(
        [&](double, double posY) -> bool {
            ZoneScopedN("ImGUI")

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

    global::callback::touchDetected.emplace_back(
        [&](TouchInput input) -> bool {
            return gui.touchDetectedCallback(input);
        }
    );

    global::callback::touchUpdated.emplace_back(
        [&](TouchInput input) -> bool {
            return gui.touchUpdatedCallback(input);
        }
    );

    global::callback::touchExit.emplace_back(
        [&](TouchInput input) {
            gui.touchExitCallback(input);
        }
    );
}

} // namespace openspace
