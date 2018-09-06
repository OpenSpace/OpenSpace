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

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/virtualpropertymanager.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>

namespace openspace {

ImGUIModule::ImGUIModule() : OpenSpaceModule(Name) {
    addPropertySubOwner(gui);

    global::callback::initialize.push_back([&]() {
        LDEBUGC("ImGUIModule", "Initializing GUI");
        gui.initialize();

        gui._globalProperty.setSource(
            []() {
            std::vector<properties::PropertyOwner*> res = {
                &global::navigationHandler,
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

    global::callback::deinitialize.push_back([&]() {
        LDEBUGC("ImGui", "Deinitialize GUI");
        gui.deinitialize();
    });

    global::callback::initializeGL.push_back([&]() {
        LDEBUGC("ImGui", "Initializing GUI OpenGL");
        gui.initializeGL();
    });

    global::callback::deinitializeGL.push_back([&]() {
        LDEBUGC("ImGui", "Deinitialize GUI OpenGL");
        gui.deinitializeGL();
    });

    global::callback::draw2D.push_back([&]() {
        // TODO emiax: Make sure this is only called for one of the eyes, in the case
        // of side-by-side / top-bottom stereo.

        WindowDelegate& delegate = global::windowDelegate;
        const bool showGui = delegate.hasGuiWindow() ? delegate.isGuiWindow() : true;
        if (delegate.isMaster() && showGui) {
            const glm::ivec2 windowSize = delegate.currentWindowSize();
            const glm::ivec2 resolution = delegate.currentWindowResolution();

            glm::vec2 mousePosition = delegate.mousePosition();
            uint32_t mouseButtons = delegate.mouseButtons(2);

            const double dt = std::max(delegate.averageDeltaTime(), 0.0);
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
    });

    global::callback::keyboard.push_back(
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

    global::callback::character.push_back(
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

    global::callback::mouseButton.push_back(
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

    global::callback::mouseScrollWheel.push_back(
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
