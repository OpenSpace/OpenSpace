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

#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/path.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char _loggerCat[] = "Debugging";

    constexpr const char RenderedPathIdentifier[] = "CurrentCameraPath";
    constexpr const char RenderedPointsIdentifier[] = "CurrentPathControlPoints";
    constexpr const char DebuggingGuiPath[] = "/Debugging";

    constexpr const glm::vec3 PathColor = { 1.0, 1.0, 0.0 };
    constexpr const glm::vec3 OrientationLineColor = { 0.0, 1.0, 1.0 };

    // Conver the input string to a format that is valid as an identifier
    std::string makeIdentifier(std::string s) {
        std::replace(s.begin(), s.end(), ' ', '_');
        std::replace(s.begin(), s.end(), '.', '-');
        // Remove quotes and apostrophe, since they cause problems
        // when a string is translated to a script call
        s.erase(remove(s.begin(), s.end(), '\"'), s.end());
        s.erase(remove(s.begin(), s.end(), '\''), s.end());
        return s;
    }
} // namespace

namespace openspace::luascriptfunctions {

/**
 * PathNavigation
 * Renders the current camera path
 */
int renderCameraPath(lua_State* L) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 3 },
        "lua::renderCameraPath"
    );

    if (!global::navigationHandler->pathNavigator().hasCurrentPath()) {
        LWARNINGC("Debugging: PathNavigation", "There is no current path to render");
    }

    const interaction::Path* currentPath =
        global::navigationHandler->pathNavigator().currentPath();

    const int nSteps = (nArguments > 0) ? ghoul::lua::value<int>(L, 1) : 100;
    bool renderDirections = (nArguments > 1) ? ghoul::lua::value<bool>(L, 2) : false;

    constexpr const double defaultLineLength = 6e7;
    const double directionLineLength =
        (nArguments > 2) ? ghoul::lua::value<double>(L, 3) : defaultLineLength;

    // Parent node. Note that we only render one path at a time, so remove the previously
    // rendered one, if any
    std::string addParentScript = fmt::format(
        "if openspace.hasSceneGraphNode('{0}') then "
            "openspace.removeSceneGraphNode('{0}') "
        "end "
        "openspace.addSceneGraphNode( {{ Identifier = '{0}' }} )",
        RenderedPathIdentifier
    );

    openspace::global::scriptEngine->queueScript(
        addParentScript,
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    // Get the poses along the path
    std::vector<CameraPose> poses;
    const double du = 1.0 / nSteps;
    const double length = currentPath->pathLength();
    for (double u = 0.0; u < 1.0; u += du) {
        const CameraPose p = currentPath->interpolatedPose(u * length);
        poses.push_back(p);
    }
    poses.push_back(currentPath->endPoint().pose);

    // Create node lines between the positions
    auto pointIdentifier = [](int i) {
        return fmt::format("Point_{}", i);
    };

    auto addPoint = [pointIdentifier] (const std::string& id, glm::dvec3 p) {
        const std::string pointNode = "{"
            "Identifier = '" + id + "',"
            "Parent = '" + RenderedPathIdentifier + "',"
            "Transform = { "
                "Translation = {"
                    "Type = 'StaticTranslation',"
                    "Position = " + ghoul::to_string(p) + ""
                "},"
            "}"
        "}";

        openspace::global::scriptEngine->queueScript(
            fmt::format("openspace.addSceneGraphNode({})", pointNode),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };

    auto addLineBetweenPoints = [pointIdentifier] (const std::string& id1,
                                                   const std::string& id2,
                                                   const glm::vec3& color,
                                                   float lineWidth)
    {
        const std::string lineNode = "{"
            "Identifier = '" + fmt::format("Line{}", id1) + "',"
            "Parent = '" + RenderedPathIdentifier + "',"
            "Renderable = {"
                "Enabled = true,"
                "Type = 'RenderableNodeLine',"
                "StartNode = '" + id1 + "',"
                "EndNode = '" + id2 + "',"
                "LineWidth = " + std::to_string(lineWidth) + ","
                "Color = " + ghoul::to_string(color) + ""
            "}"
        "}";

        openspace::global::scriptEngine->queueScript(
            fmt::format("openspace.addSceneGraphNode({})", lineNode),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };

    auto addDirectionLine = [addPoint, addLineBetweenPoints, directionLineLength]
                            (const std::string& pointId, const CameraPose& p)
    {
        const glm::dvec3 dir = glm::normalize(p.rotation * glm::dvec3(0.0, 0.0, -1.0));
        const glm::dvec3 pointPosition = p.position + directionLineLength * dir;
        const std::string id = fmt::format("{}_orientation", pointId);

        addPoint(id, pointPosition);
        addLineBetweenPoints(id, pointId, OrientationLineColor, 2.f);
    };

    // Add first point separately so that we can create first line in for loop
    addPoint(pointIdentifier(0), poses.front().position);
    if (renderDirections) {
        addDirectionLine(pointIdentifier(0), poses.front());
    }

    for (int i = 1; i < poses.size(); i++) {
        addPoint(pointIdentifier(i), poses[i].position);
        addLineBetweenPoints(pointIdentifier(i), pointIdentifier(i - 1), PathColor, 4.f);

        if (renderDirections) {
            addDirectionLine(pointIdentifier(i), poses[i]);
        }
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * PathNavigation
 * Removes the currently rendered camera path if there is one
 */
int removeRenderedCameraPath(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::removeRenderedCameraPath");

    openspace::global::scriptEngine->queueScript(
        fmt::format("openspace.removeSceneGraphNode('{}') ", RenderedPathIdentifier),
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * PathNavigation
 * Renders the control points of the current camera path
 */
int renderPathControlPoints(lua_State* L) {
    int nArguments = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 1 },
        "lua::renderPathControlPoints"
    );

    if (!global::navigationHandler->pathNavigator().hasCurrentPath()) {
        LWARNINGC(
            "Debugging: PathNavigation", "There is no current path to sample points from"
        );
    }

    const interaction::Path* currentPath =
        global::navigationHandler->pathNavigator().currentPath();

    const double radius = (nArguments > 0) ? ghoul::lua::value<int>(L, 1) : 2000000;

    // Parent node. Note that we only render one set of points at a time,
    // so remove any previously rendered ones
    std::string addParentScript = fmt::format(
        "if openspace.hasSceneGraphNode('{0}') then "
            "openspace.removeSceneGraphNode('{0}') "
        "end "
        "openspace.addSceneGraphNode( {{ Identifier = '{0}' }} )",
        RenderedPointsIdentifier
    );

    openspace::global::scriptEngine->queueScript(
        addParentScript,
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    const std::vector<glm::dvec3> points = currentPath->controlPoints();

    const std::string guiPath =
        fmt::format("{}/Camera Path Control Points", DebuggingGuiPath);

    const char* colorTexturePath = "openspace.absPath("
        "openspace.createSingleColorImage('point_color', { 0.0, 1.0, 0.0 })"
    ")";

    for (int i = 0; i < points.size(); i++) {
        const std::string& node = "{"
            "Identifier = 'ControlPoint_" + std::to_string(i) + "',"
            "Parent = '" + RenderedPointsIdentifier + "',"
            "Transform = { "
                "Translation = {"
                    "Type = 'StaticTranslation',"
                    "Position = " + ghoul::to_string(points[i]) + ""
                "},"
            "},"
            "Renderable = {"
                "Type = 'RenderableSphere',"
                "Enabled = true,"
                "Segments = 30,"
                "Size = " + std::to_string(radius) + ","
                "Texture = " + colorTexturePath + ""
            "},"
            "GUI = {"
                "Name = 'Control Point " + std::to_string(i) + "',"
                "Path = '" + guiPath + "'"
            "}"
        "}";

        openspace::global::scriptEngine->queueScript(
            fmt::format("openspace.addSceneGraphNode({})", node),
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * PathNavigation
 * Removes the rendered control points
 */
int removePathControlPoints(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::removePathControlPoints");

    openspace::global::scriptEngine->queueScript(
        fmt::format("openspace.removeSceneGraphNode('{}') ", RenderedPointsIdentifier),
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
 * Add a set of cartesian axes to the specified scene graph node
 */
int addCartesianAxes(lua_State* L) {
    int nArgs = ghoul::lua::checkArgumentsAndThrow(L, { 2, 3 }, "lua::addCartesianAxes");

    const std::string& nodeName = ghoul::lua::value<std::string>(L, 1);
    const std::string& axesIdentifier = ghoul::lua::value<std::string>(L, 2);

    SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(nodeName);
    if (!n) {
        return ghoul::lua::luaError(L, "Unknown scene graph node: " + nodeName);
    }

    double scale;
    if (nArgs > 2) {
        scale = ghoul::lua::value<double>(L, 3);
    }
    else {
        scale = 2.0 * n->boundingSphere();
        if (n->boundingSphere() < 1E-3) {
            LWARNING("Using zero bounding sphere for scale of created axes. You might "
                "have to set the scale manually for them to be visible");
            scale += 1.0;
        }
    }

    const std::string identifier = makeIdentifier(axesIdentifier);
    const std::string& axes = "{"
        "Identifier = '" + identifier + "',"
        "Parent = '" + nodeName + "',"
        "Transform = { "
            "Scale = {"
                "Type = 'StaticScale',"
                "Scale = " + std::to_string(scale) + ""
            "}"
        "},"
        "Renderable = {"
            "Type = 'RenderableCartesianAxes',"
            "Enabled = true,"
            "XColor = { 1.0, 0.0, 0.0 },"
            "YColor = { 0.0, 1.0, 0.0 },"
            "ZColor = { 0.0, 0.0, 1.0 }"
        "},"
        "GUI = {"
            "Name = '" + identifier + "',"
            "Path = '" + DebuggingGuiPath + "/Coordiante Systems'"
        "}"
    "}";

    openspace::global::scriptEngine->queueScript(
        fmt::format("openspace.addSceneGraphNode({})", axes),
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions

