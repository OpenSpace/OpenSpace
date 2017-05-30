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

#include "gtest/gtest.h"

#include <openspace/scene/sceneloader.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <openspace/scene/scene.h>

#include <ghoul/lua/lua_helper.h>
#include <fstream>


//class SceneLoaderTest : public testing::Test {};

TEST(SceneLoaderTest, NonExistingFileTest) {
    const std::string file = absPath("NonExistingFile");

    openspace::SceneLoader loader;
    EXPECT_THROW(loader.loadScene(file), ghoul::FileNotFoundError);
}

TEST(SceneLoaderTest, IllformedFileTest) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/illformed.scene");

    openspace::SceneLoader loader;
    EXPECT_THROW(loader.loadScene(file), ghoul::lua::LuaRuntimeException);
}


TEST(SceneLoaderTest, IllformedFileTestInvalidSceneFolder) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/illformedInvalidScene.scene");

    openspace::SceneLoader loader;
    EXPECT_THROW(loader.loadScene(file), openspace::documentation::SpecificationError);
}

TEST(SceneLoaderTest, IllformedFileTestWrongType) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/illformedWrongType.scene");

    openspace::SceneLoader loader;
    EXPECT_THROW(loader.loadScene(file), openspace::documentation::SpecificationError);
}


TEST(SceneLoaderTest, AbsoluteScenePath) {
    const std::string scenePath = absPath("${TEMPORARY}/tmp.scene");
    std::ofstream sceneFile(scenePath.c_str());

    ghoul::DictionaryLuaFormatter formatter;
    ghoul::Dictionary sceneFileDictionary;
    ghoul::Dictionary cameraDictionary;

    cameraDictionary.setValue<glm::vec3>("Position", glm::vec3(0.0));
    cameraDictionary.setValue<glm::vec4>("Rotation", glm::vec4(0.0));
    cameraDictionary.setValue<std::string>("Focus", "Root");

    sceneFileDictionary.setValue<std::string>("ScenePath", absPath("${TESTDIR}/SceneLoaderTest/scene-folder"));
    sceneFileDictionary.setValue<ghoul::Dictionary>("Modules", ghoul::Dictionary());
    sceneFileDictionary.setValue<ghoul::Dictionary>("Camera", cameraDictionary);

    sceneFile << "return " << formatter.format(sceneFileDictionary);
    sceneFile.close();

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(scenePath);
    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 1) << "Expected scene to consist of one root node";
}

TEST(SceneLoaderTest, Test00) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test00.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr"; 
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 1) << "Expected scene to consist of one root node";
}


TEST(SceneLoaderTest, Test00Location) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test00-location.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 1) << "Expected scene to consist of one root node";
}


TEST(SceneLoaderTest, Test01) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test01.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 2) << "Expected scene to consist of two nodes";

    std::map<std::string, openspace::SceneGraphNode*> nodesByName = scene->nodesByName();
    EXPECT_EQ(nodesByName.size(), 2) << "Expected scene to consist of two nodes";
    EXPECT_EQ(nodesByName["Root"]->name(), "Root");
    EXPECT_EQ(nodesByName["NoDependency"]->name(), "NoDependency");
    EXPECT_EQ(nodesByName["NoDependency"]->parent(), nodesByName["Root"]);
}

TEST(SceneLoaderTest, Test01Location) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test01-location.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 2) << "Expected scene to consist of two nodes";
    
    std::map<std::string, openspace::SceneGraphNode*> nodesByName = scene->nodesByName();
    EXPECT_EQ(nodesByName.size(), 2) << "Expected scene to consist of two nodes";
    EXPECT_EQ(nodesByName["Root"]->name(), "Root");
    EXPECT_EQ(nodesByName["NoDependency"]->name(), "NoDependency");
    EXPECT_EQ(nodesByName["NoDependency"]->parent(), nodesByName["Root"]);
}

TEST(SceneLoaderTest, Test02) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test02.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 3) << "Expected scene to consist of two nodes";

    std::map<std::string, openspace::SceneGraphNode*> nodesByName = scene->nodesByName();
    EXPECT_EQ(nodesByName.size(), 3) << "Expected scene to consist of two nodes";
    EXPECT_EQ(nodesByName["Root"]->name(), "Root");
    EXPECT_EQ(nodesByName["NoDependency"]->name(), "NoDependency");
    EXPECT_EQ(nodesByName["NoDependency"]->parent(), nodesByName["Root"]);
    EXPECT_EQ(nodesByName["Child"]->parent(), nodesByName["NoDependency"]);

    EXPECT_EQ(nodesByName["Root"]->dependencies().size(), 0);
    EXPECT_EQ(nodesByName["NoDependency"]->dependencies().size(), 0);
    EXPECT_EQ(nodesByName["Child"]->dependencies().size(), 0);
}

TEST(SceneLoaderTest, Test02Location) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test02-location.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 3) << "Expected scene to consist of three nodes";

    std::map<std::string, openspace::SceneGraphNode*> nodesByName = scene->nodesByName();
    EXPECT_EQ(nodesByName.size(), 3) << "Expected scene to consist of three nodes";
    EXPECT_EQ(nodesByName["Root"]->name(), "Root");
    EXPECT_EQ(nodesByName["NoDependency"]->name(), "NoDependency");
    EXPECT_EQ(nodesByName["Child"]->name(), "Child");

    EXPECT_EQ(nodesByName["NoDependency"]->parent(), nodesByName["Root"]);
    EXPECT_EQ(nodesByName["Child"]->parent(), nodesByName["NoDependency"]);

    EXPECT_EQ(nodesByName["Root"]->dependencies().size(), 0);
    EXPECT_EQ(nodesByName["NoDependency"]->dependencies().size(), 0);
    EXPECT_EQ(nodesByName["Child"]->dependencies().size(), 0);
}

TEST(SceneLoaderTest, Test03) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test03.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 3) << "Expected scene to consist of three nodes";

    std::map<std::string, openspace::SceneGraphNode*> nodesByName = scene->nodesByName();
    EXPECT_EQ(nodesByName.size(), 3) << "Expected scene to consist of three nodes";
    EXPECT_EQ(nodesByName["Root"]->name(), "Root");
    EXPECT_EQ(nodesByName["NoDependency"]->name(), "NoDependency");
    EXPECT_EQ(nodesByName["Dependent"]->name(), "Dependent");

    EXPECT_EQ(nodesByName["NoDependency"]->parent(), nodesByName["Root"]);
    EXPECT_EQ(nodesByName["Dependent"]->parent(), nodesByName["Root"]);

    EXPECT_EQ(nodesByName["Root"]->dependencies().size(), 0);
    EXPECT_EQ(nodesByName["NoDependency"]->dependencies().size(), 0);
    EXPECT_EQ(nodesByName["Dependent"]->dependencies().size(), 1);

    EXPECT_EQ(nodesByName["Dependent"]->dependencies()[0], nodesByName["NoDependency"]);
}

TEST(SceneLoaderTest, Test04) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test04.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);

    ASSERT_NE(scene, nullptr) << "loadScene returned nullptr";
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();
    EXPECT_EQ(nodes.size(), 5) << "Expected scene to consist of five nodes";

    std::map<std::string, openspace::SceneGraphNode*> nodesByName = scene->nodesByName();
    EXPECT_EQ(nodesByName.size(), 5) << "Expected scene to consist of five nodes";
    EXPECT_EQ(nodesByName["Root"]->name(), "Root");
    EXPECT_EQ(nodesByName["NoDependency"]->name(), "NoDependency");
    EXPECT_EQ(nodesByName["Dependent"]->name(), "Dependent");
    EXPECT_EQ(nodesByName["ChildAndDependent"]->name(), "ChildAndDependent");

    EXPECT_EQ(nodesByName["NoDependency"]->parent(), nodesByName["Root"]);
    EXPECT_EQ(nodesByName["Child"]->parent(), nodesByName["NoDependency"]);
    EXPECT_EQ(nodesByName["Dependent"]->parent(), nodesByName["Root"]);
    EXPECT_EQ(nodesByName["ChildAndDependent"]->parent(), nodesByName["NoDependency"]);

    EXPECT_EQ(nodesByName["Root"]->dependencies().size(), 0);
    EXPECT_EQ(nodesByName["NoDependency"]->dependencies().size(), 0);

    EXPECT_EQ(nodesByName["Dependent"]->dependencies().size(), 1);
    EXPECT_EQ(nodesByName["Dependent"]->dependencies()[0], nodesByName["NoDependency"]);

    EXPECT_EQ(nodesByName["ChildAndDependent"]->dependencies().size(), 1);
    EXPECT_EQ(nodesByName["ChildAndDependent"]->dependencies()[0], nodesByName["Dependent"]);
}

TEST(SceneLoaderTest, Test05) {
    const std::string file = absPath("${TESTDIR}/SceneLoaderTest/test05.scene");

    openspace::SceneLoader loader;
    std::unique_ptr<openspace::Scene> scene = loader.loadScene(file);
    std::vector<openspace::SceneGraphNode*> nodes = scene->allSceneGraphNodes();

    EXPECT_EQ(nodes.size(), 1);
    // TODO: Add more tests regarding circular deps.
}
