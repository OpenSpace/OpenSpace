/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/scenegraph/scenegraphloader.h>
#include <openspace/scenegraph/scenegraphnode.h>

#include <fstream>

class SceneGraphLoaderTest : public testing::Test {};

TEST_F(SceneGraphLoaderTest, NonExistingFileTest) {
    const std::string file = "NonExistingFile";

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    EXPECT_FALSE(success) << "Unsuccessful loading";
    EXPECT_TRUE(nodes.empty()) << "Empty scenegraph nodes list";
}

TEST_F(SceneGraphLoaderTest, IllformedFileTest) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/illformed.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    EXPECT_FALSE(success) << "Unsuccessful loading";
    EXPECT_TRUE(nodes.empty()) << "Empty scenegraph nodes list";
}

TEST_F(SceneGraphLoaderTest, IllformedFileTestWrongCommonFolderType) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/illformedWrongType.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    EXPECT_FALSE(success) << "Unsuccessful loading";
    EXPECT_TRUE(nodes.empty()) << "Empty scenegraph nodes list";
}

TEST_F(SceneGraphLoaderTest, IllformedFileTestInvalidSceneFolder) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/illformedInvalidScene.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    EXPECT_FALSE(success) << "Unsuccessful loading";
    EXPECT_TRUE(nodes.empty()) << "Empty scenegraph nodes list";
}

TEST_F(SceneGraphLoaderTest, IllformedFileTestWrongCommonFolder) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/illformedWrongCommon.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    EXPECT_FALSE(success) << "Unsuccessful loading";
    EXPECT_TRUE(nodes.empty()) << "Empty scenegraph nodes list";
}

TEST_F(SceneGraphLoaderTest, IllformedFileTestNonExistingCommonFolder) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/illformedNonExistingCommon.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    EXPECT_FALSE(success) << "Unsuccessful loading";
    EXPECT_TRUE(nodes.empty()) << "Empty scenegraph nodes list";
}

TEST_F(SceneGraphLoaderTest, Test00) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test00.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    EXPECT_TRUE(nodes.empty()) << "No scenegraph nodes loaded";
}

TEST_F(SceneGraphLoaderTest, Test00Location) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test00-location.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    EXPECT_TRUE(nodes.empty()) << "No scenegraph nodes loaded";
}

TEST_F(SceneGraphLoaderTest, AbsoluteScenePath) {
    const std::string scenePath = absPath("${TEMPORARY}/tmp.scene");
    std::ofstream scene(scenePath.c_str());

    scene << "return {" << std::endl <<
        " ScenePath = \"" << absPath("${TESTDIR}/SceneGraphLoaderTest/scene-folder") <<
        "\"," << std::endl <<
        " CommonFolder = \"\"," << std::endl <<
        " Modules = {}}" << std::endl;
    scene.close();

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(scenePath, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    EXPECT_TRUE(nodes.empty()) << "No scenegraph nodes loaded";
}

TEST_F(SceneGraphLoaderTest, Test01) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test01.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 1) << "Correct number of nodes";
    EXPECT_TRUE(nodes[0]->name() == "Common") << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test01Location) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test01-location.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 1) << "Correct number of nodes";
    EXPECT_TRUE(nodes[0]->name() == "Common") << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test02) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test02.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 2) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "NoDependency")
            found = true;
        
    EXPECT_TRUE(found) << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test02Location) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test02-location.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 2) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "NoDependency")
            found = true;

    EXPECT_TRUE(found) << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test03) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test03.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 2) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "CommonDependency")
            found = true;

    EXPECT_TRUE(found) << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test03Location) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test03-location.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 2) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "CommonDependency")
            found = true;

    EXPECT_TRUE(found) << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test04) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test04.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 3) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "DirectDependency")
            found = true;

    EXPECT_TRUE(found) << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test04Location) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test04-location.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 3) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "DirectDependency")
            found = true;

    EXPECT_TRUE(found) << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test05) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test05.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 4) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "MultipleDependencies")
            found = true;

    EXPECT_TRUE(found) << "Correct node loaded";
}

TEST_F(SceneGraphLoaderTest, Test05Location) {
    const std::string file = absPath("${TESTDIR}/SceneGraphLoaderTest/test05-location.scene");

    std::vector<openspace::SceneGraphNode*> nodes;
    bool success = openspace::SceneGraphLoader::load(file, nodes);

    ASSERT_TRUE(success) << "Successful loading";
    ASSERT_TRUE(nodes.size() == 4) << "Correct number of nodes";
    bool found = false;
    for (openspace::SceneGraphNode* n : nodes)
        if (n->name() == "MultipleDependencies")
            found = true;

    EXPECT_TRUE(found) << "Correct node loaded";
}


//
//
//
//TEST_F(SceneGraphTest, SceneGraphNode) {
//
//    openspace::SceneGraphNode *node =
//        openspace::SceneGraphNode::createFromDictionary(ghoul::Dictionary());
//
//    // Should not have a renderable and position should be 0,0,0,0 (undefined).
//    EXPECT_EQ(nullptr, node->renderable());
//    EXPECT_EQ(openspace::psc(), node->position());
//    
//    delete node;
//    ghoul::Dictionary nodeDictionary;
//    
//    ghoul::Dictionary positionDictionary;
//    ghoul::Dictionary positionPositionArrayDictionary;
//    
//    ghoul::Dictionary renderableDictionary;
//    
//    renderableDictionary.setValue("Type", std::string("RenderablePlanet"));
//    
//    positionPositionArrayDictionary.setValue("1", 1.0);
//    positionPositionArrayDictionary.setValue("2", 1.0);
//    positionPositionArrayDictionary.setValue("3", 1.0);
//    positionPositionArrayDictionary.setValue("4", 1.0);
//    
//    positionDictionary.setValue("Type", std::string("Static"));
//    positionDictionary.setValue("Position", positionPositionArrayDictionary);
//    
//    nodeDictionary.setValue("Position", positionDictionary);
//    nodeDictionary.setValue("Renderable", renderableDictionary);
//    
//    node =
//        openspace::SceneGraphNode::createFromDictionary(nodeDictionary);
//    
//    // This node should have a renderable (probably no good values but an existing one)
//    EXPECT_TRUE(node->renderable());
//    
//    // position should be initialized
//    EXPECT_EQ(openspace::psc(1.0,1.0,1.0,1.0), node->position());
//    
//    delete node;
//}
//
//TEST_F(SceneGraphTest, Loading) {
//    
//    
//    // Should not successfully load a non existing scenegraph
//    EXPECT_FALSE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTestNonExisting"), absPath("${TESTDIR}")));
//    
//    // Existing scenegraph should load
//    EXPECT_TRUE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
//    // TODO need to check for correctness
//    
//    // This loading should fail regardless of existing or not since the
//    // scenegraph is already loaded
//    EXPECT_FALSE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
//}
//
//TEST_F(SceneGraphTest, Reinitializing) {
//    
//    // Existing scenegraph should load
//    EXPECT_TRUE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
//    
//    _scenegraph->deinitialize();
//    
//    // Existing scenegraph should load
//    EXPECT_TRUE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
//    // TODO need to check for correctness
//}




