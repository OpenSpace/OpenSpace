/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2014                                                               *
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

#include <openspace/scenegraph/scenegraph.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/time.h>
#include <openspace/util/spice.h>
#include <openspace/util/factorymanager.h>

class SceneGraphTest : public testing::Test {
protected:
    SceneGraphTest() {
        _scenegraph = new openspace::SceneGraph;
    }

    ~SceneGraphTest() {
        _scenegraph = new openspace::SceneGraph;
    }

    void reset() {
        delete _scenegraph;
        _scenegraph = new openspace::SceneGraph;
    }

    openspace::SceneGraph* _scenegraph;
};

TEST_F(SceneGraphTest, SceneGraphNode) {

    openspace::SceneGraphNode *node =
        openspace::SceneGraphNode::createFromDictionary(ghoul::Dictionary());

    // Should not have a renderable and position should be 0,0,0,0 (undefined).
    EXPECT_EQ(nullptr, node->getRenderable());
    EXPECT_EQ(openspace::psc(), node->getPosition());
    
    delete node;
    ghoul::Dictionary nodeDictionary;
    
    ghoul::Dictionary positionDictionary;
    ghoul::Dictionary positionPositionArrayDictionary;
    
    ghoul::Dictionary renderableDictionary;
    
    renderableDictionary.setValue("Type", std::string("RenderablePlanet"));
    
    positionPositionArrayDictionary.setValue("1", 1.0);
    positionPositionArrayDictionary.setValue("2", 1.0);
    positionPositionArrayDictionary.setValue("3", 1.0);
    positionPositionArrayDictionary.setValue("4", 1.0);
    
    positionDictionary.setValue("Type", std::string("Static"));
    positionDictionary.setValue("Position", positionPositionArrayDictionary);
    
    nodeDictionary.setValue("Position", positionDictionary);
    nodeDictionary.setValue("Renderable", renderableDictionary);
    
    node =
        openspace::SceneGraphNode::createFromDictionary(nodeDictionary);
    
    // This node should have a renderable (probably no good values but an existing one)
    EXPECT_TRUE(node->getRenderable());
    
    // position should be initialized
    EXPECT_EQ(openspace::psc(1.0,1.0,1.0,1.0), node->getPosition());
    
    delete node;
}

TEST_F(SceneGraphTest, Loading) {
    
    
    // Should not successfully load a non existing scenegraph
    EXPECT_FALSE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTestNonExisting"), absPath("${TESTDIR}")));
    
    // Existing scenegraph should load
    EXPECT_TRUE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
    // TODO need to check for correctness
    
    // This loading should fail regardless of existing or not since the
    // scenegraph is already loaded
    EXPECT_FALSE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
}

TEST_F(SceneGraphTest, Reinitializing) {
    
    // Existing scenegraph should load
    EXPECT_TRUE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
    
    _scenegraph->deinitialize();
    
    // Existing scenegraph should load
    EXPECT_TRUE(_scenegraph->loadScene(absPath("${TESTDIR}/ScenegraphTest"), absPath("${TESTDIR}")));
    // TODO need to check for correctness
}




