/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

namespace {

/**
 * Adds the given list of nodes to the CosmicSonification internal list of nodes
 */
[[codegen::luawrap]] void addNodes(ghoul::Dictionary nodes) {
    openspace::SonificationModule* module =
        openspace::global::moduleEngine->module<openspace::SonificationModule>();
    if (!module) {
        LERRORC("CosmicSonification", "Could not find the SonificationModule");
        return;
    }
    openspace::SonificationBase* ptr = module->sonification("CosmicSonification");
    if (!ptr) {
        LERRORC("CosmicSonification", "Could not find the CosmicSonification");
        return;
    }

    openspace::CosmicSonification* cosmic =
        reinterpret_cast<openspace::CosmicSonification*>(ptr);

    for (const std::string_view& key : nodes.keys()) {
        cosmic->addNode(nodes.value<std::string>(key));
    }
}

/**
 * Adds the given list of labels to the CosmicSonification internal list of labels
 */
[[codegen::luawrap]] void addLabels(ghoul::Dictionary labels) {
    openspace::SonificationModule* module =
        openspace::global::moduleEngine->module<openspace::SonificationModule>();
    if (!module) {
        LERRORC("CosmicSonification", "Could not find the SonificationModule");
        return;
    }
    openspace::SonificationBase* ptr = module->sonification("CosmicSonification");
    if (!ptr) {
        LERRORC("CosmicSonification", "Could not find the CosmicSonification");
        return;
    }

    openspace::CosmicSonification* cosmic =
        reinterpret_cast<openspace::CosmicSonification*>(ptr);

    for (const std::string_view& key : labels.keys()) {
        cosmic->addLabelNode(labels.value<std::string>(key));
    }
}

#include "cosmicsonification_lua_codegen.cpp"
} // namespace
