/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/molecule/rendering/renderablemolecule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtc/matrix_transform.hpp>
#include <filesystem>
#include <fstream>
#include <optional>

namespace openspace {

documentation::Documentation RenderableMolecule::Documentation() {
    //return codegen::doc<Parameters>("galaxy_renderablegalaxy");
    return {};
}

RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{
}

void RenderableMolecule::initialize() {
    ZoneScoped
}

void RenderableMolecule::initializeGL() {
    ZoneScoped

}

void RenderableMolecule::deinitializeGL() {
    
}

bool RenderableMolecule::isReady() const {
    return true;
}

void RenderableMolecule::update(const UpdateData& data) {

}

void RenderableMolecule::render(const RenderData& data, RendererTasks& tasks) {
    
}

RenderableMolecule::Result RenderableMolecule::loadMoleculeFile() {
    Result res;
    res.success = true;
    return res;
}

RenderableMolecule::Result RenderableMolecule::loadTrajectoryFile() {
    Result res;
    res.success = true;
    return res;
}

} // namespace openspace
