/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <iostream>
#include <string>
#include <glm/glm.hpp>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/io/texture/texturereaderdevil.h>
#include <ghoul/io/texture/texturereaderfreeimage.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/ghoul.h>

#include <openspace/util/progressbar.h>

#include <apps/DataConverter/milkywayconversiontask.h>

int main(int argc, char** argv) {
    using namespace openspace;
    using namespace dataconverter;

    ghoul::initialize();

    #ifdef GHOUL_USE_DEVIL
        ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderDevIL>());
    #endif // GHOUL_USE_DEVIL
    #ifdef GHOUL_USE_FREEIMAGE
        ghoul::io::TextureReader::ref().addReader(std::make_shared<ghoul::io::TextureReaderFreeImage>());
    #endif // GHOUL_USE_FREEIMAGE

    openspace::ProgressBar pb(100);
    std::function<void(float)> onProgress = [&](float progress) {
        pb.print(progress * 100);
    };

    // TODO: Make the converter configurable using either
    // config files (json, lua dictionaries),
    // lua scripts,
    // or at the very least: a command line interface.
 
    MilkyWayConversionTask mwConversionTask(
        "F:/milky-way/cam2_main.",
        ".exr",
        1385,
        512,
        "F:/milky-way/mw_512_512_64.rawvolume", 
        glm::vec3(512, 512, 64));
    
    mwConversionTask.perform(onProgress);

    std::cout << "Done." << std::endl;

    std::cin.get();
    return 0;
};
