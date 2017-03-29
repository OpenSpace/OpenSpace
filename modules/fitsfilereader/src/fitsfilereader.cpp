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
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <CCfits>
#include <algorithm>
#include <cstring>

using namespace CCfits;
using namespace ghoul::opengl;

#define FITS_PRECISION float // Might use this later from header, but will probably just read everything as floats
#define FITS_DATA_TYPE_OPENGL GL_FLOAT

namespace {
    const std::string _loggerCat = "FitsFileReader";
    bool _printFirst = true;
}

namespace openspace {

std::unique_ptr<Texture> FitsFileReader::loadTexture(std::string& path) {
    FITS::setVerboseMode(true);
    std::valarray<float> contents;
    long sizeX;
    long sizeY;

    try {
        const std::auto_ptr<FITS> pInfile(new FITS(path, Read, true));
        PHDU& image = pInfile->pHDU();
        image.read(contents);
        sizeX = image.axis(0);
        sizeY = image.axis(1);
    } catch (FitsException& e) {
        LERROR("Could not load FITS Texture");
    }

    const glm::size3_t imageSize(sizeX, sizeY, 1);
    const Texture::Format format = ghoul::opengl::Texture::Red;
    const Texture::FilterMode filterMode = Texture::FilterMode::Linear;

    // Let texture take ownership of memory
    float* data = new float[contents.size()];
    std::memmove(data, &contents[0], contents.size() * sizeof(float));

    return std::make_unique<Texture>(
                                data,
                                imageSize,
                                format, // Format of the pixeldata
                                GL_R32F, // INTERNAL format. More preferable to give explicit precision here, otherwise up to the driver to decide
                                FITS_DATA_TYPE_OPENGL, // Type of data
                                Texture::FilterMode::Linear,
                                Texture::WrappingMode::Repeat
                            );
}

std::valarray<float> FitsFileReader::readImage(std::string& path) {
    FITS::setVerboseMode(true);

    try {
        const std::auto_ptr<FITS> pInfile(new FITS(path, Read, true));
        PHDU& image = pInfile->pHDU();
        std::valarray<float> contents;
        image.read(contents);
        return std::move(contents);
    } catch (FitsException& e){
        LERROR("Could not read FITS image");
    }
}

std::unique_ptr<Texture> FitsFileReader::loadTextureFromMemory(std::string& buffer) {
    fitsfile* infile;
    // Get string adress
    const char* memory = buffer.c_str();
    size_t size = buffer.size() * sizeof(std::string::value_type);
    void* v = const_cast<char*>(memory);
    int status = 0;
    if (fits_open_memfile(&infile, "", READONLY, &v, &size, 0, NULL, &status)) {
        LERROR("Error opening file");
        fits_report_error(stderr, status);
    }

    int numAxis = 0;
    fits_get_img_dim(infile, &numAxis, &status);
    if (numAxis != 2) {
        LERROR("Only support images with 2 axes");
    }

    long axLengths[2];
    if (fits_get_img_size(infile, 2, axLengths, &status)) {
        LERROR("Error in getting image size");
        fits_report_error(stderr, status);
    }

    int numPixels = axLengths[0] * axLengths[1];
    // Set up fpixel for a full image read
    long fpixel[2] = {1, 1};

    // Allocate space for the image - TODO do this C++ style
    float* imageArray = (float*)calloc(numPixels, sizeof(float));
    fits_read_pix(infile, TFLOAT, fpixel, numPixels, NULL, imageArray, NULL, &status);

    if (fits_close_file(infile, &status)) {
        LERROR("Error closing file");
        fits_report_error(stderr, status);
    }

    // Still ugly workaround
    unsigned char* imageData = new unsigned char[numPixels];
    for (int i = 0; i < numPixels; i++) {
        imageData[i] = (unsigned char) imageArray[i];
    }

    const glm::size3_t imageSize(axLengths[0], axLengths[1], 1);
    const Texture::Format format = ghoul::opengl::Texture::RedInt;
    const Texture::FilterMode filterMode = Texture::FilterMode::Linear;

    std::unique_ptr<Texture> texture = std::make_unique<Texture>(
                                                            imageData,
                                                            imageSize,
                                                            format,
                                                            static_cast<int>(format),
                                                            FITS_DATA_TYPE_OPENGL,
                                                            Texture::FilterMode::Linear
                                                        );
    return texture;
}

ExtHDU& FitsFileReader::readHeader(std::string& path) {
    FITS::setVerboseMode(true);
    std::string SPECTRUM = "SPECTRUM";

    try {
        std::auto_ptr<FITS> pInfile(new FITS(path, Read, std::string("SPECTRUM")));
        ExtHDU& table = pInfile->extension(SPECTRUM);
        table.readAllKeys();
        return table;
    } catch (FitsException& e) {
        LERROR("Could not read FITS header");
    }
}

void FitsFileReader::dump(PHDU& image) {
    std::valarray<unsigned long> contents;
    image.read(contents);
    std::cout << image << std::endl;

    long ax1(image.axis(0));
    long ax2(image.axis(1));

    for (long j = 0; j < ax2; j+=10) {
        std::ostream_iterator<short> c(std::cout,"\t");
        std::copy(&contents[0]+j*ax1,&contents[0]+(j+1)*ax1,c);
        std::cout << '\n';
    }
    std::cout << std::endl;
}
} // namespace openspace
