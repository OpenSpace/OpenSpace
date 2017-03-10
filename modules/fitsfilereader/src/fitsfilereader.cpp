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
#include <CCfits>

using namespace CCfits;

namespace {
    const std::string _loggerCat = "FitsFileReader";
}

namespace openspace {

std::valarray<unsigned long> FitsFileReader::readRawImage(std::string path) {
    FITS::setVerboseMode(true);

    try {
        const std::auto_ptr<FITS> pInfile(new FITS(path, Read, true));
        PHDU& image = pInfile->pHDU();
        std::valarray<unsigned long> contents;
        image.read(contents);
        return contents;
    } catch (FitsException& e){
        LERROR("Could not read FITS image");
    }
}

ExtHDU& FitsFileReader::readHeader(std::string path) {
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
