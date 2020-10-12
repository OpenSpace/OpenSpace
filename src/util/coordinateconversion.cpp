/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/util/coordinateconversion.h>

namespace openspace {

glm::dvec3 icrsToGalacticCartesian(float ra, float dec, double distance) {
    // Convert to Galactic Coordinates from ICRS right ascension and declination
    // https://gea.esac.esa.int/archive/documentation/GDR2/Data_processing/
    // chap_cu3ast/sec_cu3ast_intro/ssec_cu3ast_intro_tansforms.html#SSS1
    const glm::dmat3 conversionMatrix = glm::dmat3({
        -0.0548755604162154,  0.4941094278755837, -0.8676661490190047, // col 0
        -0.8734370902348850, -0.4448296299600112, -0.1980763734312015, // col 1
        -0.4838350155487132,  0.7469822444972189,  0.4559837761750669  // col 2
    });

    glm::dvec3 rICRS = glm::dvec3(
        cos(glm::radians(ra)) * cos(glm::radians(dec)),
        sin(glm::radians(ra)) * cos(glm::radians(dec)),
        sin(glm::radians(dec))
    );
    glm::dvec3 rGalactic = conversionMatrix * rICRS; // on the unit sphere

    return distance * rGalactic;
}

} // namespace openspace
