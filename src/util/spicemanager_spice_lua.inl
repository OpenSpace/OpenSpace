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

[[codegen::luawrap]] void appndc(std::string item, SpiceCell* cell) {
    appndc_c(item.c_str(), cell);
}

[[codegen::luawrap]] void appndd(double item, SpiceCell* cell) {
    appndd_c(item, cell);
}

[[codegen::luawrap]] void appndi(int item, SpiceCell* cell) {
    appndi_c(item, cell);
}

[[codegen::luawrap]] glm::dmat3 axisar(glm::dvec3 axis, double angle) {
    double res[3][3];
    axisar_c(glm::value_ptr(axis), angle, res);
    return glm::dmat3(
        res[0][0], res[0][1], res[0][2],
        res[1][0], res[1][1], res[1][2],
        res[2][0], res[2][1], res[2][2]
    );
}

#include "spicemanager_spice_lua_codegen.cpp"

} // namespace
