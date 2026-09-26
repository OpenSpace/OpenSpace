/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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
 *****************************************************************************************
 * This file is inspired by Peter Kankowski (kankowski@narod.ru) and his work available  *
 * at http://www.strchr.com/hash_functions and http://www.strchr.com/crc32_popcnt        *
 *****************************************************************************************
 * Slicing-by-8 algorithms by Michael E. Kounavis and Frank L. Berry from Intel Corp.    *
 * http://www.intel.com/technology/comms/perfnet/download/CRC_generators.pdf             *
 ****************************************************************************************/

#include <ghoul/misc/crc32.h>

#include <fstream>
#include <vector>

namespace ghoul {

unsigned int hashCRC32File(const std::string& file) {
    std::fstream f = std::fstream(file);
    f.seekg(0, std::ios::end);
    const unsigned int length = static_cast<int>(f.tellg());
    f.seekg(0, std::ios::beg);
    std::vector<char> buffer = std::vector<char>(length);
    f.read(buffer.data(), length);

    return hashCRC32(buffer.data(), length);
}

} // namespace ghoul
