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
 ****************************************************************************************/

#include <ghoul/systemcapabilities/version.h>

#include <ghoul/format.h>

namespace {
    constexpr unsigned int packVersion(int major, int minor, int release) {
        // Safe since: 2^8 * 1000 * 1000 < 2^32
        return major * 1000 * 1000  +  minor * 1000  +  release;
    }
} // namespace

namespace ghoul {
    template <>
    std::string to_string(const systemcapabilities::Version& v) {
        if (v.release != 0) {
            return std::format("{}.{}.{}", v.major, v.minor, v.release);
        }
        else {
            return std::format("{}.{}", v.major, v.minor);
        }
    }
} // namespace ghoul

namespace ghoul::systemcapabilities {

std::strong_ordering Version::operator<=>(const Version& rhs) const noexcept {
    const unsigned int numThis = packVersion(major, minor, release);
    const unsigned int numRhs = packVersion(rhs.major, rhs.minor, rhs.release);
    return numThis <=> numRhs;
}

} // namespace ghoul::systemcapabilities
