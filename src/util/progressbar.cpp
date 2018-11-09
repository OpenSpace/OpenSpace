/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/util/progressbar.h>

#include <iomanip>

namespace openspace {

ProgressBar::ProgressBar(int end, int width, std::ostream& stream)
    : _width(width)
    , _end(end)
    , _stream(stream)
{
    print(0);
}

ProgressBar::~ProgressBar() {
    _stream << "\n";
}

void ProgressBar::print(int current) {
    const float progress = static_cast<float>(current) / static_cast<float>(_end);
    const int iprogress = static_cast<int>(progress * 100.0f);
    if (iprogress != _previous) {
        const int pos = static_cast<int>(static_cast<float>(_width)* progress);
        const int eqWidth = pos + 1;
        const int spWidth = _width - pos + 2;
        _stream << "[" << std::setfill('=') << std::setw(eqWidth)
                << ">" << std::setfill(' ') << std::setw(spWidth)
                << "] " << std::setfill(' ') << std::setw(3) << iprogress << " %  \r"
                << std::flush;
    }
    _previous = iprogress;
}

} // namespace openspace
