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

#include <modules/exoplanets/exoplanetshelper.h>

#include <string_view>

namespace openspace::exoplanets {

std::string_view speckStarName(std::string_view csvName) {
    if (csvName == "MOA-2009-BLG-387L") { return "MOA 2009-BLG-387L"; }
    if (csvName == "OGLE-2007-BLG-368L") { return "OGLE 2007-BLG-368L"; }
    if (csvName == "OGLE-2005-BLG-169L") { return "OGLE 2005-BLG-169L"; }
    if (csvName == "OGLE-2005-BLG-071L") { return "OGLE 2005-BLG-71L"; }
    if (csvName == "OGLE-2003-BLG-235L") { return "OGLE 2003-BLG-235L"; }
    if (csvName == "MOA-2008-BLG-310L") { return "MOA 2008-BLG-310L"; }
    if (csvName == "OGLE-2006-BLG-109L") { return "OGLE 2006-BLG-109L"; }
    if (csvName == "HD 137388") { return "HD 137388 A"; }
    if (csvName == "MOA-2010-BLG-477L") { return "MOA 2010-BLG-477L"; }
    if (csvName == "MOA-2009-BLG-266L") { return "MOA 2009-BLG-266L"; }
    if (csvName == "iot Dra") { return "HIP 75458"; }
    if (csvName == "MOA-2007-BLG-400L") { return "MOA 2007-BLG-400L"; }
    if (csvName == "OGLE-2011-BLG-0251L") { return "OGLE 2011-BLG-251L"; }
    if (csvName == "OGLE-2005-BLG-390L") { return "OGLE 2005-BLG-390L"; }
    if (csvName == "MOA-2007-BLG-192L") { return "MOA 2007-BLG-192L"; }
    if (csvName == "MOA-2009-BLG-319L") { return "MOA 2009-BLG-319L"; }
    return csvName;
}

std::string_view csvStarName(std::string_view name) {
    if (name == "MOA 2009-BLG-387L") { return "MOA-2009-BLG-387L"; }
    if (name == "OGLE 2007-BLG-368L") { return "OGLE-2007-BLG-368L"; }
    if (name == "OGLE 2005-BLG-169L") { return "OGLE-2005-BLG-169L"; }
    if (name == "OGLE 2005-BLG-71L") { return "OGLE-2005-BLG-071L"; }
    if (name == "OGLE 2003-BLG-235L") { return "OGLE-2003-BLG-235L"; }
    if (name == "MOA 2008-BLG-310L") { return "MOA-2008-BLG-310L"; }
    if (name == "OGLE 2006-BLG-109L") { return "OGLE-2006-BLG-109L"; }
    if (name == "HD 137388 A") { return "HD 137388"; }
    if (name == "MOA 2010-BLG-477L") { return "MOA-2010-BLG-477L"; }
    if (name == "MOA 2009-BLG-266L") { return "MOA-2009-BLG-266L"; }
    if (name == "HIP 75458") { return "iot Dra"; }
    if (name == "MOA 2007-BLG-400L") { return "MOA-2007-BLG-400L"; }
    if (name == "OGLE 2011-BLG-251L") { return "OGLE-2011-BLG-0251L"; }
    if (name == "OGLE 2005-BLG-390L") { return "OGLE-2005-BLG-390L"; }
    if (name == "MOA 2007-BLG-192L") { return "MOA-2007-BLG-192L"; }
    if (name == "MOA 2009-BLG-319L") { return "MOA-2009-BLG-319L"; }
    return name;
}

} // namespace openspace::exoplanets
