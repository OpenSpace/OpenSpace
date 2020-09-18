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
    if (csvName == "HD 1237") { return "GJ 3021"; }
    if (csvName == "MOA-2009-BLG-387L") { return "MOA 2009-BLG-387L"; }
    if (csvName == "HD 126614 A") { return "HD 126614"; }
    if (csvName == "epsilon Ret") { return "HD 27442"; }
    if (csvName == "PH-1") { return "PH1"; }
    if (csvName == "gamma Leo A") { return "gam 1 Leo"; }
    if (csvName == "OGLE-2007-BLG-368L") { return "OGLE 2007-BLG-368L"; }
    if (csvName == "alpha Ari") { return "alf Ari"; }
    if (csvName == "mu Ara") { return "HD 160691"; }
    if (csvName == "OGLE-05-169L") { return "OGLE 2005-BLG-169L"; }
    if (csvName == "tau Gru") { return "HD 216435"; }
    if (csvName == "iota Hor") { return "HR 810"; }
    if (csvName == "OGLE-05-071L") { return "OGLE 2005-BLG-71L"; }
    if (csvName == "OGLE235-MOA53") { return "OGLE 2003-BLG-235L"; }
    if (csvName == "MOA-2008-BLG-310L") { return "MOA 2008-BLG-310L"; }
    if (csvName == "KIC 11442793") { return "KOI-351"; }
    if (csvName == "OGLE-2006-BLG-109L") { return "OGLE 2006-BLG-109L"; }
    if (csvName == "HD 137388") { return "HD 137388 A"; }
    if (csvName == "kappa CrB") { return "kap CrB"; }
    if (csvName == "XO-2") { return "XO-2 N"; }
    if (csvName == "epsilon Tau") { return  "eps Tau"; }
    if (csvName == "epsilon Eri") { return "eps Eri"; }
    if (csvName == "Kepler-448") { return "KOI-12"; }
    if (csvName == "omega Ser") { return "ome Ser"; }
    if (csvName == "MOA-2010-BLG-477L") { return "MOA 2010-BLG-477L"; }
    if (csvName == "GJ 176") { return "HD 285968"; }
    if (csvName == "HIP 2247") { return "BD-17 63"; }
    if (csvName == "MOA-2009-BLG-266L") { return "MOA 2009-BLG-266L"; }
    if (csvName == "Kepler-89") { return "KOI-94"; }
    if (csvName == "iota Dra") { return "HIP 75458"; }
    if (csvName == "MOA-2007-BLG-400L") { return "MOA 2007-BLG-400L"; }
    if (csvName == "upsilon And") { return "ups And"; }
    if (csvName == "OGLE-2011-BLG-0251") { return "OGLE 2011-BLG-251L"; }
    if (csvName == "OGLE-05-390L") { return "OGLE 2005-BLG-390L"; }
    if (csvName == "Kepler-420") { return "KOI-1257"; }
    if (csvName == "beta Pic") { return "bet Pic"; }
    if (csvName == "gamma Cep") { return "gam Cep"; }
    if (csvName == "MOA-2007-BLG-192L") { return "MOA 2007-BLG-192L"; }
    if (csvName == "MOA-2009-BLG-319L") { return "MOA 2009-BLG-319L"; }
    if (csvName == "omicron CrB") { return "omi CrB"; }
    if (csvName == "beta Gem") { return "HD 62509"; }
    if (csvName == "epsilon CrB") { return "eps CrB"; }
    if (csvName == "omicron UMa") { return "omi UMa"; }
    if (csvName == "HD 142022") { return "HD 142022 A"; }
    return csvName;
}

std::string_view csvStarName(std::string_view name) {
    if (name == "GJ 3021") { return "HD 1237"; }
    if (name == "MOA 2009-BLG-387L") { return "MOA-2009-BLG-387L"; }
    if (name == "HD 126614") { return "HD 126614 A"; }
    if (name == "HD 27442") { return "epsilon Ret"; }
    if (name == "PH1") { return "PH-1"; }
    if (name == "gam 1 Leo") { return "gamma Leo A"; }
    if (name == "OGLE 2007-BLG-368L") { return "OGLE-2007-BLG-368L"; }
    if (name == "alf Ari") { return "alpha Ari"; }
    if (name == "HD 160691") { return "mu Ara"; }
    if (name == "OGLE 2005-BLG-169L") { return "OGLE-05-169L"; }
    if (name == "HD 216435") { return "tau Gru"; }
    if (name == "HR 810") { return "iota Hor"; }
    if (name == "OGLE 2005-BLG-71L") { return "OGLE-05-071L"; }
    if (name == "OGLE 2003-BLG-235L") { return "OGLE235-MOA53"; }
    if (name == "MOA 2008-BLG-310L") { return "MOA-2008-BLG-310L"; }
    if (name == "KOI-351") { return "KIC 11442793"; }
    if (name == "OGLE 2006-BLG-109L") { return "OGLE-2006-BLG-109L"; }
    if (name == "HD 137388 A") { return "HD 137388"; }
    if (name == "kap CrB") { return "kappa CrB"; }
    if (name == "XO-2 N") { return "XO-2"; }
    if (name == "eps Tau") { return "epsilon Tau"; }
    if (name == "eps Eri") { return "epsilon Eri"; }
    if (name == "KOI-12") { return "Kepler-448"; }
    if (name == "ome Ser") { return "omega Ser"; }
    if (name == "MOA 2010-BLG-477L") { return "MOA-2010-BLG-477L"; }
    if (name == "HD 285968") { return "GJ 176"; }
    if (name == "BD-17 63") { return "HIP 2247"; }
    if (name == "MOA 2009-BLG-266L") { return "MOA-2009-BLG-266L"; }
    if (name == "KOI-94") { return "Kepler-89"; }
    if (name == "HIP 75458") { return "iota Dra"; }
    if (name == "MOA 2007-BLG-400L") { return "MOA-2007-BLG-400L"; }
    if (name == "ups And") { return "upsilon And"; }
    if (name == "OGLE 2011-BLG-251L") { return "OGLE-2011-BLG-0251"; }
    if (name == "OGLE 2005-BLG-390L") { return "OGLE-05-390L"; }
    if (name == "KOI-1257") { return "Kepler-420"; }
    if (name == "bet Pic") { return "beta Pic"; }
    if (name == "gam Cep") { return "gamma Cep"; }
    if (name == "MOA 2007-BLG-192L") { return "MOA-2007-BLG-192L"; }
    if (name == "MOA 2009-BLG-319L") { return "MOA-2009-BLG-319L"; }
    if (name == "omi CrB") { return "omicron CrB"; }
    if (name == "HD 62509") { return "beta Gem"; }
    if (name == "eps CrB") { return "epsilon CrB"; }
    if (name == "omi UMa") { return "omicron UMa"; }
    if (name == "HD 142022 A") { return "HD 142022"; }
    return name;
}

} // namespace openspace::exoplanets
