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

namespace openspace::exoplanets {

std::string getSpeckStarName(std::string csvName) {
    std::string name = csvName;
    if (csvName == "HD 1237")
        name = "GJ 3021";
    else if (csvName == "MOA-2009-BLG-387L")
        name = "MOA 2009-BLG-387L";
    else if (csvName == "HD 126614 A")
        name = "HD 126614";
    else if (csvName == "epsilon Ret")
        name = "HD 27442";
    else if (csvName == "PH-1")
        name = "PH1";
    else if (csvName == "gamma Leo A")
        name = "gam 1 Leo";
    else if (csvName == "OGLE-2007-BLG-368L")
        name = "OGLE 2007-BLG-368L";
    else if (csvName == "alpha Ari")
        name = "alf Ari";
    else if (csvName == "mu Ara")
        name = "HD 160691";
    else if (csvName == "OGLE-05-169L")
        name = "OGLE 2005-BLG-169L";
    else if (csvName == "tau Gru")
        name = "HD 216435";
    else if (csvName == "iota Hor")
        name = "HR 810";
    else if (csvName == "OGLE-05-071L")
        name = "OGLE 2005-BLG-71L";
    else if (csvName == "OGLE235-MOA53")
        name = "OGLE 2003-BLG-235L";
    else if (csvName == "MOA-2008-BLG-310L")
        name = "MOA 2008-BLG-310L";
    else if (csvName == "KIC 11442793")
        name = "KOI-351";
    else if (csvName == "OGLE-2006-BLG-109L")
        name = "OGLE 2006-BLG-109L";
    else if (csvName == "HD 137388")
        name = "HD 137388 A";
    else if (csvName == "kappa CrB")
        name = "kap CrB";
    else if (csvName == "XO-2")
        name = "XO-2 N";
    else if (csvName == "epsilon Tau")
        name = "eps Tau";
    else if (csvName == "epsilon Eri")
        name = "eps Eri";
    else if (csvName == "Kepler-448")
        name = "KOI-12";
    else if (csvName == "omega Ser")
        name = "ome Ser";
    else if (csvName == "MOA-2010-BLG-477L")
        name = "MOA 2010-BLG-477L";
    else if (csvName == "GJ 176")
        name = "HD 285968";
    else if (csvName == "HIP 2247")
        name = "BD-17 63";
    else if (csvName == "MOA-2009-BLG-266L")
        name = "MOA 2009-BLG-266L";
    else if (csvName == "Kepler-89")
        name = "KOI-94";
    else if (csvName == "iota Dra")
        name = "HIP 75458";
    else if (csvName == "MOA-2007-BLG-400L")
        name = "MOA 2007-BLG-400L";
    else if (csvName == "upsilon And")
        name = "ups And";
    else if (csvName == "OGLE-2011-BLG-0251")
        name = "OGLE 2011-BLG-251L";
    else if (csvName == "OGLE-05-390L")
        name = "OGLE 2005-BLG-390L";
    else if (csvName == "Kepler-420")
        name = "KOI-1257";
    else if (csvName == "beta Pic")
        name = "bet Pic";
    else if (csvName == "gamma Cep")
        name = "gam Cep";
    else if (csvName == "MOA-2007-BLG-192L")
        name = "MOA 2007-BLG-192L";
    else if (csvName == "MOA-2009-BLG-319L")
        name = "MOA 2009-BLG-319L";
    else if (csvName == "omicron CrB")
        name = "omi CrB";
    else if (csvName == "beta Gem")
        name = "HD 62509";
    else if (csvName == "epsilon CrB")
        name = "eps CrB";
    else if (csvName == "omicron UMa")
        name = "omi UMa";
    else if (csvName == "HD 142022")
        name = "HD 142022 A";

    return name;
}

std::string getCsvStarName(std::string name) {
    std::string csvName = name;
    if (name == "GJ 3021")
        csvName = "HD 1237";
    else if (name == "MOA 2009-BLG-387L")
        csvName = "MOA-2009-BLG-387L";
    else if (name == "HD 126614")
        csvName = "HD 126614 A";
    else if (name == "HD 27442")
        csvName = "epsilon Ret";
    else if (name == "PH1")
        csvName = "PH-1";
    else if (name == "gam 1 Leo")
        csvName = "gamma Leo A";
    else if (name == "OGLE 2007-BLG-368L")
        csvName = "OGLE-2007-BLG-368L";
    else if (name == "alf Ari")
        csvName = "alpha Ari";
    else if (name == "HD 160691")
        csvName = "mu Ara";
    else if (name == "OGLE 2005-BLG-169L")
        csvName = "OGLE-05-169L";
    else if (name == "HD 216435")
        csvName = "tau Gru";
    else if (name == "HR 810")
        csvName = "iota Hor";
    else if (name == "OGLE 2005-BLG-71L")
        csvName = "OGLE-05-071L";
    else if (name == "OGLE 2003-BLG-235L")
        csvName = "OGLE235-MOA53";
    else if (name == "MOA 2008-BLG-310L")
        csvName = "MOA-2008-BLG-310L";
    else if (name == "KOI-351")
        csvName = "KIC 11442793";
    else if (name == "OGLE 2006-BLG-109L")
        csvName = "OGLE-2006-BLG-109L";
    else if (name == "HD 137388 A")
        csvName = "HD 137388";
    else if (name == "kap CrB")
        csvName = "kappa CrB";
    else if (name == "XO-2 N")
        csvName = "XO-2";
    else if (name == "eps Tau")
        csvName = "epsilon Tau";
    else if (name == "eps Eri")
        csvName = "epsilon Eri";
    else if (name == "KOI-12")
        csvName = "Kepler-448";
    else if (name == "ome Ser")
        csvName = "omega Ser";
    else if (name == "MOA 2010-BLG-477L")
        csvName = "MOA-2010-BLG-477L";
    else if (name == "HD 285968")
        csvName = "GJ 176";
    else if (name == "BD-17 63")
        csvName = "HIP 2247";
    else if (name == "MOA 2009-BLG-266L")
        csvName = "MOA-2009-BLG-266L";
    else if (name == "KOI-94")
        csvName = "Kepler-89";
    else if (name == "HIP 75458")
        csvName = "iota Dra";
    else if (name == "MOA 2007-BLG-400L")
        csvName = "MOA-2007-BLG-400L";
    else if (name == "ups And")
        csvName = "upsilon And";
    else if (name == "OGLE 2011-BLG-251L")
        csvName = "OGLE-2011-BLG-0251";
    else if (name == "OGLE 2005-BLG-390L")
        csvName = "OGLE-05-390L";
    else if (name == "KOI-1257")
        csvName = "Kepler-420";
    else if (name == "bet Pic")
        csvName = "beta Pic";
    else if (name == "gam Cep")
        csvName = "gamma Cep";
    else if (name == "MOA 2007-BLG-192L")
        csvName = "MOA-2007-BLG-192L";
    else if (name == "MOA 2009-BLG-319L")
        csvName = "MOA-2009-BLG-319L";
    else if (name == "omi CrB")
        csvName = "omicron CrB";
    else if (name == "HD 62509")
        csvName = "beta Gem";
    else if (name == "eps CrB")
        csvName = "epsilon CrB";
    else if (name == "omi UMa")
        csvName = "omicron UMa";
    else if (name == "HD 142022 A")
        csvName = "HD 142022";

    return csvName;
}

} // namespace openspace::exoplanets
