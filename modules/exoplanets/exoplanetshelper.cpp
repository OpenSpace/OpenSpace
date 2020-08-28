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

#include <modules/exoplanets/exoplanetshelper.h>

namespace openspace::exoplanets {

std::string getSpeckStarname(std::string csvName) {
    std::string explName = csvName;
    if (csvName == "HD 1237")
        explName = "GJ 3021";
    else if (csvName == "MOA-2009-BLG-387L")
        explName = "MOA 2009-BLG-387L";
    else if (csvName == "HD 126614 A")
        explName = "HD 126614";
    else if (csvName == "epsilon Ret")
        explName = "HD 27442";
    else if (csvName == "PH-1")
        explName = "PH1";
    else if (csvName == "gamma Leo A")
        explName = "gam 1 Leo";
    else if (csvName == "OGLE-2007-BLG-368L")
        explName = "OGLE 2007-BLG-368L";
    else if (csvName == "alpha Ari")
        explName = "alf Ari";
    else if (csvName == "mu Ara")
        explName = "HD 160691";
    else if (csvName == "OGLE-05-169L")
        explName = "OGLE 2005-BLG-169L";
    else if (csvName == "tau Gru")
        explName = "HD 216435";
    else if (csvName == "iota Hor")
        explName = "HR 810";
    else if (csvName == "OGLE-05-071L")
        explName = "OGLE 2005-BLG-71L";
    else if (csvName == "OGLE235-MOA53")
        explName = "OGLE 2003-BLG-235L";
    else if (csvName == "MOA-2008-BLG-310L")
        explName = "MOA 2008-BLG-310L";
    else if (csvName == "KIC 11442793")
        explName = "KOI-351";
    else if (csvName == "OGLE-2006-BLG-109L")
        explName = "OGLE 2006-BLG-109L";
    else if (csvName == "HD 137388")
        explName = "HD 137388 A";
    else if (csvName == "kappa CrB")
        explName = "kap CrB";
    else if (csvName == "XO-2")
        explName = "XO-2 N";
    else if (csvName == "epsilon Tau")
        explName = "eps Tau";
    else if (csvName == "epsilon Eri")
        explName = "eps Eri";
    else if (csvName == "Kepler-448")
        explName = "KOI-12";
    else if (csvName == "omega Ser")
        explName = "ome Ser";
    else if (csvName == "MOA-2010-BLG-477L")
        explName = "MOA 2010-BLG-477L";
    else if (csvName == "GJ 176")
        explName = "HD 285968";
    else if (csvName == "HIP 2247")
        explName = "BD-17 63";
    else if (csvName == "MOA-2009-BLG-266L")
        explName = "MOA 2009-BLG-266L";
    else if (csvName == "Kepler-89")
        explName = "KOI-94";
    else if (csvName == "iota Dra")
        explName = "HIP 75458";
    else if (csvName == "MOA-2007-BLG-400L")
        explName = "MOA 2007-BLG-400L";
    else if (csvName == "upsilon And")
        explName = "ups And";
    else if (csvName == "OGLE-2011-BLG-0251")
        explName = "OGLE 2011-BLG-251L";
    else if (csvName == "OGLE-05-390L")
        explName = "OGLE 2005-BLG-390L";
    else if (csvName == "Kepler-420")
        explName = "KOI-1257";
    else if (csvName == "beta Pic")
        explName = "bet Pic";
    else if (csvName == "gamma Cep")
        explName = "gam Cep";
    else if (csvName == "MOA-2007-BLG-192L")
        explName = "MOA 2007-BLG-192L";
    else if (csvName == "MOA-2009-BLG-319L")
        explName = "MOA 2009-BLG-319L";
    else if (csvName == "omicron CrB")
        explName = "omi CrB";
    else if (csvName == "beta Gem")
        explName = "HD 62509";
    else if (csvName == "epsilon CrB")
        explName = "eps CrB";
    else if (csvName == "omicron UMa")
        explName = "omi UMa";
    else if (csvName == "HD 142022")
        explName = "HD 142022 A";

    return explName;
}

std::string getCsvStarname(std::string explName) {
    std::string csvName = explName;
    if (explName == "GJ 3021")
        csvName = "HD 1237";
    else if (explName == "MOA 2009-BLG-387L")
        csvName = "MOA-2009-BLG-387L";
    else if (explName == "HD 126614")
        csvName = "HD 126614 A";
    else if (explName == "HD 27442")
        csvName = "epsilon Ret";
    else if (explName == "PH1")
        csvName = "PH-1";
    else if (explName == "gam 1 Leo")
        csvName = "gamma Leo A";
    else if (explName == "OGLE 2007-BLG-368L")
        csvName = "OGLE-2007-BLG-368L";
    else if (explName == "alf Ari")
        csvName = "alpha Ari";
    else if (explName == "HD 160691")
        csvName = "mu Ara";
    else if (explName == "OGLE 2005-BLG-169L")
        csvName = "OGLE-05-169L";
    else if (explName == "HD 216435")
        csvName = "tau Gru";
    else if (explName == "HR 810")
        csvName = "iota Hor";
    else if (explName == "OGLE 2005-BLG-71L")
        csvName = "OGLE-05-071L";
    else if (explName == "OGLE 2003-BLG-235L")
        csvName = "OGLE235-MOA53";
    else if (explName == "MOA 2008-BLG-310L")
        csvName = "MOA-2008-BLG-310L";
    else if (explName == "KOI-351")
        csvName = "KIC 11442793";
    else if (explName == "OGLE 2006-BLG-109L")
        csvName = "OGLE-2006-BLG-109L";
    else if (explName == "HD 137388 A")
        csvName = "HD 137388";
    else if (explName == "kap CrB")
        csvName = "kappa CrB";
    else if (explName == "XO-2 N")
        csvName = "XO-2";
    else if (explName == "eps Tau")
        csvName = "epsilon Tau";
    else if (explName == "eps Eri")
        csvName = "epsilon Eri";
    else if (explName == "KOI-12")
        csvName = "Kepler-448";
    else if (explName == "ome Ser")
        csvName = "omega Ser";
    else if (explName == "MOA 2010-BLG-477L")
        csvName = "MOA-2010-BLG-477L";
    else if (explName == "HD 285968")
        csvName = "GJ 176";
    else if (explName == "BD-17 63")
        csvName = "HIP 2247";
    else if (explName == "MOA 2009-BLG-266L")
        csvName = "MOA-2009-BLG-266L";
    else if (explName == "KOI-94")
        csvName = "Kepler-89";
    else if (explName == "HIP 75458")
        csvName = "iota Dra";
    else if (explName == "MOA 2007-BLG-400L")
        csvName = "MOA-2007-BLG-400L";
    else if (explName == "ups And")
        csvName = "upsilon And";
    else if (explName == "OGLE 2011-BLG-251L")
        csvName = "OGLE-2011-BLG-0251";
    else if (explName == "OGLE 2005-BLG-390L")
        csvName = "OGLE-05-390L";
    else if (explName == "KOI-1257")
        csvName = "Kepler-420";
    else if (explName == "bet Pic")
        csvName = "beta Pic";
    else if (explName == "gam Cep")
        csvName = "gamma Cep";
    else if (explName == "MOA 2007-BLG-192L")
        csvName = "MOA-2007-BLG-192L";
    else if (explName == "MOA 2009-BLG-319L")
        csvName = "MOA-2009-BLG-319L";
    else if (explName == "omi CrB")
        csvName = "omicron CrB";
    else if (explName == "HD 62509")
        csvName = "beta Gem";
    else if (explName == "eps CrB")
        csvName = "epsilon CrB";
    else if (explName == "omi UMa")
        csvName = "omicron UMa";
    else if (explName == "HD 142022 A")
        csvName = "HD 142022";

    return csvName;
}

} // namespace openspace::exoplanets
