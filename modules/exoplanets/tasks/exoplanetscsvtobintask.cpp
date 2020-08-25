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

#include <modules/exoplanets/tasks/exoplanetscsvtobintask.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <fstream>

namespace {
    const char* KeyInputCSV = "InputCSV";
    const char* KeyInputSPECK = "InputSPECK";
    const char* KeyOutputBIN = "OutputBIN";
    const char* KeyOutputLUT = "OutputLUT";

    constexpr const char* _loggerCat = "CsvToBinTask";
} // namespace

namespace openspace {
namespace exoplanets {

ExoplanetsCsvToBinTask::ExoplanetsCsvToBinTask(const ghoul::Dictionary& dictionary) {
  openspace::documentation::testSpecificationAndThrow(
      documentation(),
      dictionary,
      "ExoplanetsCsvToBinTask"
  );

  _inputCSVPath = absPath(dictionary.value<std::string>(KeyInputCSV));
  _inputSPECKPath = absPath(dictionary.value<std::string>(KeyInputSPECK));
  _outputBINPath = absPath(dictionary.value<std::string>(KeyOutputBIN));
  _outputLUTPath = absPath(dictionary.value<std::string>(KeyOutputLUT));
}

std::string ExoplanetsCsvToBinTask::description() {
    return "Extract metadata from csv-file " + _inputCSVPath +
        " and write as bin to " + _outputBINPath;
}

std::string ExoplanetsCsvToBinTask::getExplName(std::string csvName) {
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

glm::vec3 ExoplanetsCsvToBinTask::getStarPosition(std::string starName)
{
    glm::vec3 pos;
    pos[0] = NAN;
    pos[1] = NAN;
    pos[2] = NAN;
    std::ifstream expl_file(_inputSPECKPath);
    if (!expl_file) {
        LERROR(fmt::format("Error opening file expl.speck."));
    }

    std::string line;
    std::string d;
    std::string n;
    while (getline(expl_file, line))
    {
        if (line[0] == '#' || line.substr(0, 7) == "datavar" || line.substr(0, 10) == "texturevar" || line.substr(0, 7) == "texture" || line.empty()) {
            continue;
        }

        std::istringstream linestream(line);

        getline(linestream, d, '#');
        getline(linestream, n);
        n.erase(0, 1);

        std::string coord;
        if (n.compare(starName) == 0)
        {
            std::stringstream datastream(d);
            getline(datastream, coord, ' ');
            pos[0] = std::stof(coord.c_str(), nullptr);
            getline(datastream, coord, ' ');
            pos[1] = std::stof(coord.c_str(), nullptr);
            getline(datastream, coord, ' ');
            pos[2] = std::stof(coord.c_str(), nullptr);
            break;
        }
    }

    //Apply transformation matrix to pos
    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);
    glm::vec3 transformedPos = glm::vec3(_transformationMatrix * glm::dvec4(pos, 1.0));

    expl_file.close();
    return transformedPos;
}

void ExoplanetsCsvToBinTask::perform(const Task::ProgressCallback& progressCallback) {
    std::ifstream csv_file(_inputCSVPath);
    if (!csv_file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _inputCSVPath));
        return;
    }

    std::ofstream bin_file(_outputBINPath, std::ios::out | std::ios::binary);
    std::ofstream lut_file(_outputLUTPath);

    int version = 1;
    bin_file.write((char *)&version, sizeof(int));

    Exoplanet p;

    std::string planetname;
    std::string component;
    std::string planet_row;
    getline(csv_file, planet_row); // The first line, containing the data names

    std::string data_s;
    bool iskeplerobject = false;
    int total = 0;
    while (getline(csv_file, planet_row)) {
        ++total;
    }
    csv_file.clear();
    csv_file.seekg(0);
    getline(csv_file, planet_row); // The first line, containing the data names
    LINFOC("CSVTOBIN", fmt::format("Loading {} stars", total));

    int count = 0;
    while (getline(csv_file, planet_row)) {
        ++count;
        progressCallback(static_cast<float>(count) / static_cast<float>(total));

        std::istringstream lineStream(planet_row);

        getline(lineStream, data_s, ','); // A
        if (!data_s.empty())
            p.A = std::stof(data_s.c_str(), nullptr);
        else
            p.A = NAN;
        getline(lineStream, data_s, ','); // AUPPER
        if (!data_s.empty())
            p.AUPPER = std::stod(data_s.c_str(), nullptr);
        else
            p.AUPPER = NAN;
        getline(lineStream, data_s, ','); // ALOWER
        if (!data_s.empty())
            p.ALOWER = std::stod(data_s.c_str(), nullptr);
        else
            p.ALOWER = NAN;
        getline(lineStream, data_s, ','); // UA
        if (!data_s.empty())
            p.UA = std::stod(data_s.c_str(), nullptr);
        else
            p.UA = NAN;
        getline(lineStream, data_s, ','); // AREF
        getline(lineStream, data_s, ','); // AURL
        getline(lineStream, data_s, ','); // AR
        getline(lineStream, data_s, ','); // ARUPPER
        getline(lineStream, data_s, ','); // ARLOWER
        getline(lineStream, data_s, ','); // UAR
        getline(lineStream, data_s, ','); // ARREF
        getline(lineStream, data_s, ','); // ARURL
        getline(lineStream, data_s, ','); // ASTROMETRY
        getline(lineStream, data_s, ','); // B
        getline(lineStream, data_s, ','); // BUPPER
        getline(lineStream, data_s, ','); // BLOWER
        getline(lineStream, data_s, ','); // UB
        getline(lineStream, data_s, ','); // BREF
        getline(lineStream, data_s, ','); // BURL
        getline(lineStream, data_s, ','); // BIGOM
        if (!data_s.empty())
            p.BIGOM = std::stof(data_s.c_str(), nullptr);
        else
            p.BIGOM = NAN;
        getline(lineStream, data_s, ','); // BIGOMUPPER
        if (!data_s.empty())
            p.BIGOMUPPER = std::stof(data_s.c_str(), nullptr);
        else
            p.BIGOMUPPER = NAN;
        getline(lineStream, data_s, ','); // BIGOMLOWER
        if (!data_s.empty())
            p.BIGOMLOWER = std::stof(data_s.c_str(), nullptr);
        else
            p.BIGOMLOWER = NAN;
        getline(lineStream, data_s, ','); // UBIGOM
        if (!data_s.empty())
            p.UBIGOM = std::stof(data_s.c_str(), nullptr);
        else
            p.UBIGOM = NAN;
        getline(lineStream, data_s, ','); // BIGOMREF
        getline(lineStream, data_s, ','); // BIGOMURL
        getline(lineStream, data_s, ','); // BINARY
        if (!data_s.empty())
            p.BINARY = std::stoi(data_s.c_str(), nullptr);
        else
            p.BINARY = -1;
        getline(lineStream, data_s, ','); // BINARYREF
        getline(lineStream, data_s, ','); // BINARYURL
        getline(lineStream, data_s, ','); // BMV
        if (!data_s.empty())
            p.BMV = std::stof(data_s.c_str(), nullptr);
        else
            p.BMV = NAN;
        getline(lineStream, data_s, ','); // CHI2
        getline(lineStream, data_s, ','); // COMP
        component = data_s;
        getline(lineStream, data_s, ','); // DATE
        getline(lineStream, data_s, ','); // DEC
        getline(lineStream, data_s, ','); // DEC_STRING
        getline(lineStream, data_s, ','); // DENSITY
        getline(lineStream, data_s, ','); // DENSITYUPPER
        getline(lineStream, data_s, ','); // DENSITYLOWER
        getline(lineStream, data_s, ','); // UDENSITY
        getline(lineStream, data_s, ','); // DENSITYREF
        getline(lineStream, data_s, ','); // DENSITYURL
        getline(lineStream, data_s, ','); // DEPTH
        getline(lineStream, data_s, ','); // DEPTHUPPER
        getline(lineStream, data_s, ','); // DEPTHLOWER
        getline(lineStream, data_s, ','); // UDEPTH
        getline(lineStream, data_s, ','); // DEPTHREF
        getline(lineStream, data_s, ','); // DEPTHURL
        getline(lineStream, data_s, ','); // DIST
        getline(lineStream, data_s, ','); // DISTUPPER
        getline(lineStream, data_s, ','); // DISTLOWER
        getline(lineStream, data_s, ','); // UDIST
        getline(lineStream, data_s, ','); // DISTREF
        getline(lineStream, data_s, ','); // DISTURL
        getline(lineStream, data_s, ','); // DR
        getline(lineStream, data_s, ','); // DRUPPER
        getline(lineStream, data_s, ','); // DRLOWER
        getline(lineStream, data_s, ','); // UDR
        getline(lineStream, data_s, ','); // DRREF
        getline(lineStream, data_s, ','); // DRURL
        getline(lineStream, data_s, ','); // DVDT
        getline(lineStream, data_s, ','); // DVDTUPPER
        getline(lineStream, data_s, ','); // DVDTLOWER
        getline(lineStream, data_s, ','); // UDVDT
        getline(lineStream, data_s, ','); // DVDTREF
        getline(lineStream, data_s, ','); // DVDTURL
        getline(lineStream, data_s, ','); // EANAME
        getline(lineStream, data_s, ','); // EAURL
        getline(lineStream, data_s, ','); // ECC
        if (!data_s.empty())
            p.ECC = std::stof(data_s.c_str(), nullptr);
        else
            p.ECC = NAN;

        getline(lineStream, data_s, ','); // ECCUPPER
        if (!data_s.empty())
            p.ECCUPPER = std::stof(data_s.c_str(), nullptr);
        else
            p.ECCUPPER = NAN;

        getline(lineStream, data_s, ','); // ECCLOWER
        if (!data_s.empty())
            p.ECCLOWER = std::stof(data_s.c_str(), nullptr);
        else
            p.ECCLOWER = NAN;

        getline(lineStream, data_s, ','); // UECC
        if (!data_s.empty())
            p.UECC = std::stof(data_s.c_str(), nullptr);
        else
            p.UECC = NAN;

        getline(lineStream, data_s, ','); // ECCREF
        getline(lineStream, data_s, ','); // ECCURL
        getline(lineStream, data_s, ','); // EOD
        getline(lineStream, data_s, ','); // ETDNAME
        getline(lineStream, data_s, ','); // ETDURL
        getline(lineStream, data_s, ','); // FE
        getline(lineStream, data_s, ','); // FEUPPER
        getline(lineStream, data_s, ','); // FELOWER
        getline(lineStream, data_s, ','); // UFE
        getline(lineStream, data_s, ','); // FEREF
        getline(lineStream, data_s, ','); // FEURL
        getline(lineStream, data_s, ','); // FIRSTREF
        getline(lineStream, data_s, ','); // FIRSTURL
        getline(lineStream, data_s, ','); // FREEZE_ECC
        getline(lineStream, data_s, ','); // GAMMA
        getline(lineStream, data_s, ','); // GAMMAUPPER
        getline(lineStream, data_s, ','); // GAMMALOWER
        getline(lineStream, data_s, ','); // UGAMMA
        getline(lineStream, data_s, ','); // GAMMAREF
        getline(lineStream, data_s, ','); // GAMMAURL
        getline(lineStream, data_s, ','); // GL
        getline(lineStream, data_s, ','); // GRAVITY
        getline(lineStream, data_s, ','); // GRAVITYUPPER
        getline(lineStream, data_s, ','); // GRAVITYLOWER
        getline(lineStream, data_s, ','); // UGRAVITY
        getline(lineStream, data_s, ','); // GRAVITYREF
        getline(lineStream, data_s, ','); // GRAVITYURL
        getline(lineStream, data_s, ','); // H
        getline(lineStream, data_s, ','); // HD
        getline(lineStream, data_s, ','); // HIPP
        getline(lineStream, data_s, ','); // HR
        getline(lineStream, data_s, ','); // I
        if (!data_s.empty())
            p.I = std::stof(data_s.c_str(), nullptr);
        else
            p.I = NAN;

        getline(lineStream, data_s, ','); // IUPPER
        if (!data_s.empty())
            p.IUPPER = std::stof(data_s.c_str(), nullptr);
        else
            p.IUPPER = NAN;

        getline(lineStream, data_s, ','); // ILOWER
        if (!data_s.empty())
            p.ILOWER = std::stof(data_s.c_str(), nullptr);
        else
            p.ILOWER = NAN;

        getline(lineStream, data_s, ','); // UI
        if (!data_s.empty())
            p.UI = std::stof(data_s.c_str(), nullptr);
        else
            p.UI = NAN;

        getline(lineStream, data_s, ','); // IREF
        getline(lineStream, data_s, ','); // IURL
        getline(lineStream, data_s, ','); // IMAGING
        getline(lineStream, data_s, ','); // J
        getline(lineStream, data_s, ','); // JSNAME
        getline(lineStream, data_s, ','); // EPEURL
        getline(lineStream, data_s, ','); // K
        getline(lineStream, data_s, ','); // KUPPER
        getline(lineStream, data_s, ','); // KLOWER
        getline(lineStream, data_s, ','); // UK
        getline(lineStream, data_s, ','); // KREF
        getline(lineStream, data_s, ','); // KURL
        getline(lineStream, data_s, ','); // KOI
        getline(lineStream, data_s, ','); // KS
        getline(lineStream, data_s, ','); // KP
        getline(lineStream, data_s, ','); // LAMBDA
        getline(lineStream, data_s, ','); // LAMBDAUPPER
        getline(lineStream, data_s, ','); // LAMBDALOWER
        getline(lineStream, data_s, ','); // ULAMBDA
        getline(lineStream, data_s, ','); // LAMBDAREF
        getline(lineStream, data_s, ','); // LAMBDAURL
        getline(lineStream, data_s, ','); // LOGG
        getline(lineStream, data_s, ','); // LOGGUPPER
        getline(lineStream, data_s, ','); // LOGGLOWER
        getline(lineStream, data_s, ','); // ULOGG
        getline(lineStream, data_s, ','); // LOGGREF
        getline(lineStream, data_s, ','); // LOGGURL;
        getline(lineStream, data_s, ','); // MASS
        getline(lineStream, data_s, ','); // MASSUPPER
        getline(lineStream, data_s, ','); // MASSLOWER
        getline(lineStream, data_s, ','); // UMASS
        getline(lineStream, data_s, ','); // MASSREF
        getline(lineStream, data_s, ','); // MASSURL
        getline(lineStream, data_s, ','); // MICROLENSING
        getline(lineStream, data_s, ','); // MSINI
        getline(lineStream, data_s, ','); // MSINIUPPER
        getline(lineStream, data_s, ','); // MSINILOWER
        getline(lineStream, data_s, ','); // UMSINI
        getline(lineStream, data_s, ','); // MSINIREF
        getline(lineStream, data_s, ','); // MSINIURL
        getline(lineStream, data_s, ','); // MSTAR
        getline(lineStream, data_s, ','); // MSTARUPPER
        getline(lineStream, data_s, ','); // MSTARLOWER
        getline(lineStream, data_s, ','); // UMSTAR
        getline(lineStream, data_s, ','); // MSTARREF
        getline(lineStream, data_s, ','); // MSTARURL
        getline(lineStream, data_s, ','); // MULT
        getline(lineStream, data_s, ','); // NAME
        getline(lineStream, data_s, ','); // NCOMP
        if (!data_s.empty())
            p.NCOMP = std::stoi(data_s.c_str(), nullptr);
        else
            p.NCOMP = -1;
        getline(lineStream, data_s, ','); // NOBS
        getline(lineStream, data_s, ','); // OM
        if (!data_s.empty())
            p.OM = std::stof(data_s.c_str(), nullptr);
        else
            p.OM = NAN;

        getline(lineStream, data_s, ','); // OMUPPER
        if (!data_s.empty())
            p.OMUPPER = std::stof(data_s.c_str(), nullptr);
        else
            p.OMUPPER = NAN;

        getline(lineStream, data_s, ','); // OMLOWER
        if (!data_s.empty())
            p.OMLOWER = std::stof(data_s.c_str(), nullptr);
        else
            p.OMLOWER = NAN;

        getline(lineStream, data_s, ','); // UOM
        if (!data_s.empty())
            p.UOM = std::stof(data_s.c_str(), nullptr);
        else
            p.UOM = NAN;

        getline(lineStream, data_s, ','); // OMREF
        getline(lineStream, data_s, ','); // OMURL
        getline(lineStream, data_s, ','); // ORBREF
        getline(lineStream, data_s, ','); // ORBURL
        getline(lineStream, data_s, ','); // OTHERNAME
        getline(lineStream, data_s, ','); // PAR
        getline(lineStream, data_s, ','); // PARUPPER
        getline(lineStream, data_s, ','); // PARLOWER
        getline(lineStream, data_s, ','); // UPAR
        getline(lineStream, data_s, ','); // PER
        if (!data_s.empty())
            p.PER = std::stod(data_s.c_str(), nullptr);
        else
            p.PER = NAN;

        getline(lineStream, data_s, ','); // PERUPPER
        if (!data_s.empty())
            p.PERUPPER = std::stof(data_s.c_str(), nullptr);
        else
            p.PERUPPER = NAN;

        getline(lineStream, data_s, ','); // PERLOWER
        if (!data_s.empty())
            p.PERLOWER = std::stof(data_s.c_str(), nullptr);
        else
            p.PERLOWER = NAN;

        getline(lineStream, data_s, ','); // UPER
        if (!data_s.empty())
            p.UPER = std::stof(data_s.c_str(), nullptr);
        else
            p.UPER = NAN;

        getline(lineStream, data_s, ','); // PERREF
        getline(lineStream, data_s, ','); // PERURL
        getline(lineStream, data_s, ','); // PLANETDISCMETH
        getline(lineStream, data_s, ','); // R
        if (!data_s.empty())
            p.R = std::stod(data_s.c_str(), nullptr);
        else
            p.R = NAN;

        getline(lineStream, data_s, ','); // RUPPER
        if (!data_s.empty())
            p.RUPPER = std::stod(data_s.c_str(), nullptr);
        else
            p.RUPPER = NAN;

        getline(lineStream, data_s, ','); // RLOWER
        if (!data_s.empty())
            p.RLOWER = std::stod(data_s.c_str(), nullptr);
        else
            p.RLOWER = NAN;

        getline(lineStream, data_s, ','); //UR
        if (!data_s.empty())
            p.UR = std::stod(data_s.c_str(), nullptr);
        else
            p.UR = NAN;

        getline(lineStream, data_s, ','); // RREF
        getline(lineStream, data_s, ','); // RURL
        getline(lineStream, data_s, ','); // RA
        getline(lineStream, data_s, ','); // RA_STRING
        getline(lineStream, data_s, ','); // RHK
        getline(lineStream, data_s, ','); // RHOSTAR
        getline(lineStream, data_s, ','); // RHOSTARUPPER
        getline(lineStream, data_s, ','); // RHOSTARLOWER
        getline(lineStream, data_s, ','); // URHOSTAR
        getline(lineStream, data_s, ','); // RHOSTARREF
        getline(lineStream, data_s, ','); // RHOSTARURL
        getline(lineStream, data_s, ','); // RMS
        getline(lineStream, data_s, ','); // RR
        getline(lineStream, data_s, ','); // RRUPPER
        getline(lineStream, data_s, ','); // RRLOWER
        getline(lineStream, data_s, ','); // URR
        getline(lineStream, data_s, ','); // RRREF
        getline(lineStream, data_s, ','); // RRURL
        getline(lineStream, data_s, ','); // RSTAR
        if (!data_s.empty())
            p.RSTAR = std::stof(data_s.c_str(), nullptr);
        else
            p.RSTAR = NAN;

        getline(lineStream, data_s, ','); // RSTARUPPER
        if (!data_s.empty())
            p.RSTARUPPER = std::stof(data_s.c_str(), nullptr);
        else
            p.RSTARUPPER = NAN;

        getline(lineStream, data_s, ','); // RSTARLOWER
        if (!data_s.empty())
            p.RSTARLOWER = std::stof(data_s.c_str(), nullptr);
        else
            p.RSTARLOWER = NAN;

        getline(lineStream, data_s, ','); // URSTAR
        if (!data_s.empty())
            p.URSTAR = std::stof(data_s.c_str(), nullptr);
        else
            p.URSTAR = NAN;

        getline(lineStream, data_s, ','); // RSTARREF
        getline(lineStream, data_s, ','); // RSTARURL
        getline(lineStream, data_s, ','); // SAO
        getline(lineStream, data_s, ','); // SE
        getline(lineStream, data_s, ','); // SEREF
        getline(lineStream, data_s, ','); // SEURL
        getline(lineStream, data_s, ','); // SEDEPTHJ
        getline(lineStream, data_s, ','); // SEDEPTHJUPPER
        getline(lineStream, data_s, ','); // SEDEPTHJLOWER
        getline(lineStream, data_s, ','); // USEDEPTHJ
        getline(lineStream, data_s, ','); // SEDEPTHJREF
        getline(lineStream, data_s, ','); // SEDEPTHJURL
        getline(lineStream, data_s, ','); // SEDEPTHH
        getline(lineStream, data_s, ','); // SEDEPTHHUPPER
        getline(lineStream, data_s, ','); // SEDEPTHHLOWER
        getline(lineStream, data_s, ','); // USEDEPTHH
        getline(lineStream, data_s, ','); // SEDEPTHHREF
        getline(lineStream, data_s, ','); // SEDEPTHHURL
        getline(lineStream, data_s, ','); // SEDEPTHKS
        getline(lineStream, data_s, ','); // SEDEPTHKSUPPER
        getline(lineStream, data_s, ','); // SEDEPTHKSLOWER
        getline(lineStream, data_s, ','); // USEDEPTHKS
        getline(lineStream, data_s, ','); // SEDEPTHKSREF
        getline(lineStream, data_s, ','); // SEDEPTHKSURL
        getline(lineStream, data_s, ','); // SEDEPTHKP
        getline(lineStream, data_s, ','); // SEDEPTHKPUPPER
        getline(lineStream, data_s, ','); // SEDEPTHKPLOWER
        getline(lineStream, data_s, ','); // USEDEPTHKP
        getline(lineStream, data_s, ','); // SEDEPTHKPREF
        getline(lineStream, data_s, ','); // SEDEPTHKPURL
        getline(lineStream, data_s, ','); // SEDEPTH36
        getline(lineStream, data_s, ','); // SEDEPTH36UPPER
        getline(lineStream, data_s, ','); // SEDEPTH36LOWER
        getline(lineStream, data_s, ','); // USEDEPTH36
        getline(lineStream, data_s, ','); // SEDEPTH36REFx
        getline(lineStream, data_s, ','); // SEDEPTH36URLx
        getline(lineStream, data_s, ','); // SEDEPTH45
        getline(lineStream, data_s, ','); // SEDEPTH45UPPER
        getline(lineStream, data_s, ','); // SEDEPTH45LOWER
        getline(lineStream, data_s, ','); // USEDEPTH45
        getline(lineStream, data_s, ','); // SEDEPTH45REF
        getline(lineStream, data_s, ','); // SEDEPTH45URL
        getline(lineStream, data_s, ','); // SEDEPTH58
        getline(lineStream, data_s, ','); // SEDEPTH58UPPER
        getline(lineStream, data_s, ','); // SEDEPTH58LOWER
        getline(lineStream, data_s, ','); // USEDEPTH58
        getline(lineStream, data_s, ','); // EDEPTH58REF
        getline(lineStream, data_s, ','); // SEDEPTH58URL
        getline(lineStream, data_s, ','); // SEDEPTH80
        getline(lineStream, data_s, ','); // SEDEPTH80UPPER
        getline(lineStream, data_s, ','); // SEDEPTH80LOWER
        getline(lineStream, data_s, ','); // USEDEPTH80
        getline(lineStream, data_s, ','); // SEDEPTH80REF
        getline(lineStream, data_s, ','); // SEDEPTH80URL
        getline(lineStream, data_s, ','); // SEP
        getline(lineStream, data_s, ','); // SEPUPPER
        getline(lineStream, data_s, ','); // SEPLOWER
        getline(lineStream, data_s, ','); // USEP
        getline(lineStream, data_s, ','); // SEPREF
        getline(lineStream, data_s, ','); // SEPURL
        getline(lineStream, data_s, ','); // SET
        getline(lineStream, data_s, ','); // SETUPPER
        getline(lineStream, data_s, ','); // SETLOWER
        getline(lineStream, data_s, ','); // USET
        getline(lineStream, data_s, ','); // SETREF
        getline(lineStream, data_s, ','); // SETURL
        getline(lineStream, data_s, ','); // SHK
        getline(lineStream, data_s, ','); // SIMBADNAME
        getline(lineStream, data_s, ','); // SIMBADURL
        getline(lineStream, data_s, ','); // SPECREF
        getline(lineStream, data_s, ','); // SPECURL
        getline(lineStream, data_s, ','); // STAR
        std::string  speckStarname = getExplName(data_s);
        glm::vec3 pos = getStarPosition(speckStarname);
        p.POSITIONX = pos[0];
        p.POSITIONY = pos[1];
        p.POSITIONZ = pos[2];

        getline(lineStream, data_s, ','); // STARDISCMETH
        getline(lineStream, data_s, ','); // T0
        getline(lineStream, data_s, ','); // T0UPPER
        getline(lineStream, data_s, ','); // T0LOWER
        getline(lineStream, data_s, ','); // UT0
        getline(lineStream, data_s, ','); // T0REF
        getline(lineStream, data_s, ','); // T0URL
        getline(lineStream, data_s, ','); // T14
        getline(lineStream, data_s, ','); // T14UPPER
        getline(lineStream, data_s, ','); // T14LOWER
        getline(lineStream, data_s, ','); // UT14
        getline(lineStream, data_s, ','); // T14REF
        getline(lineStream, data_s, ','); // T14URL
        getline(lineStream, data_s, ','); // TEFF
        float teff;
        if (!data_s.empty())
            teff = std::stof(data_s.c_str(), nullptr);
        else
            teff = NAN;

        getline(lineStream, data_s, ','); // TEFFUPPER
        getline(lineStream, data_s, ','); // TEFFLOWER
        getline(lineStream, data_s, ','); // UTEFF
        getline(lineStream, data_s, ','); // TEFFREF
        getline(lineStream, data_s, ','); // TEFFURL
        getline(lineStream, data_s, ','); // TIMING
        getline(lineStream, data_s, ','); // TRANSIT
        getline(lineStream, data_s, ','); // TRANSITREF
        getline(lineStream, data_s, ','); // TRANSITURL
        getline(lineStream, data_s, ','); // TREND
        getline(lineStream, data_s, ','); // TT
        if (!data_s.empty())
            p.TT = std::stod(data_s.c_str(), nullptr);
        else
            p.TT = NAN;

        getline(lineStream, data_s, ','); // TTUPPER
        if (!data_s.empty())
            p.TTUPPER = std::stof(data_s.c_str(), nullptr);
        else
            p.TTUPPER = NAN;

        getline(lineStream, data_s, ','); // TTLOWER
        if (!data_s.empty())
            p.TTLOWER = std::stof(data_s.c_str(), nullptr);
        else
            p.TTLOWER = NAN;

        getline(lineStream, data_s, ','); // UTT
        if (!data_s.empty())
            p.UTT = std::stof(data_s.c_str(), nullptr);
        else
            p.UTT = NAN;

        getline(lineStream, data_s, ','); // TTREF
        getline(lineStream, data_s, ','); // TTURL
        getline(lineStream, data_s, ','); // V
        getline(lineStream, data_s, ','); // VREF
        getline(lineStream, data_s, ','); // VURL
        getline(lineStream, data_s, ','); // VSINI
        getline(lineStream, data_s, ','); // VSINIUPPER
        getline(lineStream, data_s, ','); // VSINILOWER
        getline(lineStream, data_s, ','); // UVSINI
        getline(lineStream, data_s, ','); // VSINIREF
        getline(lineStream, data_s, ','); // VSINIURL
        getline(lineStream, data_s, ','); // KEPID
        if (!data_s.empty())
            iskeplerobject = true;
        getline(lineStream, data_s); // KDE

        if (!iskeplerobject) {
            // calculate B-V from Teff if not exsisting
            if (std::isnan(p.BMV)) {
                if (!std::isnan(teff)) {
                    float teff_current, teff_upper, teff_lower, BV, bv_upper, bv_lower = 0;

                    std::ifstream teff_bv(absPath("${BASE}/modules/exoplanets/teff_bv.txt"));
                    if (!teff_bv.good()) {
                        LERROR(fmt::format("Failed to open teff_bv.txt file"));
                        return;
                    }

                    std::string row, teff_string, bv_string;
                    while (getline(teff_bv, row)) {
                        std::istringstream lineStream(row);
                        getline(lineStream, teff_string, ',');
                        getline(lineStream, bv_string);

                        teff_current= std::stof(teff_string.c_str(), nullptr);
                        
                        if (teff > teff_current) {
                            teff_lower = teff_current;
                            bv_lower = std::stof(bv_string.c_str(), nullptr);
                        }
                        else {
                            teff_upper = teff_current;
                            bv_upper = std::stof(bv_string.c_str(), nullptr);
                            if (bv_lower == 0) {
                                BV = 2.00;
                            }
                            else {
                                BV = (((bv_upper - bv_lower)*(teff - teff_lower)) / (teff_upper - teff_lower)) + bv_lower;
                            }
                            break;
                        }
                    }

                    teff_bv.close();
                    p.BMV = BV;
                }
                else {
                    p.BMV = NAN;
                }
            }

            long pos = bin_file.tellp();
            planetname = speckStarname + " " + component;
            lut_file << planetname << "," << pos << std::endl;
            bin_file.write((char *)&p, sizeof(struct Exoplanet));
        }
    }

    csv_file.close();
    bin_file.close();
    lut_file.close();

    progressCallback(1.0f);
}

documentation::Documentation ExoplanetsCsvToBinTask::documentation() {
    using namespace documentation;
    return {
        "ExoplanetsCsvToBinTask",
        "exoplanets_csv_to_bin_task",
        {
            {
                "Type",
                new StringEqualVerifier("ExoplanetsCsvToBinTask"),
                Optional::No,
                "The type of this task"
            },
            {
                KeyInputCSV,
                new StringAnnotationVerifier("A file path to a csv file"),
                Optional::No,
                "The csv file to extract data from"
            },
            {
                KeyInputSPECK,
                new StringAnnotationVerifier("A file path to a speck file"),
                Optional::No,
                "The speck file to with star location"
            },
            {
                KeyOutputBIN,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The bin file to export data into"
            },
            {
                KeyOutputLUT,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The txt file to write look-up table into"
            }
        }
    };
}

} // namespace exoplanets
} // namespace openspace
