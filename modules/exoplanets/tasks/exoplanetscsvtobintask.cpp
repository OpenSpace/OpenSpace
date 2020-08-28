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

#include <modules/exoplanets/exoplanetshelper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <fstream>

namespace {
    const char* KeyInputCsv = "InputCSV";
    const char* KeyInputSpeck = "InputSPECK";
    const char* KeyOutputBin = "OutputBIN";
    const char* KeyOutputLut = "OutputLUT";

    constexpr const char* _loggerCat = "CsvToBinTask";
} // namespace

namespace openspace::exoplanets {

ExoplanetsCsvToBinTask::ExoplanetsCsvToBinTask(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ExoplanetsCsvToBinTask"
    );

    _inputCsvPath = absPath(dictionary.value<std::string>(KeyInputCsv));
    _inputSpeckPath = absPath(dictionary.value<std::string>(KeyInputSpeck));
    _outputBinPath = absPath(dictionary.value<std::string>(KeyOutputBin));
    _outputLutPath = absPath(dictionary.value<std::string>(KeyOutputLut));
}

std::string ExoplanetsCsvToBinTask::description() {
    return "Extract metadata from csv-file " + _inputCsvPath +
        " and write as bin to " + _outputBinPath;
}

glm::vec3 ExoplanetsCsvToBinTask::getStarPosition(std::string starName) {
    glm::vec3 position;
    position[0] = NAN;
    position[1] = NAN;
    position[2] = NAN;
    std::ifstream exoplanetsFile(_inputSpeckPath);
    if (!exoplanetsFile) {
        LERROR(fmt::format("Error opening file expl.speck."));
    }

    std::string line;
    std::string d;
    std::string n;
    while (getline(exoplanetsFile, line))
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
            std::stringstream dataStream(d);
            getline(dataStream, coord, ' ');
            position[0] = std::stof(coord.c_str(), nullptr);
            getline(dataStream, coord, ' ');
            position[1] = std::stof(coord.c_str(), nullptr);
            getline(dataStream, coord, ' ');
            position[2] = std::stof(coord.c_str(), nullptr);
            break;
        }
    }

    // Apply transformation matrix to pos
    glm::dmat4 _transformationMatrix = glm::dmat4(1.0);
    glm::vec3 transformedPosition = glm::vec3(
        _transformationMatrix * glm::dvec4(position, 1.0)
    );

    exoplanetsFile.close();
    return transformedPosition;
}

void ExoplanetsCsvToBinTask::perform(const Task::ProgressCallback& progressCallback) {
    std::ifstream csvFile(_inputCsvPath);
    if (!csvFile.good()) {
        LERROR(fmt::format("Failed to open CSV file '{}'", _inputCsvPath));
        return;
    }

    std::ofstream binFile(_outputBinPath, std::ios::out | std::ios::binary);
    std::ofstream lutFile(_outputLutPath);

    int version = 1;
    binFile.write((char *)&version, sizeof(int));

    Exoplanet p;

    std::string planetName;
    std::string component;
    std::string planetRow;
    getline(csvFile, planetRow); // The first line, containing the data names

    bool isKeplerObject = false;
    int total = 0;
    while (getline(csvFile, planetRow)) {
        ++total;
    }
    csvFile.clear();
    csvFile.seekg(0);
    getline(csvFile, planetRow); // The first line, containing the data names
    LINFOC("CSVTOBIN", fmt::format("Loading {} stars", total));

    int count = 0;
    std::string data;
    while (getline(csvFile, planetRow)) {
        ++count;
        progressCallback(static_cast<float>(count) / static_cast<float>(total));

        std::istringstream lineStream(planetRow);

        getline(lineStream, data, ','); // A
        if (!data.empty())
            p.A = std::stof(data.c_str(), nullptr);
        else
            p.A = NAN;
        getline(lineStream, data, ','); // AUPPER
        if (!data.empty())
            p.AUPPER = std::stod(data.c_str(), nullptr);
        else
            p.AUPPER = NAN;
        getline(lineStream, data, ','); // ALOWER
        if (!data.empty())
            p.ALOWER = std::stod(data.c_str(), nullptr);
        else
            p.ALOWER = NAN;
        getline(lineStream, data, ','); // UA
        if (!data.empty())
            p.UA = std::stod(data.c_str(), nullptr);
        else
            p.UA = NAN;
        getline(lineStream, data, ','); // AREF
        getline(lineStream, data, ','); // AURL
        getline(lineStream, data, ','); // AR
        getline(lineStream, data, ','); // ARUPPER
        getline(lineStream, data, ','); // ARLOWER
        getline(lineStream, data, ','); // UAR
        getline(lineStream, data, ','); // ARREF
        getline(lineStream, data, ','); // ARURL
        getline(lineStream, data, ','); // ASTROMETRY
        getline(lineStream, data, ','); // B
        getline(lineStream, data, ','); // BUPPER
        getline(lineStream, data, ','); // BLOWER
        getline(lineStream, data, ','); // UB
        getline(lineStream, data, ','); // BREF
        getline(lineStream, data, ','); // BURL
        getline(lineStream, data, ','); // BIGOM
        if (!data.empty())
            p.BIGOM = std::stof(data.c_str(), nullptr);
        else
            p.BIGOM = NAN;
        getline(lineStream, data, ','); // BIGOMUPPER
        if (!data.empty())
            p.BIGOMUPPER = std::stof(data.c_str(), nullptr);
        else
            p.BIGOMUPPER = NAN;
        getline(lineStream, data, ','); // BIGOMLOWER
        if (!data.empty())
            p.BIGOMLOWER = std::stof(data.c_str(), nullptr);
        else
            p.BIGOMLOWER = NAN;
        getline(lineStream, data, ','); // UBIGOM
        if (!data.empty())
            p.UBIGOM = std::stof(data.c_str(), nullptr);
        else
            p.UBIGOM = NAN;
        getline(lineStream, data, ','); // BIGOMREF
        getline(lineStream, data, ','); // BIGOMURL
        getline(lineStream, data, ','); // BINARY
        if (!data.empty())
            p.BINARY = std::stoi(data.c_str(), nullptr);
        else
            p.BINARY = -1;
        getline(lineStream, data, ','); // BINARYREF
        getline(lineStream, data, ','); // BINARYURL
        getline(lineStream, data, ','); // BMV
        if (!data.empty())
            p.BMV = std::stof(data.c_str(), nullptr);
        else
            p.BMV = NAN;
        getline(lineStream, data, ','); // CHI2
        getline(lineStream, data, ','); // COMP
        component = data;
        getline(lineStream, data, ','); // DATE
        getline(lineStream, data, ','); // DEC
        getline(lineStream, data, ','); // DEC_STRING
        getline(lineStream, data, ','); // DENSITY
        getline(lineStream, data, ','); // DENSITYUPPER
        getline(lineStream, data, ','); // DENSITYLOWER
        getline(lineStream, data, ','); // UDENSITY
        getline(lineStream, data, ','); // DENSITYREF
        getline(lineStream, data, ','); // DENSITYURL
        getline(lineStream, data, ','); // DEPTH
        getline(lineStream, data, ','); // DEPTHUPPER
        getline(lineStream, data, ','); // DEPTHLOWER
        getline(lineStream, data, ','); // UDEPTH
        getline(lineStream, data, ','); // DEPTHREF
        getline(lineStream, data, ','); // DEPTHURL
        getline(lineStream, data, ','); // DIST
        getline(lineStream, data, ','); // DISTUPPER
        getline(lineStream, data, ','); // DISTLOWER
        getline(lineStream, data, ','); // UDIST
        getline(lineStream, data, ','); // DISTREF
        getline(lineStream, data, ','); // DISTURL
        getline(lineStream, data, ','); // DR
        getline(lineStream, data, ','); // DRUPPER
        getline(lineStream, data, ','); // DRLOWER
        getline(lineStream, data, ','); // UDR
        getline(lineStream, data, ','); // DRREF
        getline(lineStream, data, ','); // DRURL
        getline(lineStream, data, ','); // DVDT
        getline(lineStream, data, ','); // DVDTUPPER
        getline(lineStream, data, ','); // DVDTLOWER
        getline(lineStream, data, ','); // UDVDT
        getline(lineStream, data, ','); // DVDTREF
        getline(lineStream, data, ','); // DVDTURL
        getline(lineStream, data, ','); // EANAME
        getline(lineStream, data, ','); // EAURL
        getline(lineStream, data, ','); // ECC
        if (!data.empty())
            p.ECC = std::stof(data.c_str(), nullptr);
        else
            p.ECC = NAN;

        getline(lineStream, data, ','); // ECCUPPER
        if (!data.empty())
            p.ECCUPPER = std::stof(data.c_str(), nullptr);
        else
            p.ECCUPPER = NAN;

        getline(lineStream, data, ','); // ECCLOWER
        if (!data.empty())
            p.ECCLOWER = std::stof(data.c_str(), nullptr);
        else
            p.ECCLOWER = NAN;

        getline(lineStream, data, ','); // UECC
        if (!data.empty())
            p.UECC = std::stof(data.c_str(), nullptr);
        else
            p.UECC = NAN;

        getline(lineStream, data, ','); // ECCREF
        getline(lineStream, data, ','); // ECCURL
        getline(lineStream, data, ','); // EOD
        getline(lineStream, data, ','); // ETDNAME
        getline(lineStream, data, ','); // ETDURL
        getline(lineStream, data, ','); // FE
        getline(lineStream, data, ','); // FEUPPER
        getline(lineStream, data, ','); // FELOWER
        getline(lineStream, data, ','); // UFE
        getline(lineStream, data, ','); // FEREF
        getline(lineStream, data, ','); // FEURL
        getline(lineStream, data, ','); // FIRSTREF
        getline(lineStream, data, ','); // FIRSTURL
        getline(lineStream, data, ','); // FREEZE_ECC
        getline(lineStream, data, ','); // GAMMA
        getline(lineStream, data, ','); // GAMMAUPPER
        getline(lineStream, data, ','); // GAMMALOWER
        getline(lineStream, data, ','); // UGAMMA
        getline(lineStream, data, ','); // GAMMAREF
        getline(lineStream, data, ','); // GAMMAURL
        getline(lineStream, data, ','); // GL
        getline(lineStream, data, ','); // GRAVITY
        getline(lineStream, data, ','); // GRAVITYUPPER
        getline(lineStream, data, ','); // GRAVITYLOWER
        getline(lineStream, data, ','); // UGRAVITY
        getline(lineStream, data, ','); // GRAVITYREF
        getline(lineStream, data, ','); // GRAVITYURL
        getline(lineStream, data, ','); // H
        getline(lineStream, data, ','); // HD
        getline(lineStream, data, ','); // HIPP
        getline(lineStream, data, ','); // HR
        getline(lineStream, data, ','); // I
        if (!data.empty())
            p.I = std::stof(data.c_str(), nullptr);
        else
            p.I = NAN;

        getline(lineStream, data, ','); // IUPPER
        if (!data.empty())
            p.IUPPER = std::stof(data.c_str(), nullptr);
        else
            p.IUPPER = NAN;

        getline(lineStream, data, ','); // ILOWER
        if (!data.empty())
            p.ILOWER = std::stof(data.c_str(), nullptr);
        else
            p.ILOWER = NAN;

        getline(lineStream, data, ','); // UI
        if (!data.empty())
            p.UI = std::stof(data.c_str(), nullptr);
        else
            p.UI = NAN;

        getline(lineStream, data, ','); // IREF
        getline(lineStream, data, ','); // IURL
        getline(lineStream, data, ','); // IMAGING
        getline(lineStream, data, ','); // J
        getline(lineStream, data, ','); // JSNAME
        getline(lineStream, data, ','); // EPEURL
        getline(lineStream, data, ','); // K
        getline(lineStream, data, ','); // KUPPER
        getline(lineStream, data, ','); // KLOWER
        getline(lineStream, data, ','); // UK
        getline(lineStream, data, ','); // KREF
        getline(lineStream, data, ','); // KURL
        getline(lineStream, data, ','); // KOI
        getline(lineStream, data, ','); // KS
        getline(lineStream, data, ','); // KP
        getline(lineStream, data, ','); // LAMBDA
        getline(lineStream, data, ','); // LAMBDAUPPER
        getline(lineStream, data, ','); // LAMBDALOWER
        getline(lineStream, data, ','); // ULAMBDA
        getline(lineStream, data, ','); // LAMBDAREF
        getline(lineStream, data, ','); // LAMBDAURL
        getline(lineStream, data, ','); // LOGG
        getline(lineStream, data, ','); // LOGGUPPER
        getline(lineStream, data, ','); // LOGGLOWER
        getline(lineStream, data, ','); // ULOGG
        getline(lineStream, data, ','); // LOGGREF
        getline(lineStream, data, ','); // LOGGURL;
        getline(lineStream, data, ','); // MASS
        getline(lineStream, data, ','); // MASSUPPER
        getline(lineStream, data, ','); // MASSLOWER
        getline(lineStream, data, ','); // UMASS
        getline(lineStream, data, ','); // MASSREF
        getline(lineStream, data, ','); // MASSURL
        getline(lineStream, data, ','); // MICROLENSING
        getline(lineStream, data, ','); // MSINI
        getline(lineStream, data, ','); // MSINIUPPER
        getline(lineStream, data, ','); // MSINILOWER
        getline(lineStream, data, ','); // UMSINI
        getline(lineStream, data, ','); // MSINIREF
        getline(lineStream, data, ','); // MSINIURL
        getline(lineStream, data, ','); // MSTAR
        getline(lineStream, data, ','); // MSTARUPPER
        getline(lineStream, data, ','); // MSTARLOWER
        getline(lineStream, data, ','); // UMSTAR
        getline(lineStream, data, ','); // MSTARREF
        getline(lineStream, data, ','); // MSTARURL
        getline(lineStream, data, ','); // MULT
        getline(lineStream, data, ','); // NAME
        getline(lineStream, data, ','); // NCOMP
        if (!data.empty())
            p.NCOMP = std::stoi(data.c_str(), nullptr);
        else
            p.NCOMP = -1;
        getline(lineStream, data, ','); // NOBS
        getline(lineStream, data, ','); // OM
        if (!data.empty())
            p.OM = std::stof(data.c_str(), nullptr);
        else
            p.OM = NAN;

        getline(lineStream, data, ','); // OMUPPER
        if (!data.empty())
            p.OMUPPER = std::stof(data.c_str(), nullptr);
        else
            p.OMUPPER = NAN;

        getline(lineStream, data, ','); // OMLOWER
        if (!data.empty())
            p.OMLOWER = std::stof(data.c_str(), nullptr);
        else
            p.OMLOWER = NAN;

        getline(lineStream, data, ','); // UOM
        if (!data.empty())
            p.UOM = std::stof(data.c_str(), nullptr);
        else
            p.UOM = NAN;

        getline(lineStream, data, ','); // OMREF
        getline(lineStream, data, ','); // OMURL
        getline(lineStream, data, ','); // ORBREF
        getline(lineStream, data, ','); // ORBURL
        getline(lineStream, data, ','); // OTHERNAME
        getline(lineStream, data, ','); // PAR
        getline(lineStream, data, ','); // PARUPPER
        getline(lineStream, data, ','); // PARLOWER
        getline(lineStream, data, ','); // UPAR
        getline(lineStream, data, ','); // PER
        if (!data.empty())
            p.PER = std::stod(data.c_str(), nullptr);
        else
            p.PER = NAN;

        getline(lineStream, data, ','); // PERUPPER
        if (!data.empty())
            p.PERUPPER = std::stof(data.c_str(), nullptr);
        else
            p.PERUPPER = NAN;

        getline(lineStream, data, ','); // PERLOWER
        if (!data.empty())
            p.PERLOWER = std::stof(data.c_str(), nullptr);
        else
            p.PERLOWER = NAN;

        getline(lineStream, data, ','); // UPER
        if (!data.empty())
            p.UPER = std::stof(data.c_str(), nullptr);
        else
            p.UPER = NAN;

        getline(lineStream, data, ','); // PERREF
        getline(lineStream, data, ','); // PERURL
        getline(lineStream, data, ','); // PLANETDISCMETH
        getline(lineStream, data, ','); // R
        if (!data.empty())
            p.R = std::stod(data.c_str(), nullptr);
        else
            p.R = NAN;

        getline(lineStream, data, ','); // RUPPER
        if (!data.empty())
            p.RUPPER = std::stod(data.c_str(), nullptr);
        else
            p.RUPPER = NAN;

        getline(lineStream, data, ','); // RLOWER
        if (!data.empty())
            p.RLOWER = std::stod(data.c_str(), nullptr);
        else
            p.RLOWER = NAN;

        getline(lineStream, data, ','); //UR
        if (!data.empty())
            p.UR = std::stod(data.c_str(), nullptr);
        else
            p.UR = NAN;

        getline(lineStream, data, ','); // RREF
        getline(lineStream, data, ','); // RURL
        getline(lineStream, data, ','); // RA
        getline(lineStream, data, ','); // RA_STRING
        getline(lineStream, data, ','); // RHK
        getline(lineStream, data, ','); // RHOSTAR
        getline(lineStream, data, ','); // RHOSTARUPPER
        getline(lineStream, data, ','); // RHOSTARLOWER
        getline(lineStream, data, ','); // URHOSTAR
        getline(lineStream, data, ','); // RHOSTARREF
        getline(lineStream, data, ','); // RHOSTARURL
        getline(lineStream, data, ','); // RMS
        getline(lineStream, data, ','); // RR
        getline(lineStream, data, ','); // RRUPPER
        getline(lineStream, data, ','); // RRLOWER
        getline(lineStream, data, ','); // URR
        getline(lineStream, data, ','); // RRREF
        getline(lineStream, data, ','); // RRURL
        getline(lineStream, data, ','); // RSTAR
        if (!data.empty())
            p.RSTAR = std::stof(data.c_str(), nullptr);
        else
            p.RSTAR = NAN;

        getline(lineStream, data, ','); // RSTARUPPER
        if (!data.empty())
            p.RSTARUPPER = std::stof(data.c_str(), nullptr);
        else
            p.RSTARUPPER = NAN;

        getline(lineStream, data, ','); // RSTARLOWER
        if (!data.empty())
            p.RSTARLOWER = std::stof(data.c_str(), nullptr);
        else
            p.RSTARLOWER = NAN;

        getline(lineStream, data, ','); // URSTAR
        if (!data.empty())
            p.URSTAR = std::stof(data.c_str(), nullptr);
        else
            p.URSTAR = NAN;

        getline(lineStream, data, ','); // RSTARREF
        getline(lineStream, data, ','); // RSTARURL
        getline(lineStream, data, ','); // SAO
        getline(lineStream, data, ','); // SE
        getline(lineStream, data, ','); // SEREF
        getline(lineStream, data, ','); // SEURL
        getline(lineStream, data, ','); // SEDEPTHJ
        getline(lineStream, data, ','); // SEDEPTHJUPPER
        getline(lineStream, data, ','); // SEDEPTHJLOWER
        getline(lineStream, data, ','); // USEDEPTHJ
        getline(lineStream, data, ','); // SEDEPTHJREF
        getline(lineStream, data, ','); // SEDEPTHJURL
        getline(lineStream, data, ','); // SEDEPTHH
        getline(lineStream, data, ','); // SEDEPTHHUPPER
        getline(lineStream, data, ','); // SEDEPTHHLOWER
        getline(lineStream, data, ','); // USEDEPTHH
        getline(lineStream, data, ','); // SEDEPTHHREF
        getline(lineStream, data, ','); // SEDEPTHHURL
        getline(lineStream, data, ','); // SEDEPTHKS
        getline(lineStream, data, ','); // SEDEPTHKSUPPER
        getline(lineStream, data, ','); // SEDEPTHKSLOWER
        getline(lineStream, data, ','); // USEDEPTHKS
        getline(lineStream, data, ','); // SEDEPTHKSREF
        getline(lineStream, data, ','); // SEDEPTHKSURL
        getline(lineStream, data, ','); // SEDEPTHKP
        getline(lineStream, data, ','); // SEDEPTHKPUPPER
        getline(lineStream, data, ','); // SEDEPTHKPLOWER
        getline(lineStream, data, ','); // USEDEPTHKP
        getline(lineStream, data, ','); // SEDEPTHKPREF
        getline(lineStream, data, ','); // SEDEPTHKPURL
        getline(lineStream, data, ','); // SEDEPTH36
        getline(lineStream, data, ','); // SEDEPTH36UPPER
        getline(lineStream, data, ','); // SEDEPTH36LOWER
        getline(lineStream, data, ','); // USEDEPTH36
        getline(lineStream, data, ','); // SEDEPTH36REFx
        getline(lineStream, data, ','); // SEDEPTH36URLx
        getline(lineStream, data, ','); // SEDEPTH45
        getline(lineStream, data, ','); // SEDEPTH45UPPER
        getline(lineStream, data, ','); // SEDEPTH45LOWER
        getline(lineStream, data, ','); // USEDEPTH45
        getline(lineStream, data, ','); // SEDEPTH45REF
        getline(lineStream, data, ','); // SEDEPTH45URL
        getline(lineStream, data, ','); // SEDEPTH58
        getline(lineStream, data, ','); // SEDEPTH58UPPER
        getline(lineStream, data, ','); // SEDEPTH58LOWER
        getline(lineStream, data, ','); // USEDEPTH58
        getline(lineStream, data, ','); // EDEPTH58REF
        getline(lineStream, data, ','); // SEDEPTH58URL
        getline(lineStream, data, ','); // SEDEPTH80
        getline(lineStream, data, ','); // SEDEPTH80UPPER
        getline(lineStream, data, ','); // SEDEPTH80LOWER
        getline(lineStream, data, ','); // USEDEPTH80
        getline(lineStream, data, ','); // SEDEPTH80REF
        getline(lineStream, data, ','); // SEDEPTH80URL
        getline(lineStream, data, ','); // SEP
        getline(lineStream, data, ','); // SEPUPPER
        getline(lineStream, data, ','); // SEPLOWER
        getline(lineStream, data, ','); // USEP
        getline(lineStream, data, ','); // SEPREF
        getline(lineStream, data, ','); // SEPURL
        getline(lineStream, data, ','); // SET
        getline(lineStream, data, ','); // SETUPPER
        getline(lineStream, data, ','); // SETLOWER
        getline(lineStream, data, ','); // USET
        getline(lineStream, data, ','); // SETREF
        getline(lineStream, data, ','); // SETURL
        getline(lineStream, data, ','); // SHK
        getline(lineStream, data, ','); // SIMBADNAME
        getline(lineStream, data, ','); // SIMBADURL
        getline(lineStream, data, ','); // SPECREF
        getline(lineStream, data, ','); // SPECURL
        getline(lineStream, data, ','); // STAR
        std::string  speckStarname = getSpeckStarName(data);
        glm::vec3 position = getStarPosition(speckStarname);
        p.POSITIONX = position[0];
        p.POSITIONY = position[1];
        p.POSITIONZ = position[2];

        getline(lineStream, data, ','); // STARDISCMETH
        getline(lineStream, data, ','); // T0
        getline(lineStream, data, ','); // T0UPPER
        getline(lineStream, data, ','); // T0LOWER
        getline(lineStream, data, ','); // UT0
        getline(lineStream, data, ','); // T0REF
        getline(lineStream, data, ','); // T0URL
        getline(lineStream, data, ','); // T14
        getline(lineStream, data, ','); // T14UPPER
        getline(lineStream, data, ','); // T14LOWER
        getline(lineStream, data, ','); // UT14
        getline(lineStream, data, ','); // T14REF
        getline(lineStream, data, ','); // T14URL
        getline(lineStream, data, ','); // TEFF
        float teff;
        if (!data.empty())
            teff = std::stof(data.c_str(), nullptr);
        else
            teff = NAN;

        getline(lineStream, data, ','); // TEFFUPPER
        getline(lineStream, data, ','); // TEFFLOWER
        getline(lineStream, data, ','); // UTEFF
        getline(lineStream, data, ','); // TEFFREF
        getline(lineStream, data, ','); // TEFFURL
        getline(lineStream, data, ','); // TIMING
        getline(lineStream, data, ','); // TRANSIT
        getline(lineStream, data, ','); // TRANSITREF
        getline(lineStream, data, ','); // TRANSITURL
        getline(lineStream, data, ','); // TREND
        getline(lineStream, data, ','); // TT
        if (!data.empty())
            p.TT = std::stod(data.c_str(), nullptr);
        else
            p.TT = NAN;

        getline(lineStream, data, ','); // TTUPPER
        if (!data.empty())
            p.TTUPPER = std::stof(data.c_str(), nullptr);
        else
            p.TTUPPER = NAN;

        getline(lineStream, data, ','); // TTLOWER
        if (!data.empty())
            p.TTLOWER = std::stof(data.c_str(), nullptr);
        else
            p.TTLOWER = NAN;

        getline(lineStream, data, ','); // UTT
        if (!data.empty())
            p.UTT = std::stof(data.c_str(), nullptr);
        else
            p.UTT = NAN;

        getline(lineStream, data, ','); // TTREF
        getline(lineStream, data, ','); // TTURL
        getline(lineStream, data, ','); // V
        getline(lineStream, data, ','); // VREF
        getline(lineStream, data, ','); // VURL
        getline(lineStream, data, ','); // VSINI
        getline(lineStream, data, ','); // VSINIUPPER
        getline(lineStream, data, ','); // VSINILOWER
        getline(lineStream, data, ','); // UVSINI
        getline(lineStream, data, ','); // VSINIREF
        getline(lineStream, data, ','); // VSINIURL
        getline(lineStream, data, ','); // KEPID
        if (!data.empty())
            isKeplerObject = true;
        getline(lineStream, data); // KDE

        if (!isKeplerObject) {
            // calculate B-V from Teff if not exsisting
            if (std::isnan(p.BMV)) {
                if (!std::isnan(teff)) {
                    std::ifstream teffToBvFile(
                        absPath("${BASE}/modules/exoplanets/teff_bv.txt")
                    );

                    if (!teffToBvFile.good()) {
                        LERROR(fmt::format("Failed to open teff_bv.txt file"));
                        return;
                    }

                    float BV = 0.f;
                    float bvUpper = 0.f;
                    float bvLower = 0.f;
                    float teffLower, teffUpper;
                    std::string row, teffString, bvString;
                    while (getline(teffToBvFile, row)) {
                        std::istringstream lineStream(row);
                        getline(lineStream, teffString, ',');
                        getline(lineStream, bvString);

                        float teffCurrent = std::stof(teffString.c_str(), nullptr);
                        float bvCurrent = std::stof(bvString.c_str(), nullptr);

                        if (teff > teffCurrent) {
                            teffLower = teffCurrent;
                            bvLower = bvCurrent;
                        }
                        else {
                            teffUpper = teffCurrent;
                            bvUpper = bvCurrent;
                            if (bvLower == 0.f) {
                                BV = 2.f;
                            }
                            else {
                                float bvDiff = (bvUpper - bvLower);
                                float teffDiff = (teffUpper - teffLower);
                                BV = ((bvDiff * (teff - teffLower)) / teffDiff) + bvLower;
                            }
                            break;
                        }
                    }
                    teffToBvFile.close();
                    p.BMV = BV;
                }
                else {
                    p.BMV = NAN;
                }
            }

            // crate look-up table
            long pos = binFile.tellp();
            planetName = speckStarname + " " + component;
            lutFile << planetName << "," << pos << std::endl;
            binFile.write((char *)&p, sizeof(Exoplanet));
        }
    }

    csvFile.close();
    binFile.close();
    lutFile.close();

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
                KeyInputCsv,
                new StringAnnotationVerifier("A file path to a csv file"),
                Optional::No,
                "The csv file to extract data from"
            },
            {
                KeyInputSpeck,
                new StringAnnotationVerifier("A file path to a speck file"),
                Optional::No,
                "The speck file with star location"
            },
            {
                KeyOutputBin,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The bin file to export data into"
            },
            {
                KeyOutputLut,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The txt file to write look-up table into"
            }
        }
    };
}

} // namespace openspace::exoplanets
