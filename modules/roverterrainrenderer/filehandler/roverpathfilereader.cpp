/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2017                                                               *
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

#include <modules/roverterrainrenderer/filehandler/roverpathfilereader.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>

#include <fstream>
#include <sstream>
#include <gdal_priv.h>
#include "ogr_feature.h"
#include <ogrsf_frmts.h>

namespace ghoul::filesystem { class File; }

template<typename Out>
static void split(const std::string &s, char delim, Out result) {
    std::stringstream ss;
    ss.str(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        *(result++) = item;
    }
}

static std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

namespace {
    const std::string _loggerCat        = "RoverPathFileReader";
    const char* keyRoverLocationPath    = "RoverLocationPath";
    const char* keyAbsPathToTextures    = "AbsPathToTextures";
    const char* keyAbsPathToModels      = "AbsPathToModels";
}

namespace openspace {

std::vector<std::shared_ptr<Subsite>> RoverPathFileReader::extractAllSubsites(const ghoul::Dictionary dictionary) {
    std::string roverLocationFilePath;
    if (!dictionary.getValue(keyRoverLocationPath, roverLocationFilePath))
        throw ghoul::RuntimeError(std::string(keyRoverLocationPath) + " must be specified!");

    std::fstream in(roverLocationFilePath.c_str());

    if (!in.is_open())
        throw ghoul::FileNotFoundError(roverLocationFilePath);

    GDALDataset *poDS;

    poDS = (GDALDataset*)GDALOpenEx(roverLocationFilePath.c_str(), GDAL_OF_VECTOR, NULL, NULL, NULL);

    OGRLayer *poLayer = poDS->GetLayerByName("rover_locations");

    OGRFeature *poFeature;
    poLayer->ResetReading();

    double siteLat;
    double siteLon;
    std::vector<std::shared_ptr<Subsite>> subsites;

    while ((poFeature = poLayer->GetNextFeature()) != NULL) {

        // Extract coordinates from OGR
        std::string frame = poFeature->GetFieldAsString("frame");
        std::string site = poFeature->GetFieldAsString("site");
        std::string drive = poFeature->GetFieldAsString("drive");
        std::string sol = poFeature->GetFieldAsString("sol");
        double lat = poFeature->GetFieldAsDouble("plcl");
        double lon = poFeature->GetFieldAsDouble("longitude");

        // Saves all coordinates for rendering the path and only site coordinates for rendering sites.
        // GetFieldAsDouble resturns zero (0) if field is empty.
        if (lat != 0 && lon != 0) {
            if (frame == "SITE") {
                siteLat = lat;
                siteLon = lon;
            }

            std::string type = "site";
            std::shared_ptr<Subsite> subsite = std::make_shared<Subsite>();
            subsite->site = convertString(site, type);
            type = "drive";
            subsite->drive = convertString(drive, type);
            subsite->geodetic.lat = lat / 180.0 * glm::pi<double>();
            subsite->geodetic.lon = lon / 180.0 * glm::pi<double>();
            subsite->frame = frame;
            subsite->siteGeodetic.lat = siteLat / 180.0 * glm::pi<double>();
            subsite->siteGeodetic.lon = siteLon / 180.0 * glm::pi<double>();
            subsite->sol = sol;

            // All features with the the frame is "Site" will have "Drive" that is -1.
            // The feature right after each site frame has the same coordinates as the site frame.
            // E.g. feature 1: "Frame = SITE, Site = 6, Drive = -1, Lat = -4.7000, Lon = 137.4000"
            //      feature 2: "Frame = ROVER, Site = 6, Drive = 0, Lat = -4.7000, Lon = 137.4000"
            if (drive != "-1")
                subsites.push_back(subsite);
        }
        OGRFeature::DestroyFeature(poFeature);
    }
    GDALClose(poDS);

    return subsites;
}

std::vector<std::shared_ptr<Subsite>> RoverPathFileReader::extractSubsitesWithModels(const ghoul::Dictionary dictionary) {

    // Make sure the dictionary includes the necessary keys
    std::string roverLocationFilePath;
    if (!dictionary.getValue(keyRoverLocationPath, roverLocationFilePath))
        throw ghoul::RuntimeError(std::string(keyRoverLocationPath) + " must be specified!");

    std::string absPathToTextures;
    if (!dictionary.getValue(keyAbsPathToTextures, absPathToTextures))
        throw ghoul::RuntimeError(std::string(keyAbsPathToTextures) + " must be specified!");

    std::string absPathToTModels;
    if (!dictionary.getValue(keyAbsPathToModels, absPathToTModels))
        throw ghoul::RuntimeError(std::string(keyAbsPathToModels) + " must be specified!");

    // Extract all subsites in the data set given the path to the file
    ghoul::Dictionary tempDictionary;
    tempDictionary.setValue(keyRoverLocationPath, roverLocationFilePath);
    std::vector<std::shared_ptr<Subsite>> allSubsites = extractAllSubsites(tempDictionary);

    std::vector<std::shared_ptr<Subsite>> subsitesWithModels;
    for (auto subsite : allSubsites) {
        std::string pathToMeshFolderLevel1;
        std::string pathToMeshFolderLevel2;
        std::string pathToMeshFolderLevel3;

        // Convert the site and drive string to match the folder structure
        std::string site = convertString(subsite->site, "site");
        std::string drive = convertString(subsite->drive, "drive");
        pathToMeshFolderLevel1 = absPathToTModels + "level1/" + "site" + site + "/" + "drive" + drive;
        pathToMeshFolderLevel2 = absPathToTModels + "level2/" + "site" + site + "/" + "drive" + drive;
        pathToMeshFolderLevel3 = absPathToTModels + "level3/" + "site" + site + "/" + "drive" + drive;

        // If the folder exists it means there are models for this subsite, then check if that
        // specific site/drive combination has already been added. If the models haven't already been
        // added, loop through the text file with file names and add those to the subsite.
        // Also store information about which levels are available for this specific subsite.
        std::string restPath = "/OBJ.obj";

        bool level1Exist = FileSys.fileExists(pathToMeshFolderLevel2 + restPath);
        bool level2Exist = FileSys.fileExists(pathToMeshFolderLevel2 + restPath);
        bool level3Exist = FileSys.fileExists(pathToMeshFolderLevel3 + restPath);

        // TODO: refactor like hell!!!
        std::string pathToMeshFolder;
        if (level1Exist) {
            subsite->availableLevels.push_back(1);
            pathToMeshFolder = pathToMeshFolderLevel1;
        }
        if (level2Exist) {
            subsite->availableLevels.push_back(2);
            pathToMeshFolder = pathToMeshFolderLevel2;
        }
        if (level3Exist) {
            subsite->availableLevels.push_back(3);
            pathToMeshFolder = pathToMeshFolderLevel3;
        }
        bool modelExists = false;
        if (level1Exist || level2Exist || level3Exist) {
            for (auto controlSubsite : subsitesWithModels) {
                if (subsite->site == controlSubsite->site && subsite->drive == controlSubsite->drive) {
                    modelExists = true;
                    break;
                }
            }
            if (!modelExists) {
                std::string pathToFilenamesTextFile = pathToMeshFolder + "/filenames.txt";
                std::string pathToColoredFilenamesTextFile = pathToMeshFolder + "/mastcam.txt";
                std::string pathToRotationMatrixTextFile = pathToMeshFolder + "/rotationmatrix.txt";

                std::shared_ptr<RoverPathFileReader::TextureInformation> textureInformation = extractTextureInfo(pathToFilenamesTextFile);

                std::shared_ptr<RoverPathFileReader::TextureInformation> coloredTextureInformation = extractTextureInfo(pathToColoredFilenamesTextFile);

                glm::dmat4 subsiteRotationMatrix = extractRotationMatrix(pathToRotationMatrixTextFile);

                subsite->coloredTextureFileNames = coloredTextureInformation->fileNames;
                subsite->cameraColoredInfoVector = coloredTextureInformation->cameraInfoVector;
                subsite->rotationMatrix = subsiteRotationMatrix;

                subsite->fileNames = textureInformation->fileNames;
                subsite->cameraInfoVector = textureInformation->cameraInfoVector;
                subsite->pathToTextureFolder = absPathToTextures;
                subsite->pathToGeometryFolder = absPathToTModels;

                subsitesWithModels.push_back(subsite);
            }
        }
    }
    return subsitesWithModels;
}

std::string RoverPathFileReader::convertString(const std::string sitenr, const std::string type) {
    int k = std::stoi(sitenr);

    std::string temp;
    if (type == "site") {
        if (k < 10) {
            temp = "00" + std::to_string(k);
        }
        else if (k < 100) {
            temp = "0" + std::to_string(k);
        }
    }
    else if (type == "drive") {
        if (k < 10) {
            temp = "000" + std::to_string(k);
        }
        else if (k < 100) {
            temp = "00" + std::to_string(k);
        }
        else if (k < 1000) {
            temp = "0" + std::to_string(k);
        }
        else {
            temp = std::to_string(k);
        }
    }
    return temp;
}

glm::dmat4 RoverPathFileReader::extractRotationMatrix(std::string filename) {
    std::string absoluteFilePath = absPath(filename);
    std::string line;
    std::ifstream myFile(absoluteFilePath);
    glm::dmat4 temp2;
    int counter = 0;
    if (myFile.is_open()) {
        while (std::getline(myFile, line)) {
            std::vector<std::string> temp;
            temp = split(line, ',');
            temp2[counter][0] = std::stod(temp.at(0));
            temp2[counter][1] = std::stod(temp.at(1));
            temp2[counter][2] = std::stod(temp.at(2));
            temp2[counter][3] = std::stod(temp.at(3));
            counter++;
        }
    }

    return temp2;

}

std::shared_ptr<RoverPathFileReader::TextureInformation> RoverPathFileReader::extractTextureInfo(std::string filePath) {
    std::string absoluteFilePath = absPath(filePath);
    std::replace(absoluteFilePath.begin(), absoluteFilePath.end(), '\\', '/');
    std::string fileName;
    std::ifstream myfile(absoluteFilePath);
    std::vector<std::string> fileNameVector;
    std::vector<PointCloudInfo> cameraInfoVector;

    if (myfile.is_open()) {
        while (std::getline(myfile, fileName)) {
            if (fileName.empty())
                break;
            fileNameVector.push_back(fileName);
            PointCloudInfo mInfo;

            std::string coordinates;
            std::getline(myfile, coordinates);
            std::vector<std::string> temp;
            temp = split(coordinates, ',');

            mInfo._cameraCenter.x = std::stof(temp.at(0));
            mInfo._cameraCenter.y = std::stof(temp.at(1));
            mInfo._cameraCenter.z = std::stof(temp.at(2));

            std::getline(myfile, coordinates);
            temp = split(coordinates, ',');

            mInfo._cameraAxis.x = std::stof(temp.at(0));
            mInfo._cameraAxis.y = std::stof(temp.at(1));
            mInfo._cameraAxis.z = std::stof(temp.at(2));

            std::getline(myfile, coordinates);
            temp = split(coordinates, ',');

            mInfo._cameraHorizontal.x = std::stof(temp.at(0));
            mInfo._cameraHorizontal.y = std::stof(temp.at(1));
            mInfo._cameraHorizontal.z = std::stof(temp.at(2));

            std::getline(myfile, coordinates);
            temp = split(coordinates, ',');

            mInfo._cameraVector.x = std::stof(temp.at(0));
            mInfo._cameraVector.y = std::stof(temp.at(1));
            mInfo._cameraVector.z = std::stof(temp.at(2));

            cameraInfoVector.push_back(mInfo);
        }
        myfile.close();
    }

    std::shared_ptr<TextureInformation> textureInformation = std::make_shared<TextureInformation>();
    textureInformation->cameraInfoVector = cameraInfoVector;
    textureInformation->fileNames = fileNameVector;

    return textureInformation;
}

} // namespace openspace
