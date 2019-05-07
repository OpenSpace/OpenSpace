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

//FIX: Delete librarys and files not necessary for code!

#include <modules\marsrover\surfaceprojection\spacecraftprojection.h>
//#include <modules\marsrover\surfaceprojection\mslterrain.h>

#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/glm.h>

#include <vector>
#include <iterator>
#include <algorithm>

#include <ghoul/opengl/uniformcache.h>
#include <memory>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <iostream>
#include <fstream>
#include <cassert>
#include <cstring>
#include <vector>
#include <array>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>

namespace {
    const char* KeyKernels = "Kernels";
    const char* DefaultReferenceFrame = "GALACTIC";

    //static const openspace::properties::Property::PropertyInfo DestinationPlanetInfo = {
    //    "DestinationPlanet",
    //    "DestinationSurface",
    //    "description"
    //};

    //static const openspace::properties::Property::PropertyInfo PositionInfo = {
    //    "Position",
    //    "Position",
    //    "This value is used as a static offset (in meters) that is applied to the scene "
    //    "graph node that this transformation is attached to relative to its parent."
    //};


    static const openspace::properties::Property::PropertyInfo TargetInfo = {
        "Target",
        "Target",
        "This is the SPICE NAIF name for the body whose translation is to be computed by "
        "the SpiceTranslation. It can either be a fully qualified name (such as 'EARTH') "
        "or a NAIF integer id code (such as '399')."
    };

    static const openspace::properties::Property::PropertyInfo ObserverInfo = {
        "Observer",
        "Observer",
        "This is the SPICE NAIF name for the parent of the body whose translation is to "
        "be computed by the SpiceTranslation. It can either be a fully qualified name "
        "(such as 'SOLAR SYSTEM BARYCENTER') or a NAIF integer id code (such as '0')."
    };

    static const openspace::properties::Property::PropertyInfo FrameInfo = {
        "Frame",
        "Reference Frame",
        "This is the SPICE NAIF name for the reference frame in which the position "
        "should be retrieved. The default value is GALACTIC."
    };

    static const openspace::properties::Property::PropertyInfo HeightTextureInfo = {
        "HeightTexture",
        "Reference Frame",
        "This is the SPICE NAIF name for the reference frame in which the position "
        "should be retrieved. The default value is GALACTIC."
    };
} // namespace

namespace openspace {

constexpr const char* _loggerCat = "ghoul.opengl.FramebufferObject";

SpacecraftProjection::SpacecraftProjection(const ghoul::Dictionary& dictionary)
    : _target(TargetInfo)
    , _observer(ObserverInfo)
    , _frame(FrameInfo, DefaultReferenceFrame)
    ,_heightTexture(HeightTextureInfo)//"HeightmapTesting.jpg"
{
    _target = dictionary.value<std::string>(TargetInfo.identifier);
    _observer = dictionary.value<std::string>(ObserverInfo.identifier);
    _heightTexture = dictionary.value<std::string>(HeightTextureInfo.identifier);

    if (dictionary.hasKey(FrameInfo.identifier)) {
        _frame = dictionary.value<std::string>(FrameInfo.identifier);

        //Read once
        readHeightMapFile(); //memory problem, need to delete old data???? SpacecraftProjection~()
    }

    auto loadKernel = [](const std::string& kernel) {
        if (!FileSys.fileExists(kernel)) {
            throw SpiceManager::SpiceException("Kernel '" + kernel + "' does not exist");
        }

        try {
            SpiceManager::ref().loadKernel(kernel);
        }
        catch (const SpiceManager::SpiceException& exception) {
            LERRORC("SpiceEphemeris", exception.message);
        }
    };

    if (dictionary.hasKey(KeyKernels)) {
        // Due to the specification, we can be sure it is either a Dictionary or a string
        if (dictionary.hasValue<std::string>(KeyKernels)) {
            std::string kernel = dictionary.value<std::string>(KeyKernels);
            loadKernel(absPath(kernel));
        }
        else {
            ghoul::Dictionary kernels = dictionary.value<ghoul::Dictionary>(KeyKernels);
            for (size_t i = 1; i <= kernels.size(); ++i) {
                std::string kernel = kernels.value<std::string>(std::to_string(i));
                loadKernel(absPath(kernel));
            }
        }
    }

    auto update = [this](){
        requireUpdate();
        notifyObservers();
    };

    _target.onChange(update);
    addProperty(_target);

    _observer.onChange(update);
    addProperty(_observer);

    _frame.onChange(update);
    addProperty(_frame);
}


void SpacecraftProjection::readHeightMapFile() 
{
    const float changeRange = 0.00390625; //in range [0, 1] devide texel value by this to get in range [0, 256]
    
    _texture = nullptr;

    if (_texturePath != "") {
        _texture = ghoul::io::TextureReader::ref().loadTexture( absPath(_texturePath) );
        
        if (_texture) {
            LDEBUGC(
                "RenderableModel",
                fmt::format("Loaded texture from '{}'", absPath(_texturePath))
            );
            fileRead = true;          
            glm::dvec3 height_point;
            glm::dvec4 texel;

            arrayWidth = _texture->width();
            arrayHeight = _texture->height();

            //Size setup
            heightMap.resize(arrayWidth);
            for (int i = 0; i < arrayWidth ; ++i) 
                heightMap[i].resize(arrayHeight);

            for (int y = 0; y < arrayHeight; y++) {
                for (int x = 0; x < arrayWidth; x++) 
                    heightMap[x][y] = _texture->texelAsFloat(x, y)[0]; 
            }
        }
    }
    
    /*
    LWARNING("TESTING TESTING");
    const char* pixels = reinterpret_cast<const char*>(_texture->pixelData());//textureconversion.cpp
    */   
}


glm::dvec3 SpacecraftProjection::getNewHeight(glm::dvec3 p) 
{
    //           \                 X    MSL
    //            \            . 
    //             \\      .            surface normal        
    //              \\ . 
    //             . \\
    //         .      \\ models
    //     .           \
    // x                \ planet surface
    // origo

    //Here we want to first find vector pointing towards mars barycenter with respect to the first wheel we want to project onto the surface
    //When we have that vector we can find the point on the height map which corresponds to the xy coordinates
    //we are therfore interested in the long/lat coordinates of the first wheel, and want with those to find the point on the heightmap that corresponds to that point

    //1. Get long/lat coordinates depeding on the wheel
    //2. want to find the radius value for the heightmap at that long/lat coordinate
    //3. project down on that

    //4. do the same as above for the front wheel

    //SceneGraphNode *planetNode =  OsEng.renderEngine().scene()->sceneGraphNode("Mars");//parent();
    
    //glm::dvec3 directionVector = glm::dvec3(0.0) - p;

    //glm::dvec3 heightPoint = interceptionPoint(mslPosition, directionVector);
    //glm::dvec3 heightPoint = getSurfaceHeight(directionVector);

    //one longite value corresponds to 
    const double marsRadius = 3389278.46; //3389 km
    const double marsPath = 3000.0; //3 km 

    double pixelHeight = marsPath/arrayHeight; //4.84m
    double pixelWidth  = marsPath/arrayWidth;

    glm::dvec2 centerPixel = glm::dvec2(arrayWidth/2, arrayHeight/2);

    //find out which pixel that corresponds to the long lat pos for rover, --> get new radius value

    glm::dvec3 MSL_longLat = SpiceManager::ref().fromCartesianToLatitudinal(p);

    //hamta punktens varde i long lat coords
    //rotera heightmap enligt marsytan
    //ta heightmaps long lat och 
    //loop through heightmap
    std::vector<std::vector<double>>::const_iterator row;
    std::vector<double>::const_iterator col;

    glm::dvec3 pixelPos;
    glm::dvec3 newPixelPos;
    double resultingHeight = 0.0;
    double resultHeightPosition;

    //int numx;// = std::distance(heightMap.begin(), heightMap.end());
    //int numy;
    //LWARNING(std::to_string(num));

    //Correct height with heightmap
    /*
    for (row = heightMap.begin(); row != heightMap.end(); ++row)
    { 
         for (col = row->begin(); col != row->end(); ++col)
         {  
            //LWARNING(std::to_string(row));
            //LWARNING(std::to_string(col));
            //LWARNING(std::to_string("hej"));
            //LWARNING(std::to_string(*col));

            //numx = std::distance(heightMap.begin(), row);
            //double x = row;
            //LWARNING(std::to_string(std::distance(heightMap.cbegin(), col)));
            
            double meterWidth = (centerPixel[0] - ( std::distance(heightMap.begin(), row) + 1.0)) * pixelWidth; //meters from center of image
            double meterHeight = (centerPixel[0] - ( std::distance(row->begin(), col) + 1.0)) * pixelHeight; //meters from center of image
            //double meterHeight = (centerPixel[1] - col) * pixelHeight; //meters from center of image

            //rotera enligt kamerans long/lat!!! long lat/ xyz

            //OBS DONT FORGET CAMREA!
            //Change marsradius!
            //pixelPos = glm::dvec3(meterWidth, meterHeight, marsRadius);// * marsTransform;// * camera; //get value for pixel on mars in XYZ
            pixelPos = glm::dvec3(meterWidth, meterHeight, 0.5);// * marsTransform;// * camera; //get value for pixel on mars in XYZ

            //convert to long/lat (rad, long, lat)
            newPixelPos = SpiceManager::ref().ConvertToLatitudinalCoords(pixelPos);
            //LWARNING(std::to_string(pixelPos));LWARNING(std::to_string(newPixelPos));

            //if long and lat are the same, we want the new height
            if (newPixelPos[1] == MSL_longLat[1] && newPixelPos[2] == MSL_longLat[2]) {
                //double resultHeightPosition = newPixelPos[0];

                MSL_longLat[0] = newPixelPos[0];
            
                //convert to xyz coors!

                glm::dvec3 newPos = SpiceManager::ref().ConvertToCartesianCoords(MSL_longLat);

                return newPos;
            }
        }
    } */

    //MSLposition[2] == *col * motsvarandehojdvarde for 0-255 :) ;

    //LERROR(fmt::format("time  '{}'", time.j2000Seconds()));
    //LERROR(fmt::format("2. _destinationPlanet sunpos  '{}'", mslPosition));
    //LERROR(fmt::format(*Time));

    //return getNewPosition(_position);

    //glm::dvec3 test = glm::dvec3()

    glm::dvec3 surfacePoint(1.0); //= getSurfaceHeight(p); 

    return surfacePoint;
}


//Converts from Rectangular coordinates to Latitudinal coordinates, depending on Mars Barycenter
glm::dvec3 SpacecraftProjection::convertFromRectangularToLatitudinal(glm::dvec3 p) 
{   

    glm::dvec3 convertedPoint = SpiceManager::ref().fromLatitudinalToCartesian(p);

    return convertedPoint;
}


glm::dvec3 SpacecraftProjection::position(const UpdateData& data) const 
{
    double lightTime = 0.0;

    glm::dvec3 spacecraftPosition = SpiceManager::ref().targetPosition(
        _target,
        _observer,
        _frame,
        {},
        data.time.j2000Seconds(),
        lightTime
    ) * glm::pow(10.0, 3.0);

    const double f = 0.00589;
    const double polarRadius  = 3376200.0;
    const double equatRadius  = 3396200.0;
    const double sphereRadius = 3396190.0;

    glm::dvec3 scaleFactor = glm::dvec3((sphereRadius / equatRadius), (sphereRadius / equatRadius), (sphereRadius / polarRadius));

    glm::dvec3 newPosition = spacecraftPosition; // * scaleFactor;

    //double rad;
    //double lon;
    //double lat; 

    newPosition = SpiceManager::ref().fromCartesianToLatitudinal(newPosition);

    //static height correction
    //newPosition.x = newPosition.x + 428.6;
    newPosition.x = newPosition.x + 557.5;

    newPosition = SpiceManager::ref().fromLatitudinalToCartesian(newPosition);

    return newPosition;

}

}
