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

#include <modules/marsrover/rotation/advancedspicerotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>

#include <iostream>
#include <string>

namespace {
    const char* KeyKernels = "Kernels";

    static const openspace::properties::Property::PropertyInfo SourceInfo = {
        "SourceFrame",
        "Source",
        "This value specifies the source frame that is used as the basis for the "
        "coordinate transformation. This has to be a valid SPICE name."
    };

    static const openspace::properties::Property::PropertyInfo DestinationInfo = {
        "DestinationFrame",
        "Destination",
        "This value specifies the destination frame that is used for the coordinate "
        "transformation. This has to be a valid SPICE name."
    };

    static const openspace::properties::Property::PropertyInfo rotAxisSpice = {
        "RotationAxisSpice",
        "RotationAxis1",
        "This value specifies about which axis to rotate about for correction."
        "Valid strings are: X90, X180, X90neg, X180neg, Y90, Y180, Y90neg, Y180neg, Z90, Z180, Z90neg, Z180neg"
    };

    static const openspace::properties::Property::PropertyInfo rotAngleSpice = {
        "RotationAngleSpice",
        "RotationAxis1",
        "This value specifies about which axis to rotate about for correction."
        "Valid strings are: X90, X180, X90neg, X180neg, Y90, Y180, Y90neg, Y180neg, Z90, Z180, Z90neg, Z180neg"
    };

    static const openspace::properties::Property::PropertyInfo rotAxisCorrection = {
        "RotationAxisStatic",
        "RotationAxis2",
        "This value specifies about which axis to rotate about for correction."
        "Valid strings are: X90, X180, X90neg, X180neg, Y90, Y180, Y90neg, Y180neg, Z90, Z180, Z90neg, Z180neg"
    };    

    static const openspace::properties::Property::PropertyInfo rotAngle = {
        "RotationAngle",
        "RotationAngle",
        "This value specifies the angle of correction around staticAxisRotation2."
        "Valid strings are expressed in radians"
    };

    static const openspace::properties::Property::PropertyInfo rotDir1 = {
        "RotationDirectionSpice",
        "direction1",
        "This value specifies the angle of correction around staticAxisRotation2."
        "Valid strings are expressed in radians"
    };

    static const openspace::properties::Property::PropertyInfo rotDir2 = {
        "RotationDirectionStatic",
        "direction2",
        "This value specifies the angle of correction around staticAxisRotation2."
        "Valid strings are expressed in radians"
    };

} // namespace

namespace openspace {
    
    constexpr const char* _loggerCat = "ghoul.opengl.FramebufferObject";


AdvancedSpiceRotation::AdvancedSpiceRotation(const ghoul::Dictionary& dictionary)
    : _sourceFrame(SourceInfo) // @TODO Missing documentation
    , _destinationFrame(DestinationInfo)
    , _rotationAxisSPICE(rotAxisSpice)
    , _rotationAngleSPICE(rotAngleSpice)
    , _rotationAxisCorrection(rotAxisCorrection)
    , _rotationAngle(rotAngle, 0.f)
    , _rotationDir1(rotDir1, true)
    , _rotationDir2(rotDir2, true)
    
{
    _sourceFrame = dictionary.value<std::string>(SourceInfo.identifier);
    _destinationFrame = dictionary.value<std::string>(DestinationInfo.identifier);

    _rotationAxisSPICE = static_cast<int>(dictionary.value<double>(rotAxisSpice.identifier));
    _rotationAngleSPICE = static_cast<int>(dictionary.value<double>(rotAngleSpice.identifier));
    _rotationAxisCorrection = static_cast<int>(dictionary.value<double>(rotAxisCorrection.identifier));
    _rotationAngle = static_cast<float>(dictionary.value<double>(rotAngle.identifier));
    _rotationDir1 = dictionary.value<bool>(rotDir1.identifier);
    _rotationDir2 = dictionary.value<bool>(rotDir2.identifier);


    if (dictionary.hasKeyAndValue<std::string>(KeyKernels)) {
        SpiceManager::ref().loadKernel(dictionary.value<std::string>(KeyKernels));
    }
    else if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyKernels)) {
        ghoul::Dictionary kernels = dictionary.value<ghoul::Dictionary>(KeyKernels);
        for (size_t i = 1; i <= kernels.size(); ++i) {
            if (!kernels.hasKeyAndValue<std::string>(std::to_string(i))) {
                throw ghoul::RuntimeError("Kernels has to be an array-style table");
            }

            std::string kernel = kernels.value<std::string>(std::to_string(i));
            SpiceManager::ref().loadKernel(kernel);
        }
    }

    //Fix: needed?
    addProperty(_sourceFrame);
    addProperty(_destinationFrame);
    addProperty(_rotationAxisSPICE);
    addProperty(_rotationAngleSPICE);
    addProperty(_rotationAxisCorrection);
    addProperty(_rotationAngle);
    addProperty(_rotationDir1);
    addProperty(_rotationDir2);

    auto update = [this]() {
        requireUpdate();
    };


    //Fix: needed?
    _sourceFrame.onChange(update);
    _destinationFrame.onChange(update);
    
    _rotationAxisSPICE.onChange(update);
    _rotationAngleSPICE.onChange(update);
    _rotationAxisCorrection.onChange(update);
    _rotationAngle.onChange(update);
    _rotationDir1.onChange(update);
    _rotationDir2.onChange(update);
}



glm::dmat3 AdvancedSpiceRotation::matrix(const UpdateData& data) const {
    try {
        glm::dmat3 result;

        result = SpiceManager::ref().positionTransformMatrix(
            _sourceFrame,
            _destinationFrame,
            data.time.j2000Seconds()
        );
        if (_rotationAxisSPICE != 0) 
        {
            double angle = SpiceManager::ref().getEulerAngle(result, _rotationAngleSPICE);
            result = getRotationMatrix(_rotationAxisSPICE, _rotationDir1, angle);
        }

        if (_rotationAxisCorrection == 1 || _rotationAxisCorrection == 2 || _rotationAxisCorrection == 3) 
            result = result * getRotationMatrix(_rotationAxisCorrection, _rotationDir2, _rotationAngle);
        
        return glm::transpose(result); 
    }
    catch (const SpiceManager::SpiceException&) {
        return glm::dmat3(1.0);
    }
}

//FIX; remove posDirections
glm::dmat3 AdvancedSpiceRotation::getRotationMatrix(int axis, bool posDirection, double radians) const {
    
    glm::dmat3 rotMatrix = glm::dmat3(1.0);

    double sin = glm::sin(radians);
    double cos = glm::cos(radians);

    switch(axis)
    {
        case 1:
            if (posDirection) 
                rotMatrix = glm::dmat3( 1.0, 0.0,  0.0,
                                        0.0, cos, -sin, 
                                        0.0, sin,  cos );
            else 
                rotMatrix = glm::dmat3( 1.0,  0.0, 0.0,
                                        0.0,  cos, sin, 
                                        0.0, -sin, cos );
            break;

        case 2:
            if (posDirection) 
                rotMatrix = glm::dmat3( cos, 0.0, sin, 
                                        0.0, 1.0, 0.0, 
                                       -sin, 0.0, cos );
            else 
                rotMatrix = glm::dmat3( cos, 0.0, -sin, 
                                        0.0, 1.0,  0.0, 
                                        sin, 0.0,  cos ); 
            break;

        case 3:    
            if (posDirection) 
                rotMatrix = glm::dmat3( cos, -sin, 0.0, 
                                        sin,  cos, 0.0, 
                                        0.0,  0.0, 1.0 );  
            else 
                rotMatrix = glm::dmat3(  cos, sin, 0.0, 
                                        -sin, cos, 0.0, 
                                         0.0, 0.0, 1.0 ); 
            break;
    }

    return rotMatrix;
}

} // namespace openspace
