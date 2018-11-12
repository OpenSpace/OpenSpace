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

#include <modules/dsn/translation/radectranslation.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo PositionInfo = {
        "Position",
        "Position",
        "Write some documentaion here!"
    };
} // namespace

namespace openspace {

documentation::Documentation RadecTranslation::Documentation() {
    using namespace documentation;
    return {
        "Radec Translation",
        "transform_translation_radec",
        {
            {
                "Type",
                new StringEqualVerifier("RadecTranslation"),
                Optional::No
            },
            {
                PositionInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                PositionInfo.description
            }
        }
    };
}


RadecTranslation::RadecTranslation()
    : _position(
        PositionInfo,
        glm::dvec3(0.0),
        glm::dvec3(-std::numeric_limits<double>::max()),
        glm::dvec3(std::numeric_limits<double>::max())
    )
{   

  
    addProperty(_position);

    _position.onChange([this]() {
        requireUpdate();
        notifyObservers();
    });
}

RadecTranslation::RadecTranslation(const ghoul::Dictionary& dictionary)
    : RadecTranslation()
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RadecTranslation"
    );
}

glm::dvec3 RadecTranslation::convertRaDecRangeToCartesian() const{
    //Todo: stream data from file
    //Static data for voyager 1
    double ra = 257.777029167736; //2018-246
    double dec = 12.2537708651048; // 2018-246
    double range = 2.14044781771236e+13;

    //Convert RA and DEC from degrees to radians 
    ra = glm::radians(ra);
    dec = glm::radians(dec);

    //Save array in vector 
    glm::dvec3 raDecPos = SpiceManager::getPositionFromRaDecRange(ra, dec, range);
   
    return raDecPos;
}

glm::dvec3 RadecTranslation::transformCartesianCoordinates() const {

    glm::vec3 pos = convertRaDecRangeToCartesian();

    glm::dvec3 earthPos = global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
    glm::dmat4 translationMatrixEarth = glm::translate( glm::dmat4(1.0), glm::dvec3(earthPos) );

    glm::dvec4 newPos = { pos, 1.0 };
    glm::dvec4 nodePos =  translationMatrixEarth * _rotEquatorialSphere * newPos;
    glm::dvec3 worldposition = { nodePos.x, nodePos.y, nodePos.z };

    return worldposition;
}

glm::dvec3 RadecTranslation::position(const UpdateData&) const{

    glm::dvec3 _position = transformCartesianCoordinates();
    return _position;
}

} // namespace openspace
