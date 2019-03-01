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
    constexpr openspace::properties::Property::PropertyInfo ObjectIdentifierInfo = {
        "ObjectIdentifier",
        "Object Identifier",
        "Identifier of the object that this translation is applied to."
    };
    constexpr openspace::properties::Property::PropertyInfo UpdateFrequencyInfo = {
        "UpdateFrequency",
        "Update Frequency",
        "Determines how many minutes between positioning data reload. "
    };
} // namespace

namespace openspace {

constexpr const char* _loggerCat = "RadecTranslation";

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
                ObjectIdentifierInfo.identifier,
                new StringVerifier,
                Optional::No,
                ObjectIdentifierInfo.description
            },
            {
                UpdateFrequencyInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                UpdateFrequencyInfo.description
            }
        }
    };
}

RadecTranslation::RadecTranslation()
    : _updateFrequency(
        UpdateFrequencyInfo, 1.f, 1.f, 100.f
    )
{
    addProperty(_updateFrequency);

    _updateFrequency.onChange([this]() {
        requireUpdate();
        notifyObservers();
        radecManager.setUpdateFrequency(_updateFrequency);
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
    std::unique_ptr<ghoul::Dictionary> dictionaryPtr = std::make_unique<ghoul::Dictionary>(dictionary);
  
    if (dictionary.hasKey(UpdateFrequencyInfo.identifier)) {
        _updateFrequency = dictionary.value<float>(UpdateFrequencyInfo.identifier);
    }

    extractData(dictionaryPtr);
}

void RadecTranslation::extractData(std::unique_ptr<ghoul::Dictionary> &dictionary){
    constexpr const char* _identifier = "RadecTranslation";

    if (!radecManager.extractMandatoryInfoFromDictionary(_identifier, dictionary)) {
        LERROR(fmt::format("{}: Did not manage to extract data for {}.", _identifier, radecManager.objectIdentifier.c_str()));
    }
    else {
        LDEBUG(fmt::format("{}: Successfully read data for {}.", _identifier, radecManager.objectIdentifier.c_str()));
        _firstTimeInData = radecManager.timeDoubles.front();
        _lastTimeInData = radecManager.timeDoubles.back();
    }
}

glm::dvec3 RadecTranslation::convertRaDecRangeToCartesian(double ra, double dec, double range) const{
    ra = glm::radians(ra);
    dec = glm::radians(dec);
    range *= 1000; //convert to meters

    glm::dvec3 raDecPos = SpiceManager::getPositionFromRaDecRange(ra, dec, range);
   
    return raDecPos;
}

glm::dvec3 RadecTranslation::radecToCartesianCoordinates(glm::vec3 pos) const {

    // get the Earth relative cartesian coordinates
    // expressed in the equatorial sphere coordinate system
    glm::dvec3 cartesianPos = convertRaDecRangeToCartesian(pos.x, pos.y, pos.z);
    // get Earth and make sure it has been placed in OpenSpace 
    glm::dvec3 earthPos = global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
    glm::dmat4 translationMatrixEarth = glm::translate( glm::dmat4(1.0), glm::dvec3(earthPos) );
    // calculate the cartesian world coordinates
    glm::dvec4 nodePos =  translationMatrixEarth * _rotEquatorialSphere * glm::vec4{ cartesianPos, 1.0 };
    glm::dvec3 worldposition = { nodePos.x, nodePos.y, nodePos.z };

    return worldposition;
}

glm::dvec3 RadecTranslation::position(const UpdateData& data) const{

    double time = data.time.j2000Seconds();
    const bool haveDataForTime = (time >= _firstTimeInData) && (time < _lastTimeInData);

    if (haveDataForTime) {
        glm::dvec3 radecPos = radecManager.getPosForTime(time);
        if (radecManager.isReady) {
            _position = radecToCartesianCoordinates(radecPos);
        }
    }
    else if(time < _firstTimeInData){
        glm::dvec3 radecPos = radecManager.getPosForTime(_firstTimeInData);
        if (radecManager.isReady) {
            _position = radecToCartesianCoordinates(radecPos);
        }
    }
    else { 
        glm::dvec3 radecPos = radecManager.getPosForTime(_lastTimeInData);
        if (radecManager.isReady) {
            _position = radecToCartesianCoordinates(radecPos);
        }
    }
    return _position;

}
} // namespace openspace
