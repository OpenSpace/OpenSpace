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

#ifndef __OPENSPACE_MODULE_DSN___RADECTRANSLATION___H__
#define __OPENSPACE_MODULE_DSN___RADECTRANSLATION___H__

#include <openspace/scene/translation.h>
#include <openspace/util/spicemanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/properties/vector/dvec3property.h>
#include <modules/dsn/managers/radecmanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/scene/scene.h>
#include <openspace/engine/globals.h>

namespace openspace {

struct UpdateData;

namespace documentation { struct Documentation; }

class RadecTranslation : public Translation {
public:

    RadecTranslation();
    RadecTranslation(const ghoul::Dictionary& dictionary);

    void extractData(std::unique_ptr<ghoul::Dictionary> &dictionary);
    glm::dvec3 position(const UpdateData& data) const override;
    static documentation::Documentation Documentation();
 
private:
    double _firstTimeInData = 0;
    double _lastTimeInData = 0;

    RadecManager radecManager;
    ///Converts the Ra Dec range coordinates into cartesian coordinates
    glm::dvec3 convertRaDecRangeToCartesian(double ra, double dec, double range) const;
    ///Transforms the cartesian coordinates with a rotation and a translation
    glm::dvec3 radecToCartesianCoordinates(glm::vec3 pos) const;
    ///Translated position
    mutable glm::dvec3 _position = {0.0,0.0,0.0};
    ///Determines how many minutes between updates in positioning data
    properties::FloatProperty _updateFrequency;

    glm::dmat4 _rotEquatorialSphere = { -0.05487554,  0.4941095, -0.8676661, 0.0,
            -0.8734371 , -0.4448296, -0.1980764, 0.0,
            -0.483835  ,  0.7469823,  0.4559838, 0.0,
             0.0       ,  0.0      ,  0.0      , 1.0
    };

};
} // namespace openspace

#endif 
