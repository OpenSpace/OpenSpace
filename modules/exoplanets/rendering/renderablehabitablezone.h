/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___RENDERABLEHABITABLEZONE___H__
#define __OPENSPACE_MODULE_EXOPLANETS___RENDERABLEHABITABLEZONE___H__

#include <modules/base/rendering/renderabledisc.h>
#include <openspace/properties/scalar/floatproperty.h>

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableHabitableZone : public RenderableDisc {
public:
    RenderableHabitableZone(const ghoul::Dictionary& dictionary);

    static documentation::Documentation Documentation();

private:
    void computeZone();

    /**
     * Compute the inner and outer boundary of the habitable zone of a star, accordring to
     * formula and coefficients by Kopparapu et al. (2015) https://arxiv.org/abs/1404.5292
     *
     * \param teff The effective temperature of the star, in Kelvin
     * \param luminosity The luminosity of the star, in solar luminosities
     * \return A vec2 with the lower and upper boundary in atronomical units
     */
    glm::vec2 computeKopparapuZoneBoundaries(float teff, float luminosity);

    properties::FloatProperty _teff;
    properties::FloatProperty _luminosity;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_EXOPLANETS___RENDERABLEHABITABLEZONE___H__
