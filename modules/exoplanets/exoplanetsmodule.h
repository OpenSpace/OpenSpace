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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSMODULE___H__
#define __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSMODULE___H__

#include <modules/exoplanets/discoverymethods/discoverymethods.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/openspacemodule.h>

namespace openspace {
    struct Exoplanet {
        float A;
        double AUPPER;
        double ALOWER;
        double UA;
        float BIGOM;
        float BIGOMUPPER;
        float BIGOMLOWER;
        float UBIGOM;
        bool BINARY;
        float BMV;
        float ECC;
        float ECCUPPER;
        float ECCLOWER;
        float UECC;
        float I;
        float IUPPER;
        float ILOWER;
        float UI;
        int NCOMP;
        float OM;
        float OMUPPER;
        float OMLOWER;
        float UOM;
        double PER;
        float PERUPPER;
        float PERLOWER;
        float UPER;
        double R;
        double RUPPER;
        double RLOWER;
        double UR;
        float RSTAR;
        float RSTARUPPER;
        float RSTARLOWER;
        float URSTAR;
        double TT;
        float TTUPPER;
        float TTLOWER;
        float UTT;
        float POSITIONX;
        float POSITIONY;
        float POSITIONZ;
    };

class ExoplanetsModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Exoplanets";

    ExoplanetsModule();
    virtual ~ExoplanetsModule() = default;

    scripting::LuaLibrary luaLibrary() const override;

    std::vector<documentation::Documentation> documentations() const override;

    void setClosestExoplanet(Exoplanet);
    void setStarName(std::string);
    void setPlanetSystem(std::vector<Exoplanet>);
    void setPlanetNames(std::vector<std::string>);
    void setRotation(glm::dmat3);
    void setNorthVector(glm::dvec3);

    Exoplanet closestExoplanet();
    std::string getStarName();
    std::vector<Exoplanet> planetSystem();
    std::vector<std::string> planetNames();
    glm::dmat3 getRotation();
    glm::dvec3 getNorthVector();

protected:
    void internalInitialize(const ghoul::Dictionary&) override;
    std::unique_ptr<openspace::exoplanets::DiscoveryMethods> _discoveryMethods;

    Exoplanet _exo;
    std::string _starName;
    std::vector<Exoplanet> _planetSystem;
    std::vector<std::string> _planetNames;
    glm::dmat3 _rotation;
    glm::dvec3 _north;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSMODULE___H__
