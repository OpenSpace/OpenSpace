/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including withovut limitation the rights to use, copy, modify,    *
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

#ifndef __OPENSPACE_MODULE_KAMELEONVOLUME___KAMELEONVOLUMEREADER___H__
#define __OPENSPACE_MODULE_KAMELEONVOLUME___KAMELEONVOLUMEREADER___H__

#include <string>
#include <memory>
#include <modules/volume/rawvolume.h>
#include <ghoul/misc/dictionary.h>

#include <ccmc/Kameleon.h>

namespace ccmc {
    class Model;
    class Interpolator;
}

namespace openspace {
namespace kameleonvolume {

class KameleonVolumeReader {
public:
    KameleonVolumeReader(const std::string& path);

    std::unique_ptr<volume::RawVolume<float>> readFloatVolume(
        const glm::uvec3& dimensions,
        const std::string& variable,
        const glm::vec3& lowerBound,
        const glm::vec3& upperBound) const;

    std::unique_ptr<volume::RawVolume<float>> readFloatVolume(
        const glm::uvec3& dimensions,
        const std::string& variable,
        const glm::vec3& lowerBound,
        const glm::vec3& upperBound,
        float& newMinValue,
        float& newMaxValue) const;

    std::unique_ptr<volume::RawVolume<glm::vec3>> readVec3Volume(
        const glm::uvec3& dimensions,
        const std::vector<std::string> & variables,
        const glm::vec3& lowerBound,
        const glm::vec3& upperBound) const;

    std::unique_ptr<volume::RawVolume<glm::vec3>> readVec3Volume(
        const glm::uvec3& dimensions,
        const std::vector<std::string> & variables,
        const glm::vec3& lowerBound,
        const glm::vec3& upperBound,
        glm::vec3& newMinValue,
        glm::vec3& newMaxValue) const;

    ghoul::Dictionary readMetaData() const;

    std::string time() const;
    std::string simulationStart() const;
    std::string simulationEnd() const;
    float elapsedTime() const;

    double minValue(const std::string& variable) const;
    double maxValue(const std::string& variable) const;

    std::vector<std::string> gridVariableNames() const;

    std::vector<std::string> gridUnits() const; // DOESN'T EXIST!
    std::vector<std::string> variableNames() const;
    std::vector<std::string> variableAttributeNames() const;
    std::vector<std::string> globalAttributeNames() const;

    ccmc::Kameleon* getKameleon();

private:
    static void addAttributeToDictionary(ghoul::Dictionary& dictionary, const std::string& key, ccmc::Attribute& attr);
    std::string _path;
    ccmc::Kameleon _kameleon;
    ccmc::Model* _model;
    std::unique_ptr<ccmc::Interpolator> _interpolator;
    ccmc::Interpolator* _kameleonInterpolator;
};

} // namespace kameleonvolume
} // namespace openspace

#endif // __OPENSPACE_MODULE_KAMELEONVOLUME___KAMELEONVOLUMEREADER___H__
