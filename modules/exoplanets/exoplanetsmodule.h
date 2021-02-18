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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSMODULE___H__
#define __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/stringproperty.h>

namespace openspace {

class ExoplanetsModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Exoplanets";

    ExoplanetsModule();
    virtual ~ExoplanetsModule() = default;

    std::string exoplanetsDataPath() const;
    std::string lookUpTablePath() const;
    std::string starTexturePath() const;
    std::string noDataTexturePath() const;
    std::string orbitDiscTexturePath() const;
    std::string habitableZoneTexturePath() const;
    bool showComparisonCircle() const;
    bool showHabitableZone() const;
    bool useOptimisticZone() const;

    scripting::LuaLibrary luaLibrary() const override;
    std::vector<documentation::Documentation> documentations() const override;

protected:
    void internalInitialize(const ghoul::Dictionary& dict) override;

    properties::StringProperty _exoplanetsDataFolder;
    properties::StringProperty _starTexturePath;
    properties::StringProperty _noDataTexturePath;
    properties::StringProperty _orbitDiscTexturePath;
    properties::StringProperty _habitableZoneTexturePath;
    properties::BoolProperty _showComparisonCircle;
    properties::BoolProperty _showHabitableZone;
    properties::BoolProperty _useOptimisticZone;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSMODULE___H__
