/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/scene/profile.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <string>
#include <stack>

#include "profile_lua.inl"

namespace {
    constexpr const char* _loggerCat = "Profile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";

} // namespace

namespace openspace {

void Profile::saveCurrentSettingsToProfile(std::string filename) {
    if (! global::configuration.usingProfile) {
        std::string errorMessage = "Program was not started using a profile, "
            "so cannot use this save-current-settings feature.";
        LERROR(errorMessage);
    }
    std::string initProfile = global::configuration.profile;
}

std::string Profile::convertToAsset(ProfileFile& pf) {
    std::string result;

    result += convertToAsset_modules(pf) + "\n";
    result += convertToAsset_assets(pf) + "\n";
    result += convertToAsset_keybindings(pf) + "\n";
    result += "asset.onInitialize(function ()\n";
    result += convertToAsset_time(pf) + "\n";
    result += "  sceneHelper.bindKeys(Keybindings)\n\n";
    result += convertToAsset_markNodes(pf) + "\n";
    result += convertToAsset_properties(pf) + "\n";
    result += convertToAsset_camera(pf);
    result += "end)\n";

    return result;
}

std::string Profile::convertToAsset_modules(ProfileFile& pf) {
    std::string result;
    std::vector<std::string> fields;

    for (std::string m : pf.modules()) {
        pf.splitByTab(m, fields);
        if (fields[moduleFieldLoaded] != "" && fields[moduleFieldNotLoaded] != "") {
            result += "if openspace.modules.isLoaded(\"" + fields[moduleFieldName];
            result += "\") then\n    " + fields[moduleFieldLoaded] + "\nelse\n";
            result += "   " + fields[moduleFieldNotLoaded] + "\nend\n";
        }
        else if (fields[moduleFieldNotLoaded] == "") {
            result += "if not openspace.modules.isLoaded(\"" + fields[moduleFieldName];
            result += "\") then\n    " + fields[moduleFieldNotLoaded] + "\nend\n";
        }
        else if (fields[moduleFieldLoaded] == "") {
            result += "if openspace.modules.isLoaded(\"" + fields[moduleFieldName];
            result += "\") then\n    " + fields[moduleFieldLoaded] + "\nend\n";
        }
    }
    return result;
}

std::string Profile::convertToAsset_assets(ProfileFile& pf) {
    std::string result;
    std::vector<std::string> fields;
    std::string assetR;

    result += "asset.require(\"base\");\n";
    result += "local assetHelper = asset.require(\"util/asset_helper\")\n";
    result += "local propertyHelper = asset.require(\"util/property_helper\")\n";
    result += "local sceneHelper = asset.require(\"util/scene_helper\")\n";
    result += "local renderableHelper = asset.require(\"util/renderable_helper\")\n";

    for (size_t i = 0; i < pf.assets().size(); ++i) {
        std::string a = pf.assets()[i];
        pf.splitByTab(a, fields);

        if (fields[assetFieldReqd] == "required") {
            assetR = "require";
        }
        else if (fields[assetFieldReqd] == "requested") {
            assetR = "request";
        }
        else if (fields[assetFieldReqd] == "") {
            assetR = "require";
        }
        else {
            std::string err = "Asset " + std::to_string(i + 1) + " of ";
            err += std::to_string(pf.assets().size()) + " has bad arg 2/2 which must ";
            err += "be either 'required' or 'requested'";
            throw ghoul::RuntimeError(err);
        }
        result += "asset." + assetR + "(\"" + fields[assetFieldName] + "\")\n";
    }
    return result;
}

std::string Profile::convertToAsset_properties(ProfileFile& pf) {
    std::string result;
    std::vector<std::string> fields;

    for (size_t i = 0; i < pf.properties().size(); ++i) {
        std::string p = pf.properties()[i];
        pf.splitByTab(p, fields);

        if (fields[propertyFieldType] != "setPropertyValue"
            && fields[propertyFieldType] != "setPropertyValueSingle")
        {
            std::string err = "Property" + std::to_string(i + 1) + " of ";
            err += std::to_string(pf.properties().size()) + " has bad arg 1/1 which ";
            err += "must be either 'setPropertyValue' or 'setPropertyValueSingle'";
            throw ghoul::RuntimeError(err);
        }
        else {
            result += "  openspace." + fields[propertyFieldType] + "(\""
                + fields[propertyFieldName] + "\", " + fields[propertyFieldValue] + ")\n";
        }
    }
    return result;
}

std::string Profile::convertToAsset_keybindings(ProfileFile& pf) {
    std::string result;
    std::vector<std::string> fields;
    std::string assetR;

    result += "local Keybindings = {\n";
    for (size_t i = 0; i < pf.keybindings().size(); ++i) {
        std::string k = pf.keybindings()[i];
        pf.splitByTab(k, fields);

        result += "  {\n";
        result += "    Key = \"" + fields[0] + "\",\n";
        result += "    Documentation = \"" + fields[1] + "\",\n";
        result += "    Name = \"" + fields[2] + "\",\n";
        result += "    GuiPath = \"" + fields[3] + "\",\n";
        result += "    Local = " + fields[4] + ",\n";
        result += "    Command = " + fields[5] + "\n";
        result += "  },\n";
    }
    result += "}\n";
    return result;
}

std::string Profile::convertToAsset_markNodes(ProfileFile& pf) {
    std::string result;

    if (pf.markNodes().size() > 0) {
        result += "  openspace.markInterestingNodes({";
    }
    for (std::string m : pf.markNodes()) {
        result += "\"" + m + "\", ";
    }
    result += "})\n";
    return result;
}

std::string Profile::convertToAsset_time(ProfileFile& pf) {
    std::string result;
    std::vector<std::string> fields;
    std::string assetR;

    pf.splitByTab(pf.time(), fields);

    if (fields[timeFieldType] == "absolute") {
        result += "  openspace.time.setTime(\"" + fields[timeFieldSet] + "\")\n";
    }
    else if (fields[timeFieldType] == "relative") {
        result += "  local now = openspace.time.currentWallTime(); ";
        result += "openspace.time.setTime(";
        result += "openspace.time.advancedTime(now, \"" + fields[timeFieldSet] + "\"))\n";
    }
    else {
        std::string err = "Time entry's arg 1/1 must be either 'absolute' or 'relative'";
        throw ghoul::RuntimeError(err);
    }
    return result;
}

std::string Profile::convertToAsset_camera(ProfileFile& pf) {
    std::string result;
    std::vector<std::string> fields;
    std::string assetR;

    pf.splitByTab(pf.camera(), fields);

    if (fields[cameraFieldType] == "setNavigationState") {
        result += "  openspace.navigation.setNavigationState({";
        result += "Anchor = " + fields[cameraNavigationFieldAnchor] + ", ";
        if (fields[cameraNavigationFieldAim] != "") {
            result += "Aim = " + fields[cameraNavigationFieldAim] + ", ";
        }
        if (fields[cameraNavigationFieldRef] != "") {
            result += "ReferenceFrame = " + fields[cameraNavigationFieldRef] + ", ";
        }
        result += "Position = {" + fields[cameraNavigationFieldPosition] + "}, ";
        if (fields[cameraNavigationFieldUp] != "") {
            result += "Up = {" + fields[cameraNavigationFieldUp] + "}, ";
        }
        if (fields[cameraNavigationFieldYaw] != "") {
            result += "Yaw = " + fields[cameraNavigationFieldYaw] + ", ";
        }
        if (fields[cameraNavigationFieldPitch] != "") {
            result += "Pitch = " + fields[cameraNavigationFieldPitch] + " ";
        }
        result += "})\n";
    }
    else if (fields[cameraFieldType] == "goToGeo") {
        result += "  openspace.globebrowsing.goToGeo({ ";
        if (fields[cameraGeoFieldAnchor] != "") {
            result += fields[cameraGeoFieldAnchor] + ", ";
        }
        result += fields[cameraGeoFieldLatitude] + ", ";
        result += fields[cameraGeoFieldLongitude] + ", ";
        if (fields[cameraGeoFieldAltitude] != "") {
            result += fields[cameraGeoFieldAltitude] + ", ";
        }
        result += ")\n";
    }
    else {
        std::string err = "Camera entry's arg 1/1 must be either ";
        err += "'setNavigationState' or 'goToGeo'";
        throw ghoul::RuntimeError(err);
    }
    return result;
}

scripting::LuaLibrary Profile::luaLibrary() {
    return {
        "",
        {
            {
                "saveCurrentSettingsToProfile",
                &luascriptfunctions::saveCurrentSettingsToProfile,
                {},
                "string",
                "Collects all changes that have been made since startup, including all "
                "property changes and assets required, requested, or removed. All "
                "changes will be added to the profile that OpenSpace was started with, "
                "and the new saved file will contain all of this information. The "
                "file will be named according to the argument provided. If this argument "
                "is blank, then the new file will have the base name of the profile that "
                " was started, with a _YYYYMMDD suffix."
            }
        }
    };
}

}  // namespace openspace
