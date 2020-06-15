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
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicensewriter.h>
#include <openspace/scene/sceneinitializer.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <string>
#include <stack>
#include <optional>

#include "profile_lua.inl"

namespace openspace {

namespace {
    constexpr const char* _loggerCat = "Profile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";
    
    const std::map<Profile::AssetEventType, std::string> AssetEventTypeString{
        { Profile::AssetEventType::Add, "add" },
        { Profile::AssetEventType::Require, "required" },
        { Profile::AssetEventType::Request, "requested" },
        { Profile::AssetEventType::Remove,  "removed" },
        { Profile::AssetEventType::Ignore,  "ignored" }
    };

    void handleChangedAdd(std::vector<Profile::AssetEvent>& base, unsigned int changedIdx,
                          std::vector<Profile::AssetEvent>& changed, std::string asset)
    {
        // @TODO:  Replace the next for loop with std::any_of or std::all_of

        bool addThisAsset = true;
        // Check base profile to see if has already been added there
        for (const Profile::AssetEvent& b : base) {
            if (b.name == asset) {
                if (b.eventType == Profile::AssetEventType::Require
                    || b.eventType == Profile::AssetEventType::Request)
                {
                    addThisAsset = false;
                    break;
                }
            }
        }

        // Check changed asset commands only prior to this one to see if already added
        for (unsigned int i = 0; i < changedIdx; i++) {
            if (changed[i].name == asset) {
                addThisAsset = false;
                break;
            }
        }

        if (addThisAsset) {
            Profile::AssetEvent ae = {
                std::move(asset),
                Profile::AssetEventType::Request
            };
            base.push_back(ae);
        }
    }

    void handleChangedRemove(std::vector<Profile::AssetEvent>& base, std::string asset) {
        base.push_back({ std::move(asset), Profile::AssetEventType::Remove });
    }

    void addAssetsToProfileFile(ProfileFile& pf,
                                const std::vector<Profile::AssetEvent>& allAssets)
    {
        pf.clearAssets();
        for (Profile::AssetEvent a : allAssets) {
            if (a.eventType != Profile::AssetEventType::Ignore) {
                std::string entry =
                    a.name + "\t" + AssetEventTypeString.at(a.eventType);
                pf.addAssetLine(entry);
            }
        }
    }

    std::string recurseForFullName(properties::PropertyOwner* po) {
        if (po == nullptr) {
            return "";
        }
        std::string name = recurseForFullName(po->owner()) + po->identifier();
        if (!name.empty()) {
            return name + ".";
        }
        else {
            return "";
        }
    }

    void checkForChangedProps(std::vector<properties::Property*>& changedList,
                              properties::PropertyOwner* po)
    {
        if (po) {
            for (properties::PropertyOwner* subOwner : po->propertySubOwners()) {
                checkForChangedProps(changedList, subOwner);
            }
            for (properties::Property* p : po->properties()) {
                if (p->hasChanged()) {
                    changedList.push_back(p);
                }
            }
        }
    }

} // namespace

void Profile::saveCurrentSettingsToProfile(const std::string& filename) {
    ProfileFile pf = collateBaseWithChanges();
    pf.writeToFile(filename);
}

std::string Profile::saveCurrentSettingsToProfile_string() {
    ProfileFile pf = collateBaseWithChanges();
    return pf.writeToString();
}

bool Profile::usingProfile() const {
    return global::configuration.usingProfile;
}

std::string Profile::initialProfile() const {
    return global::configuration.profile;
}

std::vector<Profile::AssetEvent> Profile::assetEvents() const {
    return global::openSpaceEngine.assetEvents();
}

std::string Profile::profileBaseDirectory() const {
    return _profileBaseDirectory;
}

ProfileFile Profile::collateBaseWithChanges() {
    if (!usingProfile()) {
        std::string errorMessage = "Program was not started using a profile, "
            "so cannot use this save-current-settings feature";
        LERROR(errorMessage);
    }
    std::string initProfile = initialProfile();
    std::string inputProfilePath = absPath(_profileBaseDirectory) + "/" + initProfile
        + ".profile";
    ProfileFile pf(inputProfilePath);
    pf.setVersion(FormatVersion);
    std::vector<AssetEvent> ass = modifyAssetsToReflectChanges(pf);
    addAssetsToProfileFile(pf, ass);
    modifyPropertiesToReflectChanges(pf);

    // add current time to profile file
    std::string t = currentTimeUTC();
    std::string update = "absolute\t" + t;
    pf.updateTime(update);
    
    addCurrentCameraToProfileFile(pf);
    return pf;
}

std::vector<Profile::AssetEvent> Profile::modifyAssetsToReflectChanges(ProfileFile& pf) {
    std::vector<AssetEvent> a;
    parseAssetFileLines(a, pf);
    AllAssetDetails assetDetails;

    assetDetails.base = a;
    assetDetails.changed = assetEvents();

    for (unsigned int i = 0; i < assetDetails.changed.size(); i++) {
        AssetEvent event = assetDetails.changed[i];

        if (event.eventType == AssetEventType::Add) {
            handleChangedAdd(assetDetails.base, i, assetDetails.changed, event.name);
        }
        else if (event.eventType == AssetEventType::Remove) {
            handleChangedRemove(assetDetails.base, event.name);
        }
    }
    return assetDetails.base;
}

void Profile::parseAssetFileLines(std::vector<AssetEvent>& results, ProfileFile& pf) {
    AssetEvent a;

    for (std::string line : pf.assets()) {
        std::vector<std::string> elements = ghoul::tokenizeString(line, '\t');

        if (elements[0].empty()) {
            LERROR(fmt::format(
                "Error parsing profile line '{}'. Asset name is needed (field 1/2)",
                line
            ));
        }
        else {
            a.name = elements[0];
        }

        if (elements[1] == AssetEventTypeString.at(AssetEventType::Require)) {
            a.eventType = AssetEventType::Require;
        }
        else if (elements[1] == AssetEventTypeString.at(AssetEventType::Request)) {
            a.eventType = AssetEventType::Request;
        }
        else if (elements[1] == "") {
            a.eventType = AssetEventType::Request;
        }
        else {
            LERROR(fmt::format(
                "Error parsing profile line '{}'. Invalid required param (field 2/2)",
                line
            ));
        }

        results.push_back(a);
    }
}

void Profile::modifyPropertiesToReflectChanges(ProfileFile& pf) {
    std::vector<std::string> formatted = changedPropertiesFormatted();

    for (std::string line : formatted) {
        pf.addPropertyLine(std::move(line));
    }
}

std::vector<std::string> Profile::changedPropertiesFormatted() {
    std::vector<properties::Property*> changedProps = changedProperties();
    std::vector<std::string> formattedLines;

    for (properties::Property* prop : changedProps) {
        std::string newLine = "setPropertyValueSingle\t";
        newLine += getFullPropertyPath(prop) + "\t";
        newLine += prop->getStringValue();
        formattedLines.push_back(newLine);
    }
    return formattedLines;
}

std::string Profile::getFullPropertyPath(properties::Property* prop) {
    return recurseForFullName(prop->owner()) + prop->identifier();
}

std::vector<properties::Property*> Profile::changedProperties() {
    ZoneScoped

    std::vector<SceneGraphNode*> nodes 
        = global::renderEngine.scene()->allSceneGraphNodes();
    std::vector<properties::Property*> changedProps;

    for (SceneGraphNode* n : nodes) {
        checkForChangedProps(changedProps, n);
    }
    return changedProps;
}

std::string Profile::currentTimeUTC() const {
    return global::timeManager.time().ISO8601();
}

interaction::NavigationHandler::NavigationState Profile::currentCameraState() const {
    return global::navigationHandler.navigationState();
}

void Profile::addCurrentCameraToProfileFile(ProfileFile& pf) const {
    std::string update = "setNavigationState\t";
    interaction::NavigationHandler::NavigationState nav;
    nav = currentCameraState();
    update += "\"" + nav.anchor + "\"\t";
    update += "\"" + nav.aim + "\"\t";
    update += "\"" + nav.referenceFrame + "\"\t";
    update += std::to_string(nav.position.x) + ",";
    update += std::to_string(nav.position.y) + ",";
    update += std::to_string(nav.position.z) + "\t";
    if (nav.up.has_value()) {
        glm::dvec3 u = nav.up.value();
        //glm::dvec3 u = static_cast<glm::dvec3>(nav.up.value());
        update += std::to_string(u.x) + ",";
        update += std::to_string(u.y) + ",";
        update += std::to_string(u.z);
    }
    update += "\t";
    update += std::to_string(nav.yaw) + "\t";
    update += std::to_string(nav.pitch);

    pf.updateCamera(update);
}

void Profile::convertToSceneFile(const std::string& inProfilePath,
                                 const std::string& outFilePath)
{
    ZoneScoped

    ProfileFile pf(inProfilePath);

    std::ofstream outFile;
    try {
        outFile.open(outFilePath, std::ofstream::out);
    }
    catch (const std::ofstream::failure& e) {
        LERROR("Exception opening scene file for write: " + outFilePath
            + " (" + e.what() + ")");
    }

    try {
        outFile << convertToScene(pf);
    }
    catch (const std::ofstream::failure& e) {
        LERROR("Data write error to scene file: " + outFilePath
            + " (" + e.what() + ")");
    }

    try {
        outFile.close();
    }
    catch (const std::ofstream::failure& e) {
        LERROR("Exception closing scene file after write: " + outFilePath
            + " (" + e.what() + ")");
    }
}

std::string Profile::convertToScene(ProfileFile& pf) {
    ZoneScoped

    std::string result;

    result += convertToScene_modules(pf) + "\n";
    result += convertToScene_assets(pf) + "\n";
    result += convertToScene_keybindings(pf) + "\n";
    result += "asset.onInitialize(function ()\n";
    result += convertToScene_time(pf) + "\n";
    result += "  sceneHelper.bindKeys(Keybindings)\n\n";
    result += convertToScene_markNodes(pf) + "\n";
    result += convertToScene_properties(pf) + "\n";
    result += convertToScene_camera(pf);
    result += "end)\n";

    return result;
}

std::string Profile::convertToScene_modules(ProfileFile& pf) {
    std::string result;

    for (std::string m : pf.modules()) {
        std::vector<std::string> fields = ghoul::tokenizeString(m, '\t');
        if (!fields[moduleFieldLoaded].empty() && !fields[moduleFieldNotLoaded].empty()) {
            result += fmt::format(
                "if openspace.modules.isLoaded(\"{}\") then {} else {} end\n",
                fields[moduleFieldName], fields[moduleFieldLoaded], fields[moduleFieldNotLoaded]
            );
        }
        else if (fields[moduleFieldNotLoaded].empty()) {
            result += fmt::format(
                "if not openspace.modules.isLoaded(\"{}\") then {} end\n",
                fields[moduleFieldName], fields[moduleFieldNotLoaded]
            );
        }
        else if (fields[moduleFieldLoaded].empty()) {
            result += fmt::format(
                "if openspace.modules.isLoaded(\"{}\") then {} end\n",
                fields[moduleFieldName], fields[moduleFieldLoaded]
            );
        }
    }
    return result;
}

std::string Profile::convertToScene_assets(ProfileFile& pf) {
    std::string result;
    std::string assetR;

    result += "asset.require(\"base\");\n";
    result += "local assetHelper = asset.require(\"util/asset_helper\")\n";
    result += "local propertyHelper = asset.require(\"util/property_helper\")\n";
    result += "local sceneHelper = asset.require(\"util/scene_helper\")\n";
    result += "local renderableHelper = asset.require(\"util/renderable_helper\")\n";

    for (size_t i = 0; i < pf.assets().size(); ++i) {
        std::vector<std::string> fields = ghoul::tokenizeString(pf.assets()[i], '\t');

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
            std::string err = fmt::format(
                "Asset {} of {} has bad arg 2/2 which must be 'required' or 'requested'",
                i + 1, pf.assets().size()
            );
            throw ghoul::RuntimeError(err);
        }
        result += fmt::format("asset.{}(\"{}\")\n", assetR, fields[assetFieldName]);
    }
    return result;
}

std::string Profile::convertToScene_properties(ProfileFile& pf) {
    std::string result;

    for (size_t i = 0; i < pf.properties().size(); ++i) {
        std::vector<std::string> fields = ghoul::tokenizeString(pf.properties()[i], '\t');

        if (fields[propertyFieldType] != "setPropertyValue"
            && fields[propertyFieldType] != "setPropertyValueSingle")
        {
            std::string err = fmt::format(
                "Property {} of {} has bad arg 1/1 which must be "
                "'setPropertyValue' or 'setPropertyValueSingle'",
                i + 1, pf.properties().size()
            );
            throw ghoul::RuntimeError(err);
        }
        else {
            result += fmt::format(
                "  openspace.{}(\"{}\", {})\n",
                fields[propertyFieldType], fields[propertyFieldName], fields[propertyFieldValue]
            );
        }
    }
    return result;
}

std::string Profile::convertToScene_keybindings(ProfileFile& pf) {
    std::string result;

    result += "local Keybindings = {\n";
    for (size_t i = 0; i < pf.keybindings().size(); ++i) {
        std::vector<std::string> fields = ghoul::tokenizeString(pf.keybindings()[i], '\t');

        result += "  {\n";
        result += fmt::format("    {} = \"{}\",\n", "Key", fields[0]);
        result += fmt::format("    {} = \"{}\",\n", "Documentation", fields[1]);
        result += fmt::format("    {} = \"{}\",\n", "Name", fields[2]);
        result += fmt::format("    {} = \"{}\",\n", "GuiPath", fields[3]);
        result += fmt::format("    {} = \"{}\",\n", "Local", fields[4]);
        result += fmt::format("    {} = \"{}\"\n", "Command", fields[5]);
        result += "  },\n";
    }
    result += "}\n";
    return result;
}

std::string Profile::convertToScene_markNodes(ProfileFile& pf) {
    std::string result;

    if (!pf.markNodes().empty()) {
        result += "  openspace.markInterestingNodes({";
        for (const std::string& m : pf.markNodes()) {
            result += fmt::format("\"{}\",", m);
        }
        result += "})\n";
    }
    return result;
}

std::string Profile::convertToScene_time(ProfileFile& pf) {
    std::string result;

    std::vector<std::string> fields = ghoul::tokenizeString(pf.time(), '\t');
    if (fields[timeFieldType] == "absolute") {
        result += fmt::format("  openspace.time.setTime(\"{}\")\n", fields[timeFieldSet]);
    }
    else if (fields[timeFieldType] == "relative") {
        result += fmt::format(
            "  openspace.time.setTime(openspace.time.advancedTime("
            "openspace.time.currentWallTime(), \"{}\"))\n",
            fields[timeFieldSet]
        );
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

std::string Profile::convertToScene_camera(ProfileFile& pf) {
    std::string result;

    std::vector<std::string> fields = ghoul::tokenizeString(pf.camera(), '\t');
    if (fields[cameraFieldType] == "setNavigationState") {
        result += "  openspace.navigation.setNavigationState({";
        result += fmt::format("Anchor = {}, ", fields[cameraNavigationFieldAnchor]);
        if (!fields[cameraNavigationFieldAim].empty()) {
            result += fmt::format("Aim = {}, ", fields[cameraNavigationFieldAim]);
        }
        if (!fields[cameraNavigationFieldRef].empty()) {
            result += fmt::format("ReferenceFrame = {}, ", fields[cameraNavigationFieldRef]);
        }
        result += fmt::format("Position = {{ {} }}, ", fields[cameraNavigationFieldPosition]);
        if (!fields[cameraNavigationFieldUp].empty()) {
            result += fmt::format("Up = {{ {} }}, ", fields[cameraNavigationFieldUp]);
        }
        if (!fields[cameraNavigationFieldYaw].empty()) {
            result += fmt::format("Yaw = {}, ", fields[cameraNavigationFieldYaw]);
        }
        if (!fields[cameraNavigationFieldPitch].empty()) {
            result += fmt::format("Pitch = {} ", fields[cameraNavigationFieldPitch]);
        }
        result += "})\n";
    }
    else if (fields[cameraFieldType] == "goToGeo") {
        result += "  openspace.globebrowsing.goToGeo(";
        if (!fields[cameraGeoFieldAnchor].empty()) {
            result += fields[cameraGeoFieldAnchor] + ", ";
        }
        result += fields[cameraGeoFieldLatitude] + ", ";
        result += fields[cameraGeoFieldLongitude];
        if (!fields[cameraGeoFieldAltitude].empty()) {
            result += + ", " + fields[cameraGeoFieldAltitude];
        }
        result += ")\n";
    }
    else {
        throw ghoul::RuntimeError(
            "Camera entry's arg 1/1 must be either 'setNavigationState' or 'goToGeo'"
        );
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
