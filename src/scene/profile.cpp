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
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <string>
#include <stack>
#include <optional>

#include "profile_lua.inl"

namespace {
    constexpr const char* _loggerCat = "Profile";
    constexpr const char* KeyIdentifier = "Identifier";
    constexpr const char* KeyParent = "Parent";
} // namespace

namespace openspace {

void Profile::saveCurrentSettingsToProfile(std::string filename) {
    ProfileFile pf = collateBaseWithChanges();
    pf.writeToFile(filename);
}

std::string Profile::saveCurrentSettingsToProfile_string() {
    ProfileFile pf = collateBaseWithChanges();
    return pf.writeToString();
}

bool Profile::usingProfile() {
    return global::configuration.usingProfile;
}

std::string Profile::initialProfile() {
    return global::configuration.profile;
}

ProfileFile Profile::collateBaseWithChanges() {
    if (! usingProfile()) {
        std::string errorMessage = "Program was not started using a profile, "
            "so cannot use this save-current-settings feature.";
        LERROR(errorMessage);
    }
    std::string initProfile = initialProfile();
    ProfileFile pf(initProfile);
    std::vector<AssetEvent> ass = modifyAssetsToReflectChanges(pf);
    addAssetsToProfileFile(ass, pf);
    modifyPropertiesToReflectChanges(pf);
    addCurrentTimeToProfileFile(pf);
    addCurrentCameraToProfileFile(pf);
    return pf;
}

std::vector<Profile::AssetEvent> Profile::modifyAssetsToReflectChanges(ProfileFile& pf) {
    std::vector<AssetEvent> a;
    parseAssetFileLines(a, pf);
    AllAssetDetails assetDetails;

    assetDetails.base = a;
    assetDetails.changed = global::openSpaceEngine.listOfAllAssetEvents();

    for (unsigned int i = 0; i < assetDetails.changed.size(); i++) {
        AssetEvent event = assetDetails.changed[i];

        if (event.eventType == AssetEventType::add) {
            handleChangedAdd(assetDetails.base, i, assetDetails.changed, event.name);
        }
        else if (event.eventType == AssetEventType::remove) {
            handleChangedRemove(assetDetails.base, i, assetDetails.changed, event.name);
        }
    }
    return assetDetails.base;
}

void Profile::handleChangedAdd(std::vector<AssetEvent>& base, unsigned int changedIdx,
                               std::vector<AssetEvent>& changed, std::string asset)
{
    bool addThisAsset = true;
    //Check base profile to see if has already been added there
    for (auto b : base) {
        if (b.name == asset ) {
            if (   b.eventType == AssetEventType::require
                || b.eventType == AssetEventType::request)
            {
                addThisAsset = false;
                break;
            }
        }
    }

    //Check changed asset commands only prior to this one to see if already added
    for (unsigned int i = 0; i < changedIdx; i++) {
        if (changed[i].name == asset) {
            addThisAsset = false;
            break;
        }
    }

    if (addThisAsset) {
        AssetEvent ae = {asset, AssetEventType::request};
        base.push_back(ae);
    }
}

void Profile::handleChangedRemove(std::vector<AssetEvent>& base, unsigned int changedIdx,
                                  std::vector<AssetEvent>& changed, std::string asset)
{
    //Blank-out any base profile entries where this asset was required/requested
    for (unsigned int i = 0; i < base.size(); i++) {
        if (base[i].name == asset) {
            base[i].name = "";
            base[i].eventType = AssetEventType::ignore;
        }
    }

    //Blank-out any changes profile entries where this asset was required/requested
    for (unsigned int i = 0; i < changedIdx; i++) {
        if (changed[i].name == asset) {
            changed[i].name = "";
            changed[i].eventType = AssetEventType::ignore;
        }
    }
}

void Profile::addAssetsToProfileFile(std::vector<AssetEvent>& allAssets, ProfileFile& pf)
{
    pf.clearAssets();
    for (AssetEvent a : allAssets) {
        std::string entry = a.name + "\t" + AssetEventTypeString.at(a.eventType);
        pf.addAssetLine(entry);
    }
}

void Profile::parseAssetFileLines(std::vector<AssetEvent>& results, ProfileFile& pf) {
    std::vector<std::string> elements;
    AssetEvent a;

    for (std::string line : pf.assets()) {
        pf.splitByTab(line, elements);

        if (a.name.empty()) {
            LERROR("Error in parsing asset file line '" + line
                + "'. Asset name is needed (field 1/2).");
        }
        else {
            a.name = elements[0];
        }

        if (elements[1] == AssetEventTypeString.at(AssetEventType::require)) {
            a.eventType = AssetEventType::require;
        }
        else if (elements[1] == AssetEventTypeString.at(AssetEventType::request)) {
            a.eventType = AssetEventType::request;
        }
        else if (elements[1] == "") {
            a.eventType = AssetEventType::request;
        }
        else {
            LERROR("Error in parsing asset file line '" + line
                + "'. Invalid required param (field 2/2).");
        }

        results.push_back(a);
    }
}

void Profile::modifyPropertiesToReflectChanges(ProfileFile& pf) {
    std::vector<std::string> formatted = getChangedPropertiesFormatted();

    for (std::string line: formatted) {
        pf.addPropertyLine(line);
    }
}

std::vector<std::string> Profile::getChangedPropertiesFormatted() {
    std::vector<openspace::properties::Property*> changedProps
        = getChangedProperties();
    std::vector<std::string> formattedLines;

    for (auto prop : changedProps) {
        std::string newLine = "setPropertyValueSingle\t";
        newLine += prop->identifier() + "\t";
        newLine += prop->getStringValue() + "\n";
        formattedLines.push_back(newLine);
    }
    return formattedLines;
}

std::vector<openspace::properties::Property*> Profile::getChangedProperties()
{
    ZoneScoped

    std::vector<SceneGraphNode*> nodes
        = global::renderEngine.scene()->allSceneGraphNodes();
    std::vector<openspace::properties::Property*> changedProps;

    for (auto n : nodes) {
        if (n != nullptr) {
            std::vector<openspace::properties::Property*> props = n->properties();
            for (auto p : props) {
                if (p->hasChanged()) {
                    changedProps.push_back(p);
                }
            }
        }
    }
    return changedProps;
}

std::string Profile::getCurrentTimeUTC() {
    return global::timeManager.time().UTC();
}

void Profile::addCurrentTimeToProfileFile(ProfileFile& pf) {
    std::string t = getCurrentTimeUTC();
    std::string update = "absolute\t" + t + "\n";
    pf.updateTime(update);
}

interaction::NavigationHandler::NavigationState Profile::getCurrentCameraState() {
    return global::navigationHandler.navigationState();
}

void Profile::addCurrentCameraToProfileFile(ProfileFile& pf) {
    std::string update = "setNavigationState\t";
    interaction::NavigationHandler::NavigationState nav;
    nav = getCurrentCameraState();
    update += nav.anchor + "\t";
    update += nav.aim + "\t";
    update += nav.referenceFrame + "\t";
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
    update += std::to_string(nav.pitch) + "\n";

    pf.updateCamera(update);
}

void Profile::convertToSceneFile(const std::string inProfilePath,
                                 const std::string outFilePath)
{
    ZoneScoped

    ProfileFile pf(inProfilePath);

    std::ofstream outFile;
    try {
        outFile.open(outFilePath, std::ofstream::out);
    }
    catch (std::ofstream::failure& e) {
        LERROR("Exception opening profile file for write: " + outFilePath);
    }

    try {
        outFile << convertToScene(pf);
    }
    catch (std::ofstream::failure& e) {
        LERROR("Data write error to file: " + outFilePath);
    }

    try {
        outFile.close();
    }
    catch (std::ofstream::failure& e) {
        LERROR("Exception closing profile file after write: " + outFilePath);
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

std::string Profile::convertToScene_assets(ProfileFile& pf) {
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

std::string Profile::convertToScene_properties(ProfileFile& pf) {
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

std::string Profile::convertToScene_keybindings(ProfileFile& pf) {
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

std::string Profile::convertToScene_markNodes(ProfileFile& pf) {
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

std::string Profile::convertToScene_time(ProfileFile& pf) {
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

std::string Profile::convertToScene_camera(ProfileFile& pf) {
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
