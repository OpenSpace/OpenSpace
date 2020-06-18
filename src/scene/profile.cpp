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

    void addAssetsToProfileFile(ProfileStruct& ps,
                                const std::vector<Profile::AssetEvent>& allAssets)
    {
        ps.assets.clear();
        for (Profile::AssetEvent a : allAssets) {
            if (a.eventType != Profile::AssetEventType::Ignore) {
                ProfileStruct::Asset asset;
                asset.path = a.name;
                asset.type = ProfileStruct::Asset::Type::Require;
                ps.assets.push_back(std::move(asset));
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
    ProfileStruct ps = collateBaseWithChanges();

    if (filename.find('/') != std::string::npos) {
        LERROR("Profile filename must not contain path (/) elements");
        return;
    }
    else if (filename.find(':') != std::string::npos) {
        LERROR("Profile filename must not contain path (:) elements");
        return;
    }
    else if (filename.find('.') != std::string::npos) {
        LERROR("Only provide the filename to save without file extension");
        return;
    }
    const std::string absFilename = absPath("${ASSETS}/" + filename + ".profile");

    if (FileSys.fileExists(absFilename)) {
        LERROR(fmt::format(
            "Unable to save profile '{}'. File of same name already exists.",
            absFilename.c_str()
        ));
        return;
    }

    std::ofstream outFile;
    // @TODO (abock, 2020-06-15) Replace with non-throwing stream
    try {
        outFile.open(absFilename, std::ofstream::out);
    }
    catch (const std::ofstream::failure& e) {
        LERROR(fmt::format(
            "Exception opening profile file for write: {} ({})", absFilename, e.what()
        ));
    }

    try {
        outFile << serialize(ps);
    }
    catch (const std::ofstream::failure& e) {
        LERROR("Data write error to file: "
            + absFilename + " (" + e.what() + ")");
    }

    outFile.close();
}

std::string Profile::saveCurrentSettingsToProfile_string() {
    ProfileStruct ps = collateBaseWithChanges();
    return serialize(ps);
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

ProfileStruct Profile::collateBaseWithChanges() {
    if (!usingProfile()) {
        std::string errorMessage = "Program was not started using a profile, "
            "so cannot use this save-current-settings feature";
        LERROR(errorMessage);
    }
    std::string initProfile = initialProfile();
    std::string inputProfilePath = absPath(_profileBaseDirectory) + "/" + initProfile
        + ".profile";
    ProfileStruct ps = readFromFile(inputProfilePath);
    ps.version = ProfileStruct::Version{};

    std::vector<AssetEvent> ass = modifyAssetsToReflectChanges(ps);
    addAssetsToProfileFile(ps, ass);
    modifyPropertiesToReflectChanges(ps);

    // add current time to profile file
    ProfileStruct::Time time;
    time.time = currentTimeUTC();
    time.type = ProfileStruct::Time::Type::Absolute;
    ps.time = std::move(time);
    
    addCurrentCameraToProfileFile(ps);
    return ps;
}

std::vector<Profile::AssetEvent> Profile::modifyAssetsToReflectChanges(ProfileStruct& ps) {
    std::vector<AssetEvent> a;
    parseAssetFileLines(a, ps);
    AllAssetDetails assetDetails;

    assetDetails.base = a;
    assetDetails.changed = assetEvents();

    for (unsigned int i = 0; i < assetDetails.changed.size(); i++) {
        AssetEvent event = assetDetails.changed[i];

        if (event.eventType == AssetEventType::Add) {
            handleChangedAdd(assetDetails.base, i, assetDetails.changed, event.name);
        }
        else if (event.eventType == AssetEventType::Remove) {
            assetDetails.base.push_back({ event.name, Profile::AssetEventType::Remove });
        }
    }
    return assetDetails.base;
}

void Profile::parseAssetFileLines(std::vector<AssetEvent>& results, ProfileStruct& ps) {
    for (ProfileStruct::Asset& a : ps.assets) {
        AssetEvent assetEvent;
        assetEvent.name = a.path;
        assetEvent.eventType = [](ProfileStruct::Asset::Type type) {
            switch (type) {
                case ProfileStruct::Asset::Type::Request: return AssetEventType::Request;
                case ProfileStruct::Asset::Type::Require: return AssetEventType::Require;
                default: throw ghoul::MissingCaseException();
            }
        }(a.type);
        results.push_back(assetEvent);
    }
}

void Profile::modifyPropertiesToReflectChanges(ProfileStruct& ps) {
    std::vector<properties::Property*> changedProps = changedProperties();
    std::vector<std::string> formattedLines;

    for (properties::Property* prop : changedProps) {
        ProfileStruct::Property p;
        p.setType = ProfileStruct::Property::SetType::SetPropertyValueSingle;
        p.name = getFullPropertyPath(prop);
        p.value = prop->getStringValue();
        ps.properties.push_back(std::move(p));
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

void Profile::addCurrentCameraToProfileFile(ProfileStruct& ps) const {
    interaction::NavigationHandler::NavigationState nav = currentCameraState();

    ProfileStruct::CameraNavState camera;
    camera.anchor = nav.anchor;
    camera.aim = nav.aim;
    camera.referenceFrame = nav.referenceFrame;
    camera.position = fmt::format(
        "{},{},{}",
        nav.position.x, nav.position.y, nav.position.z
    );
    if (nav.up.has_value()) {
        camera.up = fmt::format(
            "{},{},{}",
            nav.up->x, nav.up->y, nav.up->z
        );
    }
    camera.yaw = nav.yaw;
    camera.pitch = nav.pitch;
    ps.camera = std::move(camera);
}

void Profile::convertToSceneFile(const std::string& inProfilePath,
                                 const std::string& outFilePath)
{
    ZoneScoped

    ProfileStruct ps = readFromFile(inProfilePath);

    std::ofstream outFile;
    try {
        outFile.open(outFilePath, std::ofstream::out);
    }
    catch (const std::ofstream::failure& e) {
        LERROR(fmt::format(
            "Exception opening scene file for write: {} ({})", outFilePath, e.what()
        ));
    }

    try {
        outFile << openspace::convertToSceneFile(ps);
    }
    catch (const std::ofstream::failure& e) {
        LERROR(fmt::format(
            "Data write error to scene file: {} ({})", outFilePath, e.what()
        ));
    }
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
