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

#ifndef __OPENSPACE_CORE___PROFILE___H__
#define __OPENSPACE_CORE___PROFILE___H__

#include <openspace/scene/profilefile.h>

#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scenelicense.h>
#include <ghoul/misc/easing.h>
#include <ghoul/misc/exception.h>
#include <mutex>
#include <set>
#include <unordered_map>
#include <vector>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }
namespace scripting { struct LuaLibrary; }

const std::string profileFormatVersion = "1.0";

class Profile {
public:
    enum class AssetEventType {
        add,
        require,
        request,
        remove,
        ignore
    };
    const std::map<AssetEventType, std::string> AssetEventTypeString {
        {AssetEventType::add, "add"},
        {AssetEventType::require, "required"},
        {AssetEventType::request, "requested"},
        {AssetEventType::remove,  "removed"},
        {AssetEventType::ignore,  "ignored"},
    };
    struct AssetEvent {
        std::string name;
        AssetEventType eventType;
    };

    virtual ~Profile() {};

    const std::string formatVersion() {
        return profileFormatVersion;
    }

    /**
     * Saves all current settings, starting from the profile that was loaded at startup,
     * and all of the property & asset changes that were made since startup.
     * \param filename The filename of the new profile to be saved
     */
    void saveCurrentSettingsToProfile(std::string filename);

    /**
     * Saves all current settings, similar to saveCurrentSettingsToProfile() except the
     * output is a string without writing to a file.
     */
    std::string saveCurrentSettingsToProfile_string();

    /**
     * Reads in a .profile file, converts it to scene/asset equivalent syntax, and
     * writes the result to the specified output path.
     * \param inProfilePath The .profile file to be converted
     * \param outFilePath The output file path that will be written with the converted
     *                       contents (in an .asset file)
     */
    void convertToSceneFile(const std::string inProfilePath,
        const std::string outFilePath);

    /**
     * Returns the string contents of a profileFile object converted to scene/asset
     * equivalent syntax.
     * \param pf The profileFile object to be converted
     * \return The full string contents of scene/asset equivalent of the profile file.
     */
    std::string convertToScene(ProfileFile& pf);

    /**
     * Returns the Lua library that contains all Lua functions available to provide
     * profile functionality.
     * \return The Lua library that contains all Lua functions available for profiles
     */
    static scripting::LuaLibrary luaLibrary();

protected:
    std::string _profileBaseDirectory = "${ASSETS}";

private:
    struct AllAssetDetails {
        std::vector<AssetEvent> base;
        std::vector<AssetEvent> changed;
    };
    virtual bool usingProfile();
    virtual std::string initialProfile();
    virtual std::string profileBaseDirectory();
    virtual std::vector<AssetEvent> listOfAllAssetEvents();
    ProfileFile collateBaseWithChanges();
    std::string convertToScene_assets(ProfileFile& pf);
    std::string convertToScene_modules(ProfileFile& pf);
    std::string convertToScene_properties(ProfileFile& pf);
    std::string convertToScene_markNodes(ProfileFile& pf);
    std::string convertToScene_keybindings(ProfileFile& pf);
    std::string convertToScene_time(ProfileFile& pf);
    std::string convertToScene_camera(ProfileFile& pf);

    void updateToCurrentFormatVersion(ProfileFile& pf);
    std::vector<AssetEvent> modifyAssetsToReflectChanges(ProfileFile& pf);
    void parseAssetFileLines(std::vector<AssetEvent>& results, ProfileFile& pf);
    void handleChangedAdd(std::vector<AssetEvent>& base, unsigned int changedIdx,
        std::vector<AssetEvent>& changed, std::string asset);
    void handleChangedRemove(std::vector<AssetEvent>& base, std::string asset);
    void addAssetsToProfileFile(std::vector<AssetEvent>& allAssets, ProfileFile& pf);
    void modifyPropertiesToReflectChanges(ProfileFile& pf);
    virtual std::vector<openspace::properties::Property*> getChangedProperties();
    void checkForChangedProps(std::vector<openspace::properties::Property*>& changedList,
        openspace::properties::PropertyOwner* po);
    std::string getFullPropertyPath(openspace::properties::Property* prop);
    virtual std::vector<std::string> getChangedPropertiesFormatted();
    virtual std::string getCurrentTimeUTC();
    virtual interaction::NavigationHandler::NavigationState getCurrentCameraState();
    void addCurrentTimeToProfileFile(ProfileFile& pf);
    void addCurrentCameraToProfileFile(ProfileFile& pf);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILE___H__
