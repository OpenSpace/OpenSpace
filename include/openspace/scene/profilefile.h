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

#ifndef __OPENSPACE_CORE___PROFILEFILE___H__
#define __OPENSPACE_CORE___PROFILEFILE___H__

#include <string>
#include <istream>
#include <fstream>
#include <functional>
#include <vector>

namespace ghoul { class Dictionary; }
namespace ghoul::opengl { class ProgramObject; }

namespace openspace {

namespace documentation { struct Documentation; }
namespace scripting { struct LuaLibrary; }

const size_t versionLinesExpected = 1;
const size_t timeLinesExpected = 1;
const size_t cameraLinesExpected = 1;
const size_t versionFieldsExpected = 1;
const size_t moduleFieldsExpected = 3;
const size_t assetFieldsExpected = 2;
const size_t propertyFieldsExpected = 3;
const size_t keybindingFieldsExpected = 6;
const size_t timeFieldsExpected = 2;
const size_t cameraNavigationFieldsExpected = 8;
const size_t cameraGeoFieldsExpected = 5;
const size_t markNodesFieldsExpected = 1;

const size_t moduleFieldName = 0;
const size_t moduleFieldLoaded = 1;
const size_t moduleFieldNotLoaded = 2;
const size_t assetFieldName = 0;
const size_t assetFieldReqd = 1;
const size_t propertyFieldType = 0;
const size_t propertyFieldName = 1;
const size_t propertyFieldValue = 2;
const size_t keybindingFieldKey = 0;
const size_t keybindingFieldDoc = 1;
const size_t keybindingFieldName = 2;
const size_t keybindingFieldGuiPath = 3;
const size_t keybindingFieldLocal = 4;
const size_t keybindingFieldCommand = 5;
const size_t timeFieldType = 0;
const size_t timeFieldSet = 1;
const size_t cameraFieldType = 0;
const size_t cameraNavigationFieldAnchor = 1;
const size_t cameraNavigationFieldAim = 2;
const size_t cameraNavigationFieldRef = 3;
const size_t cameraNavigationFieldPosition = 4;
const size_t cameraNavigationFieldUp = 5;
const size_t cameraNavigationFieldYaw = 6;
const size_t cameraNavigationFieldPitch = 7;
const size_t cameraGeoFieldAnchor = 1;
const size_t cameraGeoFieldLatitude = 2;
const size_t cameraGeoFieldLongitude = 3;
const size_t cameraGeoFieldAltitude = 4;

const std::string header_Version    = "#Version";
const std::string header_Module     = "#Module";
const std::string header_Asset      = "#Asset";
const std::string header_Property   = "#Property";
const std::string header_Keybinding = "#Keybinding";
const std::string header_Time       = "#Time";
const std::string header_Camera     = "#Camera";
const std::string header_MarkNodes  = "#MarkNodes";

class ProfileFile {
public:
    /**
     * Reads the contents of a profile file and populates vector containers for all
     * sections. This only pulls individual line entries into their proper sections;
     * it does not parse the tab-delimited fields of each line.
     * \param filename The profile file to read
     */
    void readFromFile(std::string filename);

    /**
     * Alternative function for reading the lines from a profile file. This is mainly
     * intended for testing purposes, but it can be used to provide the profile file
     * contents from another source (the function readFromFile() provides its own
     * ifstream source).
     * \param reader A std::function object that accepts a string reference which will
     *               be populated with a single line of content. This function returns
     *               true if a single line was read successfully, or false if not to
     *               indicate that the end of the content has been reached.
     */
    void readLines(std::function<bool(std::string&)> reader);

    /**
     * Returns the string contents of this object converted to scene/asset
     * equivalent syntax, with all section headers and contents of each listed on an
     * individual line.
     * \return The full contents of the profile file in string format.
     */
    std::string writeToString();

    /**
     * Writes the formatted contents of this object to a file.
     * This function calls writeToString() in order to get everything in formatted
     * form.
     * \param filename The filename to write to.
     */
    void writeToFile(const std::string filename);

    /**
     * Updates the full string that defines the starting time. The format for this line
     * is defined by ProfileFile::parseTime and Profile::convertToAsset_time
     * \param line The time entry line to replace current time entry
     */
    void updateTime(const std::string line);

    /**
     * Updates the full string that defines the starting camera position. The format for
     * this line is defined by ProfileFile::parseCamera & Profile::convertToAsset_camera
     * \param line The camera entry line to replace current camera entry
     */
    void updateCamera(const std::string line);

    /**
     * Adds a new module line to the list of module lines to be analyzed by the profile
     * at startup. The format for a module line is defined by ProfileFile::parseModule
     * and Profile::convertToAsset_modules
     * \param line The module name to be added
     */
    void addModuleLine(const std::string line);

    /**
     * Adds a new asset to the list of assets to be loaded at startup. The format for an
     * asset line is defined by ProfileFile::parseAsset & Profile::convertToAsset_assets
     * \param line The asset name to be added
     */
    void addAssetLine(const std::string line);

    /**
     * Adds a new property set command to the list of property settings to be
     * performed at startup. The format for a property set command line is defined by
     * ProfileFile::parseProperty and Profile::convertToAsset_properties
     * \param line The property set command to be added
     */
    void addPropertyLine(const std::string line);

    /**
     * Adds a new keybinding shortcut to the list of keybindings. The format for a
     * keybinding line is defined by ProfileFile::parseKeybinding and
     * Profile::convertToAsset_keybindings
     * \param line The keyboard shortcut line to be added
     */
    void addKeybindingLine(const std::string line);

    /**
     * Adds a new scenegraph node name to be added to the list of those marked as
     * 'interesting'. The format for a mark nodes line is defined by
     * ProfileFile::parseMarkNodes and Profile::convertToAsset_markNodes
     * \param line The scenegraph node to be added
     */
    void addMarkNodesLine(const std::string line);

    /**
     * Returns the version number (profiles syntax version) string
     * \return The version string
     */
    const std::string getVersion() const;

    /**
     * Returns the profile's time string. See updateTime comment header for notes on
     * syntax of this time string
     * \return The time string
     */
    std::string time() const;

    /**
     * Returns the profile's camera string. See updateCamera comment header for notes on
     * syntax of this camera string
     * \return The camera string
     */
    std::string camera() const;

    /**
     * Returns the vector of OpenSpace modules listed in this profile. See addModuleLine
     * comment header for notes on the syntax of each entry.
     * \return The vector of module lines
     */
    std::vector<std::string> modules() const;

    /**
     * Returns the vector of OpenSpace assets listed in this profile. See addAssetLine
     * comment header for notes on the syntax of each entry.
     * \return The vector of asset lines
     */
    std::vector<std::string> assets() const;

    /**
     * Returns the vector of OpenSpace property set commands included in this profile.
     * See addPropertyLine comment header for notes on the syntax of each entry.
     * \return The vector of property set commands
     */
    std::vector<std::string> properties() const;

    /**
     * Returns the vector of OpenSpace keybinding shortcut definitions included in this
     * profile. See addKeybindingLine comment header for syntax notes of each entry.
     * \return The vector of keybinding shortcut definitions
     */
    std::vector<std::string> keybindings() const;

    /**
     * Returns the vector of OpenSpace scenegraph nodes marked as 'interesting'.
     * See addMarkNodesLine comment header for syntax notes of each entry.
     * \return The vector of nodes to be marked as interesting.
     */
    std::vector<std::string> markNodes() const;

    /**
     * Splits a tab-delimited line (of any type) into a vector with individual string
     * entries.
     * \param line The tab-delimited line from which to extract the fields
     * \param result A vector of n strings that is populated by the split operation
     * \return The number of fields that were extracted
     */
    size_t splitByTab(std::string line, std::vector<std::string>& result);

private:
    std::string errorString(std::string message);
    void clearAllFields();
    bool isBlank(std::string line);
    void verifyRequiredFields(std::string sectionName, std::vector<std::string> fields,
                              std::vector<std::string> standard, unsigned int nFields);
    void processIndividualLine(bool& insideSection, std::string line);
    bool determineSection(std::string line);
    void (ProfileFile::* parseCurrentSection)(std::string);
    void parseVersion(std::string line);
    void parseModule(std::string line);
    void parseAsset(std::string line);
    void parseProperty(std::string line);
    void parseKeybinding(std::string line);
    void parseTime(std::string line);
    void parseCamera(std::string line);
    void parseMarkNodes(std::string line);
    void addAllElements(std::string& str, std::vector<std::string>& list);

    size_t _lineNum = 1;
    size_t _numLinesVersion = 0;
    size_t _numLinesTime    = 0;
    size_t _numLinesCamera  = 0;

    std::string _version;
    std::string _time;
    std::string _camera;
    std::vector<std::string> _modules;
    std::vector<std::string> _assets;
    std::vector<std::string> _properties;
    std::vector<std::string> _keybindings;
    std::vector<std::string> _markNodes;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PROFILEFILE___H__
