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
    void writeToFile(std::string filename);

    //Methods for updating contents
    void updateTime();
    void updateCamera();
    void addModuleLine(std::string line);
    void addAssetLine(std::string line);
    void addPropertyLine(std::string line);
    void addKeybindingLine(std::string line);
    void addMarkNodesLine(std::string line);

    //Methods for getting contents of each section
    const std::string getVersion() const;
    std::string time() const;
    std::string camera() const;
    std::vector<std::string> modules() const;
    std::vector<std::string> assets() const;
    std::vector<std::string> properties() const;
    std::vector<std::string> keybindings() const;
    std::vector<std::string> markNodes() const;
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
