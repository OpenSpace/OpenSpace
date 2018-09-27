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

#include <modules/dsn/rendering/communicationlines.h>
#include <openspace/scene/scene.h>


namespace openspace {
    constexpr const char* _loggerCat = "CommunicationLine";

    // Keys to get values from dictionary
    constexpr const char* KeyDataFolder = "DataFolder";
    constexpr const char* KeyDataFileType = "DataFileType";

    //Filetypes
    const std::string dataFileTypeStringXml = "xml";

    enum class DataFileType : int {
        Xml = 0,
        Invalid
    };

    //Member functions
    CommunicationLines::CommunicationLines(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary) {
        _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
        CommunicationLines::initializeGL();
    }
    /**
    * Using rapidxml lib to parse the data from xml files
    */
    void CommunicationLines::xmlParser(std::string filename, std::ofstream &logfile)
    {
        rapidxml::file<> xmlfile((char*)filename.c_str());
        rapidxml::xml_document<> doc;
        doc.parse<0>(xmlfile.data());

        rapidxml::xml_node<> *rootNode = doc.first_node(); //find root, dsn
        rapidxml::xml_node<> *node = rootNode->first_node();

        //loop through all nodes
        while (node != nullptr) {
            logfile << node->name() << "\n";
            //loop through all attributes of a node
            for (rapidxml::xml_attribute<> *attribute = node->first_attribute();
                attribute; attribute = attribute->next_attribute()) {

                if (attribute->value())
                    logfile << "   " << attribute->name() << ": " << attribute->value() << "\n";
            }

            rapidxml::xml_node<> *childNode = node->first_node();
            while (childNode != nullptr) {
                logfile << "   " << childNode->name() << "\n";

                for (rapidxml::xml_attribute<> *childAttribute = childNode->first_attribute();
                    childAttribute; childAttribute = childAttribute->next_attribute()) {

                    if(childAttribute->value())
                    logfile <<"        "<< childAttribute->name() << ": " << childAttribute->value() << "\n";
                }

                childNode = childNode->next_sibling();
            }
            node = node->next_sibling();
        }
    }
    /**
    * Extracts the general information (from the lua modfile) that is mandatory for the class
    * to function; such as the file type and the location of the data files.
    * Returns false if it fails to extract mandatory information!
    */
    bool CommunicationLines::extractMandatoryInfoFromDictionary()
    {
        DataFileType sourceFileType = DataFileType::Invalid;

        // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
        std::string dataFileTypeString;
        if (!_dictionary->getValue(KeyDataFileType, dataFileTypeString)) {
            LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyDataFileType));
        }
        else {
            std::transform(
                dataFileTypeString.begin(),
                dataFileTypeString.end(),
                dataFileTypeString.begin(),
                [](char c) { return static_cast<char>(tolower(c)); }
            );
            // Verify that the input type is correct
            if (dataFileTypeString == dataFileTypeStringXml) {
                sourceFileType = DataFileType::Xml;
            }
            else {
                LERROR(fmt::format(
                    "{}: {} is not a recognized {}",
                    _identifier, dataFileTypeString, KeyDataFileType
                ));
                return false;
            }
        }

        std::string dataFolderPath;
        if (!_dictionary->getValue(KeyDataFolder, dataFolderPath)) {
            LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyDataFolder));
            return false;
        }

        // Ensure that the source folder exists and then extract
        // the files with the same extension as <inputFileTypeString>
        ghoul::filesystem::Directory dataFolder(dataFolderPath);
        if (FileSys.directoryExists(dataFolder)) {
            // Extract all file paths from the provided folder
            _dataFiles = dataFolder.readFiles(
                ghoul::filesystem::Directory::Recursive::No,
                ghoul::filesystem::Directory::Sort::Yes
            );

            // Remove all files that don't have <dataFileTypeString> as extension
            _dataFiles.erase(
                std::remove_if(
                    _dataFiles.begin(),
                    _dataFiles.end(),
                    [dataFileTypeString](const std::string& str) {
                const size_t extLength = dataFileTypeString.length();
                std::string sub = str.substr(str.length() - extLength, extLength);
                std::transform(
                    sub.begin(),
                    sub.end(),
                    sub.begin(),
                    [](char c) { return static_cast<char>(::tolower(c)); }
                );
                return sub != dataFileTypeString;
            }),
                _dataFiles.end()
                );
            // Ensure that there are available and valid source files left
            if (_dataFiles.empty()) {
                LERROR(fmt::format(
                    "{}: {} contains no {} files",
                    _identifier, dataFolderPath, dataFileTypeString
                ));
                return false;
            }
        }
        else {
            LERROR(fmt::format(
                "{}: {} is not a valid directory",
                _identifier,
                dataFolderPath
            ));
            return false;
        }
        return true;
    }

    void CommunicationLines::logSomething() {

        std::ofstream logfile{ "dsnmodulelog.txt" };

        for (std::vector<std::string>::iterator it = _dataFiles.begin(); it != _dataFiles.end(); ++it) {
            logfile << *it << std::endl;
        }
        logfile.close();
    }

    void CommunicationLines::render(const RenderData& data, RendererTasks&) {

    }

    void CommunicationLines::initializeGL() {
        // EXTRACT MANDATORY INFORMATION FROM DICTIONARY
        if (!extractMandatoryInfoFromDictionary()) {
            return;
        }
        //logfile for checking so that the data parsing works
        std::ofstream logfile{ "dsndatalogger.txt" };

        for (std::vector<std::string>::iterator it = _dataFiles.begin(); it != _dataFiles.end(); ++it) {
            CommunicationLines::xmlParser(*it, logfile);
        }
        logfile.close();
    }
    void CommunicationLines::deinitializeGL() {

    }

    bool CommunicationLines::isReady() const {
        return true;
    }

    void CommunicationLines::update(const UpdateData& data) {

    }
} // namespace openspace


