/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/fieldlinessequence/rendering/renderablefieldlinessequence.h>

#include <openspace/scene/scenegraphnode.h>

#include <ghoul/filesystem/filesystem.h>

using std::string;

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";

    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
    const char* KEY_INPUT_FILE_TYPE         = "InputFileType";   // [STRING]
    const char* KEY_SOURCE_FOLDER           = "SourceFolder";    // [STRING]

    // ---------------------------- OPTIONAL MODFILE KEYS  ---------------------------- //
    const char* KEY_OSLFS_LOAD_AT_RUNTIME   = "LoadAtRuntime";   // [BOOLEAN] If value False => Load in initializing step and store in RAM

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    const char* VALUE_INPUT_FILE_TYPE_CDF   = "cdf";
    const char* VALUE_INPUT_FILE_TYPE_JSON  = "json";
    const char* VALUE_INPUT_FILE_TYPE_OSFLS = "osfls";
} // namespace

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary) {

    if(!extractInfoFromDictionary(dictionary)) {
        _sourceFileType = INVALID;
    }

}

void RenderableFieldlinesSequence::initialize() {
    switch (_sourceFileType) {
        case CDF:
            LERROR("CDF NOT YET IMPLEMENTED!");
            break;
        case JSON:
            LERROR("JSON NOT YET IMPLEMENTED!");
            break;
        case OSFLS:
            if (_isLoadingStatesAtRuntime) {
                LERROR("OSFLS LOAD AT RUNTIME NOT YET IMPLEMENTED!");
            } else {
                for (string filePath : _sourceFiles) {
                    FieldlinesState newState;
                    bool loadedSuccessfully = newState.loadStateFromOsfls(filePath);
                    if (loadedSuccessfully) {
                        _states.push_back(newState);
                        _startTimes.push_back(newState.triggerTime());
                        _nStates++;
                    }
                }
            }
            break;
        default:
            break;
    }

    computeSequenceEndTime();
}

void RenderableFieldlinesSequence::deinitialize() {
}

bool RenderableFieldlinesSequence::isReady() const {
    return true;
}

void RenderableFieldlinesSequence::render(const RenderData& data, RendererTasks&) {
}

void RenderableFieldlinesSequence::update(const UpdateData& data) {
}

bool RenderableFieldlinesSequence::extractInfoFromDictionary(
        const ghoul::Dictionary& dictionary) {

    string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);
    name += ": ";

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    string inputFileTypeValue;
    if(!dictionary.getValue(KEY_INPUT_FILE_TYPE, inputFileTypeValue)) {
        LERROR(name << "The field " << string(KEY_INPUT_FILE_TYPE) << " is missing!");
        return false;
    } else {
        std::transform(inputFileTypeValue.begin(), inputFileTypeValue.end(),
                       inputFileTypeValue.begin(), ::tolower);
        // Verify that the input type is correct
        if (inputFileTypeValue == VALUE_INPUT_FILE_TYPE_CDF) {
            _sourceFileType = CDF;
        } else if (inputFileTypeValue == VALUE_INPUT_FILE_TYPE_JSON) {
            _sourceFileType = JSON;
        } else if (inputFileTypeValue == VALUE_INPUT_FILE_TYPE_OSFLS) {
            _sourceFileType = OSFLS;
        } else {
            LERROR(name << inputFileTypeValue << " is not a recognised "
                        << KEY_INPUT_FILE_TYPE);
            _sourceFileType = INVALID;
            return false;
        }
    }

    string sourceFolderPath;
    if(!dictionary.getValue(KEY_SOURCE_FOLDER, sourceFolderPath)) {
        LERROR(name << "The field " << string(KEY_SOURCE_FOLDER) << " is missing!");
        return false;
    }

    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeValue>
    ghoul::filesystem::Directory sourceFolder(sourceFolderPath);
    if (FileSys.directoryExists(sourceFolder)) {
        // Extract all file paths from the provided folder (Non-recursively! Sorted!)
        _sourceFiles = sourceFolder.readFiles(ghoul::Boolean::No, ghoul::Boolean::Yes);

        // Remove all files that don't have <inputFileTypeValue> as extension
        _sourceFiles.erase(std::remove_if(_sourceFiles.begin(), _sourceFiles.end(),
            [inputFileTypeValue] (string str) {
                const size_t EXT_LENGTH = inputFileTypeValue.length();
                string sub = str.substr(str.length() - EXT_LENGTH, EXT_LENGTH);
                std::transform(sub.begin(), sub.end(), sub.begin(), ::tolower);
                return sub != inputFileTypeValue;
            }), _sourceFiles.end());
        // Ensure that there are available and valid source files left
        if (_sourceFiles.empty()) {
            LERROR(name << sourceFolderPath << " contains no ." << inputFileTypeValue
                        << " files!");
            return false;
        }
    } else {
        LERROR(name << "FieldlinesSequence" << sourceFolderPath
                    << " is not a valid directory!");
        return false;
    }

    switch (_sourceFileType) {
        case CDF:
            LERROR(name << "CDF NOT YET IMPLEMENTED!");
            break;
        case JSON:
            LERROR(name << "JSON NOT YET IMPLEMENTED!");
            break;
        case OSFLS: {
                bool shouldLoadInRealtime = false;
                if (dictionary.getValue(KEY_OSLFS_LOAD_AT_RUNTIME, shouldLoadInRealtime)) {
                    _isLoadingStatesAtRuntime = shouldLoadInRealtime;
                } else {
                    LWARNING(name << KEY_OSLFS_LOAD_AT_RUNTIME <<
                        " isn't specified! Fieldline states will be stored in RAM");
                }
            } break;
        default:
            break;
    }
}

// Calculate expected end time.
void RenderableFieldlinesSequence::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double LAST_TRIGGER_TIME      = _startTimes[_nStates - 1];
        const double SEQUENCE_DURATION      = LAST_TRIGGER_TIME - _startTimes[0];
        const double AVERAGE_STATE_DURATION = SEQUENCE_DURATION / (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = LAST_TRIGGER_TIME + AVERAGE_STATE_DURATION;
    } else {
        // If there's just one state it should never disappear!
        _sequenceEndTime = DBL_MAX;
    }
}

} // namespace openspace
