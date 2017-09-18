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

namespace {
    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
    const char* KEY_INPUT_FILE_TYPE         = "InputFileType";   // [STRING]

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    const char* VALUE_INPUT_FILE_TYPE_CDF   = "cdf";
    const char* VALUE_INPUT_FILE_TYPE_JSON  = "json";
    const char* VALUE_INPUT_FILE_TYPE_OSFLS = "osfls";
} // namespace

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary) {

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);

    _loggerCat += " [" + name + "]";

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    std::string inputFileType;
    if(!dictionary.getValue(KEY_INPUT_FILE_TYPE, inputFileType)) {
        LERRORC("FieldlinesSequence",
                "The field " + std::string(KEY_INPUT_FILE_TYPE) + " is missing!");
        return;
    }
}

void RenderableFieldlinesSequence::initialize() {
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

} // namespace openspace
