/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/kameleonvolume/tasks/kameleondocumentationtask.h>

#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <openspace/openspace.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <filesystem>
#include <fstream>

namespace {
    constexpr std::string_view MainTemplateFilename =
        "${WEB}/kameleondocumentation/main.hbs";
    constexpr std::string_view HandlebarsFilename = "${WEB}/common/handlebars-v4.0.5.js";
    constexpr std::string_view JsFilename = "${WEB}/kameleondocumentation/script.js";
    constexpr std::string_view BootstrapFilename = "${WEB}/common/bootstrap.min.css";
    constexpr std::string_view CssFilename = "${WEB}/common/style.css";

    struct [[codegen::Dictionary(KameleonDocumentationTask)]] Parameters {
        // The CDF file to extract data from
        std::filesystem::path input;

        // The HTML file to write documentation to
        std::string output [[codegen::annotation("A valid filepath")]];
    };
#include "kameleondocumentationtask_codegen.cpp"
} // namespace

namespace openspace::kameleonvolume {

documentation::Documentation KameleonDocumentationTask::documentation() {
    return codegen::doc<Parameters>("kameleon_documentation_task");
}

KameleonDocumentationTask::KameleonDocumentationTask(const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _inputPath = absPath(p.input.string());
    _outputPath = absPath(p.output);
}

std::string KameleonDocumentationTask::description() {
    return std::format(
        "Extract metadata from CDF file '{}' and output HTML documentation to '{}'",
        _inputPath, _outputPath
    );
}

void KameleonDocumentationTask::perform(const Task::ProgressCallback & progressCallback) {
    KameleonVolumeReader reader(_inputPath.string());
    ghoul::Dictionary kameleonDictionary = reader.readMetaData();
    progressCallback(0.33f);

    ghoul::Dictionary dictionary;
    dictionary.setValue("kameleon", std::move(kameleonDictionary));
    dictionary.setValue("version", std::string(OPENSPACE_VERSION_NUMBER));
    dictionary.setValue("input", _inputPath.string());

    std::string json = ghoul::formatJson(dictionary);
    progressCallback(0.66f);

    std::ifstream handlebarsInput(absPath(HandlebarsFilename));
    std::ifstream jsInput(absPath(JsFilename));

    std::string jsContent;
    std::back_insert_iterator<std::string> jsInserter(jsContent);

    std::copy(
        std::istreambuf_iterator<char>{handlebarsInput},
        std::istreambuf_iterator<char>(),
        jsInserter
    );
    std::copy(
        std::istreambuf_iterator<char>{jsInput},
        std::istreambuf_iterator<char>(),
        jsInserter
    );

    std::ifstream bootstrapInput(absPath(BootstrapFilename));
    std::ifstream cssInput(absPath(CssFilename));

    std::string cssContent;
    std::back_insert_iterator<std::string> cssInserter(cssContent);

    std::copy(
        std::istreambuf_iterator<char>{bootstrapInput},
        std::istreambuf_iterator<char>(),
        cssInserter
    );
    std::copy(
        std::istreambuf_iterator<char>{cssInput},
        std::istreambuf_iterator<char>(),
        cssInserter
    );

    std::ifstream mainTemplateInput(absPath(MainTemplateFilename));
    std::string mainTemplateContent{
        std::istreambuf_iterator<char>{mainTemplateInput},
        std::istreambuf_iterator<char>{}
    };

    std::ofstream file;
    file.exceptions(~std::ofstream::goodbit);
    file.open(_outputPath);

     std::stringstream html;
        html << "<!DOCTYPE html>\n"
            << "<html>\n"
            << "\t<head>\n"
            << "\t\t<script id=\"mainTemplate\" type=\"text/x-handlebars-template\">\n"
            << mainTemplateContent << "\n"
            << "\t\t</script>\n"
            << "\t\t<script id=\"data\" type=\"application/json\">\n"
            << json << "\n"
            << "\t</script>\n"
            << "\t<script>\n"
            << jsContent << "\n"
            << "\t</script>\n"
            << "\t<style type=\"text/css\">\n"
            << cssContent << "\n"
            << "\t</style>\n"
            << "\t\t<title>Documentation</title>\n"
            << "\t</head>\n"
            << "\t<body>\n"
            << "\t</body>\n"
            << "</html>\n";

    file << html.str();

    progressCallback(1.f);
}

} // namespace openspace::kameleonvolume
