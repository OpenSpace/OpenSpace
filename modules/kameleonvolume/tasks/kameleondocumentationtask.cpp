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

#include <modules/kameleonvolume/tasks/kameleondocumentationtask.h>

#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <openspace/openspace.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <fstream>

namespace {
    constexpr const char* KeyInput = "Input";
    constexpr const char* KeyOutput = "Output";
    constexpr const char* MainTemplateFilename = "${WEB}/kameleondocumentation/main.hbs";
    constexpr const char* HandlebarsFilename = "${WEB}/common/handlebars-v4.0.5.js";
    constexpr const char* JsFilename = "${WEB}/kameleondocumentation/script.js";
    constexpr const char* BootstrapFilename = "${WEB}/common/bootstrap.min.css";
    constexpr const char* CssFilename = "${WEB}/common/style.css";
} // namespace

namespace openspace::kameleonvolume {

KameleonDocumentationTask::KameleonDocumentationTask(const ghoul::Dictionary& dictionary)
{
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "KameleonDocumentationTask"
    );

    _inputPath = absPath(dictionary.value<std::string>(KeyInput));
    _outputPath = absPath(dictionary.value<std::string>(KeyOutput));
}

std::string KameleonDocumentationTask::description() {
    return fmt::format(
        "Extract metadata from cdf file {} and output html documentation to {}",
        _inputPath, _outputPath
    );
}

void KameleonDocumentationTask::perform(const Task::ProgressCallback & progressCallback) {
    KameleonVolumeReader reader(_inputPath);
    ghoul::Dictionary kameleonDictionary = reader.readMetaData();
    progressCallback(0.33f);
    ghoul::DictionaryJsonFormatter formatter;


    ghoul::Dictionary dictionary = {
        { "kameleon", std::move(kameleonDictionary) },
        { "version",
            std::to_string(OPENSPACE_VERSION_MAJOR) + "." +
            std::to_string(OPENSPACE_VERSION_MINOR) + "." +
            std::to_string(OPENSPACE_VERSION_PATCH)
        },
        { "input", _inputPath }
    };

    std::string json = formatter.format(dictionary);
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

    progressCallback(1.0f);
}

documentation::Documentation KameleonDocumentationTask::documentation() {
    using namespace documentation;
    return {
        "KameleonDocumentationTask",
        "kameleon_documentation_task",
        {
            {
                "Type",
                new StringEqualVerifier("KameleonDocumentationTask"),
                Optional::No,
                "The type of this task"
            },
            {
                KeyInput,
                new StringAnnotationVerifier("A file path to a cdf file"),
                Optional::No,
                "The cdf file to extract data from"
            },
            {
                KeyOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The html file to write documentation to"
            }
        }
    };
}

} // namespace openspace::kameleonvolume
