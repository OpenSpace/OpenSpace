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

#include <openspace/documentation/documentationgenerator.h>

#include <openspace/openspace.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/invariants.h>

#include <fstream>

namespace {
    const char* HandlebarsFilename = "${OPENSPACE_DATA}/web/common/handlebars-v4.0.5.js";
    const char* BootstrapFilename = "${OPENSPACE_DATA}/web/common/bootstrap.min.css";
    const char* CssFilename = "${OPENSPACE_DATA}/web/common/style.css";
} // namespace

namespace openspace {

DocumentationGenerator::DocumentationGenerator(std::string name,
                                               std::string jsonName,
                                        std::vector<HandlebarTemplate> handlebarTemplates,
                                               std::string javascriptFilename)
    : _name(std::move(name))
    , _jsonName(std::move(jsonName))
    , _handlebarTemplates(std::move(handlebarTemplates))
    , _javascriptFile(std::move(javascriptFilename))
{
    ghoul_precondition(!_name.empty(), "name must not be empty");
    ghoul_precondition(!_jsonName.empty(), "jsonName must not be empty");
    for (const HandlebarTemplate& t : _handlebarTemplates) {
        (void)t; // Unused variable in Release mode
        ghoul_precondition(!t.name.empty(), "name must not be empty");
        ghoul_precondition(!t.filename.empty(), "filename must not be empty");
    }
    ghoul_precondition(!_javascriptFile.empty(), "javascriptFilename must not be empty");
}
    
void DocumentationGenerator::writeDocumentation(const std::string& filename) {
    std::ifstream handlebarsInput;
    handlebarsInput.exceptions(~std::ofstream::goodbit);
    handlebarsInput.open(absPath(HandlebarsFilename));
    const std::string handlebarsContent = std::string(
        std::istreambuf_iterator<char>(handlebarsInput),
        std::istreambuf_iterator<char>()
    );

    std::ifstream jsInput;
    jsInput.exceptions(~std::ofstream::goodbit);
    jsInput.open(absPath(_javascriptFile));
    const std::string jsContent = std::string(
        std::istreambuf_iterator<char>(jsInput),
        std::istreambuf_iterator<char>()
    );

    std::ifstream bootstrapInput;
    bootstrapInput.exceptions(~std::ofstream::goodbit);
    bootstrapInput.open(absPath(BootstrapFilename));
    const std::string bootstrapContent = std::string(
        std::istreambuf_iterator<char>(bootstrapInput),
        std::istreambuf_iterator<char>()
    );

    std::ifstream cssInput;
    cssInput.exceptions(~std::ofstream::goodbit);
    cssInput.open(absPath(CssFilename));
    const std::string cssContent = std::string(
        std::istreambuf_iterator<char>(cssInput),
        std::istreambuf_iterator<char>()
    );

    std::ofstream file;
    file.exceptions(~std::ofstream::goodbit);
    file.open(filename);
    
    std::string json = generateJson();
    // We probably should escape backslashes here?
    
    file           << "<!DOCTYPE html>"                                           << '\n'
                   << "<html>"                                                    << '\n'
         << "\t"   << "<head>"                                                    << '\n';
    
    for (const HandlebarTemplate& t : _handlebarTemplates) {
        const char* Type = "text/x-handlebars-template";
        file << "\t\t"
                   << "<script id=\"" << t.name << "\" type=\"" << Type << "\">"  << '\n';
        
        std::ifstream templateFilename(absPath(t.filename));
        std::string templateContent(
            std::istreambuf_iterator<char>{templateFilename},
            std::istreambuf_iterator<char>{}
        );
        file << templateContent                                                   << '\n';
        file << "\t"
                   << "</script>"                                                 << '\n';
    }
    
    const std::string Version =
        "[" +
        std::to_string(OPENSPACE_VERSION_MAJOR) + "," +
        std::to_string(OPENSPACE_VERSION_MINOR) + "," +
        std::to_string(OPENSPACE_VERSION_PATCH) +
        "]";
    
        std::string generationTime;
        try {
            generationTime = Time::now().ISO8601();
        }
        catch (...) {}

    file
         << "\t"   << "<script>"                                                  << '\n'
         << "\t\t" << "var " << _jsonName << " = JSON.parse('" << json << "');"   << '\n'
         << "\t\t" << "var version = " << Version << ";"                          << '\n'
         << "\t\t" << "var generationTime = '" << generationTime << "';"          << '\n'
         << "\t\t" << handlebarsContent                                           << '\n'
         << "\t\t" << jsContent                                                   << '\n'
         << "\t"   << "</script>"                                                 << '\n'
         << "\t"   << "<style type=\"text/css\">"                                 << '\n'
         << "\t\t" << cssContent                                                  << '\n'
         << "\t\t" << bootstrapContent                                            << '\n' 
         << "\t"   << "</style>"                                                  << '\n'
         << "\t\t" << "<title>" << _name << "</title>"                            << '\n'
         << "\t"   << "</head>"                                                   << '\n'
         << "\t"   << "<body>"                                                    << '\n'
         << "\t"   << "</body>"                                                   << '\n'
                   << "</html>"                                                   << '\n';
}
    
} // namespace openspace
