/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/documentation/documentationengine.h>

#include <openspace/openspace.h>
#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>

#include <fstream>

namespace {
    constexpr const char* HandlebarsFilename =
        "${WEB}/documentation/handlebars-v4.0.5.js";
    constexpr const char* BootstrapFilename = "${WEB}/common/bootstrap.min.css";
    constexpr const char* CssFilename = "${WEB}/documentation/style.css";
    constexpr const char* JsFilename = "${WEB}/documentation/script.js";
} // namespace

namespace openspace::documentation {

DocumentationEngine* DocumentationEngine::_instance = nullptr;

DocumentationEngine::DuplicateDocumentationException::DuplicateDocumentationException(
                                                              Documentation documentation)
    : ghoul::RuntimeError(fmt::format(
        "Duplicate Documentation with name '{}' and id '{}'",
        documentation.name,
        documentation.id
    ))
    , documentation(std::move(documentation))
{}

DocumentationEngine::DocumentationEngine()
    : DocumentationGenerator(
        "Top Level",
        "toplevel",
        {
            { "toplevelTemplate", "${WEB}/documentation/toplevel.hbs" },
        }
    )
{}

void DocumentationEngine::initialize() {
    ghoul_assert(!isInitialized(), "DocumentationEngine is already initialized");
    _instance = new DocumentationEngine;
}

void DocumentationEngine::deinitialize() {
    ghoul_assert(isInitialized(), "DocumentationEngine is not initialized");
    delete _instance;
    _instance = nullptr;
}

bool DocumentationEngine::isInitialized() {
    return _instance != nullptr;
}

DocumentationEngine& DocumentationEngine::ref() {
    if (_instance == nullptr) {
        _instance = new DocumentationEngine;
        registerCoreClasses(*_instance);
    }
    return *_instance;
}

std::string generateJsonDocumentation(const Documentation& d) {
    std::stringstream result;
    result << "{";

    result << R"("name": ")" << d.name << "\",";
    result << R"("id": ")" << d.id << "\",";
    result << R"("entries": [)";
    for (const DocumentationEntry& p : d.entries) {
        result << '{';
        result << R"("key": ")" << p.key << "\",";
        result << R"("optional": )" << (p.optional ? "true" : "false") << ',';
        result << R"("type": ")" << p.verifier->type() << "\",";
        result << R"("documentation": ")" << escapedJson(p.documentation) << "\",";
        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        ReferencingVerifier* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

        if (rv) {
            const std::vector<Documentation>& documentations = DocEng.documentations();
            auto it = std::find_if(
                documentations.begin(),
                documentations.end(),
                [rv](const Documentation& doc) { return doc.id == rv->identifier; }
            );

            if (it == documentations.end()) {
                result << R"("reference": { "found": false })";
            } else {
                result << R"("reference": {)"
                    << R"("found": true,)"
                    << R"("name": ")" << it->name << "\","
                    << R"("identifier": ")" << rv->identifier << '\"'
                    << '}';
            }
        }
        else if (tv) {
            std::string json = generateJsonDocumentation({ "", "", tv->documentations });
            // We have a TableVerifier, so we need to recurse
            result << R"("restrictions": )" << json;
        }
        else {
            result << R"("description": ")" << p.verifier->documentation() << '\"';
        }
        result << '}';
        if (&p != &d.entries.back()) {
            result << ", ";
        }

    }

    result << ']';
    result << '}';

    return result.str();
}

std::string DocumentationEngine::generateJson() const {
    std::stringstream json;
    json << "[";

    for (const Documentation& d : _documentations) {
        json << generateJsonDocumentation(d);
        if (&d != &_documentations.back()) {
            json << ", ";
        }
    }

    json << "]";

    return json.str();
}

void DocumentationEngine::addDocumentation(Documentation documentation) {
    if (documentation.id.empty()) {
        _documentations.push_back(std::move(documentation));
    }
    else {
        auto it = std::find_if(
            _documentations.begin(),
            _documentations.end(),
            [documentation](const Documentation& d) { return documentation.id == d.id; }
        );

        if (it != _documentations.end()) {
            throw DuplicateDocumentationException(std::move(documentation));
        }
        else {
            _documentations.push_back(std::move(documentation));
        }
    }
}

void DocumentationEngine::addHandlebarTemplates(std::vector<HandlebarTemplate> templates) {
    _handlebarTemplates.insert(std::end(_handlebarTemplates), std::begin(templates), std::end(templates));
}

std::vector<Documentation> DocumentationEngine::documentations() const {
    return _documentations;
}

void DocumentationEngine::writeDocumentationHtml(const std::string path, const std::string data) {

    std::ifstream handlebarsInput;
    handlebarsInput.exceptions(~std::ofstream::goodbit);
    handlebarsInput.open(absPath(HandlebarsFilename));
    const std::string handlebarsContent = std::string(
        std::istreambuf_iterator<char>(handlebarsInput),
        std::istreambuf_iterator<char>()
    );
    std::ifstream jsInput;
    jsInput.exceptions(~std::ofstream::goodbit);
    jsInput.open(absPath(JsFilename));
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

    std::string filename = path + ("index.html");
    std::ofstream file;
    file.exceptions(~std::ofstream::goodbit);
    file.open(filename);

    // We probably should escape backslashes here?
    file << "<!DOCTYPE html>" << '\n'
        << "<html>" << '\n'
        << "  " << "<head>" << '\n';

    //write handlebar templates to htmlpage as script elements (as per hb)
    for (const HandlebarTemplate& t : _handlebarTemplates) {
        const char* Type = "text/x-handlebars-template";
        file << "    <script id=\"" << t.name << "\" type=\"" << Type << "\">";
        file << '\n';

        std::ifstream templateFilename(absPath(t.filename));
        std::string templateContent(
            std::istreambuf_iterator<char>{templateFilename},
            std::istreambuf_iterator<char>{}
        );
        file << templateContent << "\n    </script>" << '\n';
    }

    //write main template
    file << "    <script id=\"mainTemplate\" type=\"text/x-handlebars-template\">";
    file << '\n';
    std::ifstream templateFilename(absPath("${WEB}/documentation/main.hbs"));
    std::string templateContent(
        std::istreambuf_iterator<char>{templateFilename},
        std::istreambuf_iterator<char>{}
    );
    file << templateContent << "    </script>" << '\n';

    //write scripte to register templates dynamically
    file << "    <script type=\"text/javascript\">" << '\n';
    file << "      templates = [];" << '\n';
    file << "      registerTemplates = function() {" << '\n';

    for (const HandlebarTemplate& t : _handlebarTemplates) {
        std::string nameOnly = t.name.substr(0, t.name.length() - 8); //-8 for Template
        file << "\t\t\t\tvar " << t.name;
        file << "Element = document.getElementById('" << t.name << "');" << '\n';

        file << "\t\t\t\tHandlebars.registerPartial('" << nameOnly << "', ";
        file << t.name << "Element.innerHTML);" << '\n';

        file << "\t\t\t\ttemplates['" << nameOnly << "'] = Handlebars.compile(";
        file << t.name << "Element.innerHTML);" << '\n';
    }
    file << "\t\t\t}" << '\n';
    file << "\t\t</script>" << '\n';


    const std::string DataId = "data";

    const std::string Version =
        "[" +
        std::to_string(OPENSPACE_VERSION_MAJOR) + "," +
        std::to_string(OPENSPACE_VERSION_MINOR) + "," +
        std::to_string(OPENSPACE_VERSION_PATCH) +
        "]";

    file
        << "    " << "<script id=\"" << DataId
        << "\" type=\"text/application/json\">" << '\n'
        << "      " << data << '\n'
        << "    " << "</script>" << '\n';


    file
        << "  " << "<script>" << '\n'
        << "    " << jsContent << '\n'
        << "    " << "var documentation = parseJson('" << DataId << "').documentation;\n"
        << "    " << "var version = " << Version << ";" << '\n'
        << "    " << "var currentDocumentation = documentation[0];" << '\n'
        << "    " << handlebarsContent << '\n'
        << "  " << "</script>" << '\n'
        << "  " << "<style type=\"text/css\">" << '\n'
        << "    " << cssContent << '\n'
        << "    " << bootstrapContent << '\n'
        << "  " << "</style>" << '\n'
        << "    " << "<title>OpenSpace Documentation</title>" << '\n'
        << "  " << "</head>" << '\n'
        << "  " << "<body>" << '\n'
        << "  " << "</body>" << '\n'
        << "</html>" << '\n';
}


} // namespace openspace::documentation
