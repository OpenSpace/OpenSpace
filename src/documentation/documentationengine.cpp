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

#include <openspace/documentation/documentationengine.h>

#include <openspace/openspace.h>
#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/misc/assert.h>
#include <ghoul/filesystem/filesystem.h>

#include <algorithm>
#include <fstream>
#include <sstream>
#include <streambuf>

#include <fmt/format.h>

namespace {
    const char* MainTemplateFilename = "${OPENSPACE_DATA}/web/documentation/main.hbs";
    const char* DocumentationTemplateFilename =
        "${OPENSPACE_DATA}/web/documentation/documentation.hbs";
    const char* JsFilename = "${OPENSPACE_DATA}/web/documentation/script.js";
} // namespace

namespace openspace {
namespace documentation {

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
        "Documentation",
        "documentation",
        {
            { "mainTemplate", MainTemplateFilename },
            { "documentationTemplate", DocumentationTemplateFilename }
        },
        JsFilename
    )
{}


DocumentationEngine& DocumentationEngine::ref() {
    if (_instance == nullptr) {
        _instance = new DocumentationEngine;
        registerCoreClasses(*_instance);
    }
    return *_instance;
}

std::string generateTextDocumentation(const Documentation& d, int& indentLevel) {
    using namespace std::string_literals;

    auto indentMessage = [&indentLevel](std::string prefix, std::string msg) {
        if (msg.empty()) {
            return ""s;
        }
        else {
            return std::string(indentLevel, '\t') + prefix + ": " + msg + '\n';
        }
    };
    std::string result;

    result += indentMessage("Name", d.name);
    if (!d.name.empty()) {
        ++indentLevel;
    }
    for (const auto& p : d.entries) {
        result += indentMessage("Key", (p.key == "*") ? p.key : "\"" + p.key + "\"");
        result += indentMessage("Optional", (p.optional ? "true" : "false"));
        result += indentMessage("Type", p.verifier->type());
        result += indentMessage("Documentation", p.documentation);
        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        ReferencingVerifier* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

        // We have to check ReferencingVerifier first as a ReferencingVerifier is also a
        // TableVerifier
        if (rv) {
            std::vector<Documentation> documentations = DocEng.documentations();
            auto it = std::find_if(
                documentations.begin(),
                documentations.end(),
                [rv](const Documentation& doc) { return doc.id == rv->identifier; }
            );

            if (it == documentations.end()) {
                result += indentMessage("Referencing", rv->identifier + "(NOT FOUND)");
            }
            else {
                result += indentMessage("Referencing", it->name);
            }
        } else if (tv) {
            // We have a TableVerifier, so we need to recurse
            ++indentLevel;
            result += generateTextDocumentation(
                { "", "", tv->documentations },
                indentLevel
            );
            result = result.substr(0, result.size() - 2);
            --indentLevel;
        }
        else {
            result += indentMessage("Restrictions", p.verifier->documentation());
        }
        result += indentMessage("Documentation", p.documentation);
        result += "\n\n";
    }
    if (!d.name.empty()) {
        --indentLevel;
    }

    return result;
}

std::string generateJsonDocumentation(const Documentation& d) {
    std::stringstream result;
    result << "{";

    result << "\"name\": \"" << d.name << "\",";
    result << "\"id\": \"" << d.id << "\",";
    result << "\"entries\": [";
    for (const auto& p : d.entries) {
        result << "{";
        result << "\"key\": \"" << p.key << "\",";
        result << "\"optional\": " << (p.optional ? "true" : "false") << ",";
        result << "\"type\": \"" << p.verifier->type() << "\",";
        result << "\"documentation\": \"" << escapedJson(p.documentation) << "\",";
        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        ReferencingVerifier* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

        if (rv) {
            std::vector<Documentation> documentations = DocEng.documentations();
            auto it = std::find_if(
                documentations.begin(),
                documentations.end(),
                [rv](const Documentation& doc) { return doc.id == rv->identifier; }
            );

            if (it == documentations.end()) {
                result << "\"reference\": { \"found\": false }";
            } else {
                result << "\"reference\": {"
                    << "\"found\": true,"
                    << "\"name\": \"" << it->name << "\","
                    << "\"identifier\": \"" << rv->identifier << "\""
                    << "}";
            }
        }
        else if (tv) {
            std::string json = generateJsonDocumentation({ "", "", tv->documentations });
            // We have a TableVerifier, so we need to recurse
            result << "\"restrictions\": " << json;
        }
        else {
            result << "\"description\": \"" << p.verifier->documentation() << "\"";
        }
        result << "}";
        if (&p != &d.entries.back()) {
            result << ", ";
        }

    }

    result << ']';
    result << "}";

    return result.str();
}

std::string generateHtmlDocumentation(const Documentation& d) {
    std::stringstream html;

    html << "\t<tr>\n"
         << "\t\t<td colspan=6>" << d.name << "<a name=\"" << d.id << "\"></a></td>\n";

    for (const auto& p : d.entries) {
        html << "\t<tr>\n"
             << "\t\t<td></td>\n"
             << "\t\t<td>" << p.key << "</td>\n"
             << "\t\t<td>" << (p.optional ? "Optional" : "Required") << "</td>\n"
             << "\t\t<td>" << escapedJson(p.documentation) << "</td>\n"
             << "\t\t<td>" << p.verifier->type() << "</td>\n";

        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        ReferencingVerifier* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

        // We have to check ReferencingVerifier first as a ReferencingVerifier is also a
        // TableVerifier
        if (rv) {
            std::vector<Documentation> documentations = DocEng.documentations();
            auto it = std::find_if(
                documentations.begin(),
                documentations.end(),
                [rv](const Documentation& doc) { return doc.id == rv->identifier; }
            );

            if (it == documentations.end()) {
                html << "\t\t<td>"
                     << "<font color=\"red\">"
                     << "Could not find identifier: " << rv->identifier
                     << "</font>"
                     << "</td>";
            }
            else {
                html << "\t\t<td>"
                     << "\t\t\tReferencing: "
                     << "<a href=\"#" << rv->identifier << "\">" << it->name << "</a>"
                     << "\t\t</td>";
            }
        }
        else if (tv) {
            // We have a TableVerifier, so we need to recurse
            html << "<td><table>\n"
                 << "\t<thead>\n"
                 << "\t\t<tr>\n"
                 << "\t\t\t<th></th>\n"
                 << "\t\t\t<th>Key</th>\n"
                 << "\t\t\t<th>Optional</th>\n"
                 << "\t\t\t<th>Type</th>\n"
                 << "\t\t\t<th>Restrictions</th>\n"
                 << "\t\t\t<th>Documentation</th>\n"
                 << "\t\t</tr>\n"
                 << "\t</thead>\n"
                 << "\t<tbody>\n"
                 << generateHtmlDocumentation({ "", "", tv->documentations })
                 << "\t</tbody>\n"
                 << "</table>\n"
                 << "</td>\n";
        }
        else {
            html << "\t\t<td>" << p.verifier->documentation() << "</td>\n";
        }
        html << "\t\t<td>" << p.documentation << "</td>\n"
             << "\t</tr>\n";

    }

    return html.str();
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

    std::string jsonString = "";
    for (const char& c : json.str()) {
        switch (c) {
            case '\'':
                jsonString += "\\'";
                break;
            default:
                jsonString += c;
        }
    }

    return jsonString;
}

void DocumentationEngine::addDocumentation(Documentation doc) {
    if (doc.id.empty()) {
        _documentations.push_back(std::move(doc));
    }
    else {
        auto it = std::find_if(
            _documentations.begin(),
            _documentations.end(),
            [doc](const Documentation& d) { return doc.id == d.id; }
        );

        if (it != _documentations.end()) {
            throw DuplicateDocumentationException(std::move(doc));
        }
        else {
            _documentations.push_back(std::move(doc));
        }
    }
}

std::vector<Documentation> DocumentationEngine::documentations() const {
    return _documentations;
}

} // namespace documentation
} // namespace openspace
