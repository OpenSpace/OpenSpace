/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/documentation/verifier.h>

#include <ghoul/misc/assert.h>

#include <fstream>

#include <fmt/format.h>

namespace openspace {
namespace documentation {

DocumentationEngine::DuplicateDocumentationException::DuplicateDocumentationException(
                    Documentation documentation)
    : ghoul::RuntimeError(fmt::format(
        "Duplicate Documentation with name '{}' and id '{}'",
        documentation.name,
        documentation.id
    ))
    , documentation(std::move(documentation))
{}

DocumentationEngine& DocumentationEngine::ref() {
    static DocumentationEngine engine;
    return engine;
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
        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        if (tv) {
            // We have a TableVerifier, so we need to recurse
            ++indentLevel;
            result += generateTextDocumentation({ "", "", tv->documentations }, indentLevel);
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
    result << "\"entries\": [";
    for (const auto& p : d.entries) {
        result << "{";
        result << "\"key\": \"" << p.key << "\",";
        result << "\"optional\": \"" << (p.optional ? "true" : "false") << "\",";
        result << "\"type\": \"" << p.verifier->type() << "\",";
        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        if (tv) {
            std::string json = generateJsonDocumentation({ "", "", tv->documentations });
            // We have a TableVerifier, so we need to recurse
            result << "\"restrictions\": " << json << ",";
        }
        else {
            result << "\"restrictions\": \"" << p.verifier->documentation() << "\",";
        }
        result << "},";
    }

    result << ']';
    result << "}";

    return result.str();
}

std::string generateHtmlDocumentation(const Documentation& d) {
    std::stringstream html;

    html << "\t<tr>\n"
         << "\t\t<td colspan=6>" << d.name << "</td>\n";

    for (const auto& p : d.entries) {
        html << "\t<tr>\n"
             << "\t\t<td></td>\n"
             << "\t\t<td>" << p.key << "</td>\n"
             << "\t\t<td>" << (p.optional ? "true" : "false") << "</td>\n"
             << "\t\t<td>" << p.verifier->type() << "</td>\n";
        TableVerifier* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
        if (tv) {
            // We have a TableVerifier, so we need to recurse
            html << "<td><table cellpadding=3 cellspacing=0 border=1>\n"
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

void DocumentationEngine::writeDocumentation(const std::string& f, const std::string& t) {
    if (t == "text") {
        std::ofstream file;
        file.exceptions(~std::ofstream::goodbit);
        file.open(f);

        for (const Documentation& d : _documentations) {
            int indent = 0;
            file << documentation::generateTextDocumentation(d, indent) << "\n\n";
        }
    }
    else if (t == "html") {
        std::ofstream file;
        file.exceptions(~std::ofstream::goodbit);
        file.open(f);

#ifdef JSON
        std::stringstream json;
        json << "[";

        for (const Documentation& d : _documentations) {
            json << generateJsonDocumentation(d);
            json << ",";
        }

        json << "]";

        std::string jsonText = json.str();
#else
        std::stringstream html;

        html << "<html>\n"
            << "\t<head>\n"
            << "\t\t<title>Documentation</title>\n"
            << "\t</head>\n"
            << "<body>\n";


        html << "<table cellpadding=3 cellspacing=0 border=1>\n"
            << "\t<caption>Documentation</caption>\n\n"
            << "\t<thead>\n"
            << "\t\t<tr>\n"
            << "\t\t\t<th rowspan=2>Name</th>\n"
            << "\t\t</tr>\n"
            << "\t\t<tr>\n"
            << "\t\t\t<th>Key</th>\n"
            << "\t\t\t<th>Optional</th>\n"
            << "\t\t\t<th>Type</th>\n"
            << "\t\t\t<th>Restrictions</th>\n"
            << "\t\t\t<th>Documentation</th>\n"
            << "\t\t</tr>\n"
            << "\t</thead>\n"
            << "\t<tbody>\n";

        for (const Documentation& d : _documentations) {
            html << generateHtmlDocumentation(d);

            html << "\t<tr><td style=\"line-height: 50px;\" colspan=6></br></td></tr>\n";
        }

        html << "\t</tbody>\n"
            << "</table>\n";

        html << "</body>\n";
        html << "</html>\n";

        file << html.str();
#endif
    }
}

void DocumentationEngine::addDocumentation(Documentation doc) {
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

} // namespace documentation
} // namespace openspace
