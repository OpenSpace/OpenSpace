/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/format.h>
#include <openspace/interaction/action.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/assert.h>
#include <openspace/misc/dictionary.h>
#include <openspace/misc/exception.h>
#include <openspace/misc/profiling.h>
#include <openspace/misc/stringhelper.h>
#include <openspace/misc/templatefactory.h>
#include <openspace/properties/property.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/profile.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/json_helper.h>
#include <openspace/util/keys.h>
#include <algorithm>
#include <array>
#include <cctype>
#include <fstream>
#include <future>
#include <limits>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "DocumentationEngine";

    // General keys
    constexpr std::string_view NameKey = "name";
    constexpr std::string_view IdentifierKey = "identifier";
    constexpr std::string_view DescriptionKey = "description";
    constexpr std::string_view DataKey = "data";
    constexpr std::string_view TypeKey = "type";
    constexpr std::string_view DocumentationKey = "documentation";
    constexpr std::string_view ActionKey = "action";
    constexpr std::string_view IdKey = "id";

    // Actions
    constexpr std::string_view ActionTitle = "Actions";
    constexpr std::string_view GuiNameKey = "guiName";
    constexpr std::string_view CommandKey = "command";
    constexpr std::string_view ColorKey = "color";
    constexpr std::string_view TextColorKey = "textColor";

    // Factory
    constexpr std::string_view MembersKey = "members";
    constexpr std::string_view OptionalKey = "optional";
    constexpr std::string_view ReferenceKey = "reference";
    constexpr std::string_view FoundKey = "found";
    constexpr std::string_view ClassesKey = "classes";

    constexpr std::string_view OtherName = "Other";
    constexpr std::string_view OtherIdentifierName = "other";
    constexpr std::string_view PropertyOwnerName = "propertyOwner";
    constexpr std::string_view CategoryName = "category";

    // Properties
    constexpr std::string_view SettingsTitle = "Settings";
    constexpr std::string_view SceneTitle = "Scene";
    constexpr std::string_view PropertiesKeys = "properties";
    constexpr std::string_view PropertyOwnersKey = "propertyOwners";
    constexpr std::string_view TagsKey = "tags";
    constexpr std::string_view UriKey = "uri";

    // Scripting
    constexpr std::string_view DefaultValueKey = "defaultValue";
    constexpr std::string_view ArgumentsKey = "arguments";
    constexpr std::string_view ReturnTypeKey = "returnType";
    constexpr std::string_view HelpKey = "help";
    constexpr std::string_view FileKey = "file";
    constexpr std::string_view LineKey = "line";
    constexpr std::string_view FullNameKey = "fullName";
    constexpr std::string_view FunctionsKey = "functions";
    constexpr std::string_view SourceLocationKey = "sourceLocation";
    constexpr std::string_view OpenSpaceScriptingKey = "openspace";

    // Licenses
    constexpr std::string_view LicensesTitle = "Licenses";
    constexpr std::string_view ProfileName = "Profile";
    constexpr std::string_view AssetsName = "Assets";
    constexpr std::string_view LicensesName = "Licenses";
    constexpr std::string_view NoLicenseName = "No License";

    constexpr std::string_view ProfileNameKey = "profileName";
    constexpr std::string_view VersionKey = "version";
    constexpr std::string_view AuthorKey = "author";
    constexpr std::string_view UrlKey = "url";
    constexpr std::string_view LicenseKey = "license";
    constexpr std::string_view IdentifiersKey = "identifiers";
    constexpr std::string_view PathKey = "path";
    constexpr std::string_view AssetKey = "assets";
    constexpr std::string_view LicensesKey = "licenses";

    // Keybindings
    constexpr std::string_view KeybindingsTitle = "Keybindings";
    constexpr std::string_view KeybindingsKey = "keybindings";

    // Events
    constexpr std::string_view EventsTitle = "Events";
    constexpr std::string_view FiltersKey = "filters";
    constexpr std::string_view ActionsKey = "actions";

    nlohmann::json documentationToJson(const Documentation& documentation) {
        nlohmann::json json;

        json[NameKey] = documentation.name;
        json[IdentifierKey] = documentation.id;
        json[DescriptionKey] = documentation.description;
        json[MembersKey] = nlohmann::json::array();

        for (const DocumentationEntry& p : documentation.entries) {
            nlohmann::json entry;
            entry[NameKey] = p.key;
            entry[OptionalKey] = p.optional.value;
            entry[TypeKey] = p.verifier->type();
            entry[DocumentationKey] = p.documentation;

            auto* tv = dynamic_cast<TableVerifier*>(p.verifier.get());
            auto* rv = dynamic_cast<ReferencingVerifier*>(p.verifier.get());

            if (rv) {
                const std::vector<Documentation>& doc = DocEng.documentations();
                auto it = std::find_if(
                    doc.begin(),
                    doc.end(),
                    [rv](const Documentation& d) { return d.id == rv->identifier; }
                );

                if (it == doc.end()) {
                    entry[ReferenceKey][FoundKey] = false;
                }
                else {
                    nlohmann::json reference;
                    reference[FoundKey] = true;
                    reference[NameKey] = it->name;
                    reference[IdentifierKey] = rv->identifier;

                    entry[ReferenceKey] = reference;
                }
            }
            else if (tv) {
                Documentation doc = { .entries = tv->documentations };

                // Since this is a table we need to recurse this function to extract data
                nlohmann::json tableDocs = documentationToJson(doc);

                // Set the members entry to the members of the table to remove unnecessary
                // nestling
                entry[MembersKey] = tableDocs[MembersKey];
            }
            else {
                entry[DescriptionKey] = p.verifier->documentation();
            }
            json[MembersKey].push_back(entry);
        }
        sortJson(json[MembersKey], NameKey);

        return json;
    }

    nlohmann::json propertyOwnerToJson(PropertyOwner* owner) {
        ZoneScoped;

        nlohmann::json json;
        json[NameKey] =
            !owner->guiName().empty() ? owner->guiName() : owner->identifier();

        json[DescriptionKey] = owner->description();
        json[PropertiesKeys] = nlohmann::json::array();
        json[PropertyOwnersKey] = nlohmann::json::array();
        json[TypeKey] = owner->type();
        json[TagsKey] = owner->tags();

        for (Property* p : owner->properties()) {
            nlohmann::json propertyJson;
            std::string name = !p->guiName().empty() ? p->guiName() : p->identifier();
            propertyJson[NameKey] = name;
            propertyJson[TypeKey] = p->className();
            propertyJson[UriKey] = p->uri();
            propertyJson[IdentifierKey] = p->identifier();
            propertyJson[DescriptionKey] = p->description();

            json[PropertiesKeys].push_back(propertyJson);
        }
        sortJson(json[PropertiesKeys], NameKey);

        for (PropertyOwner* o : owner->propertySubOwners()) {
            nlohmann::json propertyOwner;
            json[PropertyOwnersKey].push_back(propertyOwnerToJson(o));
        }
        sortJson(json[PropertyOwnersKey], NameKey);

        return json;
    }

    // Remove all double whitespaces from the helptext (these may be generated when using
    // multi-line strings in Lua)
    std::string cleanHelpText(std::string helpText) {
        trimWhitespace(helpText);
        size_t doubleSpace = helpText.find("  ");
        while (doubleSpace != std::string::npos) {
            helpText.erase(doubleSpace, 1);
            doubleSpace = helpText.find("  ");
        }
        return helpText;
    }

    nlohmann::json luaFunctionToJson(const LuaLibrary::Function& f,
                                     bool includeSourceLocation)
    {
        nlohmann::json function;
        function[NameKey] = f.name;
        nlohmann::json arguments = nlohmann::json::array();

        for (const LuaLibrary::Function::Argument& arg : f.arguments) {
            nlohmann::json argument;
            argument[NameKey] = arg.name;
            argument[TypeKey] = arg.type;
            argument[DefaultValueKey] = arg.defaultValue.value_or("");
            arguments.push_back(argument);
        }

        function[ArgumentsKey] = arguments;
        function[ReturnTypeKey] = f.returnType;

        function[HelpKey] = cleanHelpText(f.helpText);

        if (includeSourceLocation) {
            nlohmann::json sourceLocation;
            sourceLocation[FileKey] = f.sourceLocation.file;
            sourceLocation[LineKey] = f.sourceLocation.line;
            function[SourceLocationKey] = sourceLocation;
        }

        return function;
    }

    std::string_view trimView(std::string_view s) {
        while (!s.empty() && ::isspace(static_cast<unsigned char>(s.front()))) {
            s.remove_prefix(1);
        }
        while (!s.empty() && ::isspace(static_cast<unsigned char>(s.back()))) {
            s.remove_suffix(1);
        }
        return s;
    }

    // Turns each line of the text into a LuaLS comment line
    std::string luaLsComment(std::string_view text) {
        std::string res;
        size_t start = 0;
        while (!text.empty() && start <= text.size()) {
            size_t end = text.find('\n', start);
            if (end == std::string_view::npos) {
                end = text.size();
            }
            const std::string_view line = trimView(text.substr(start, end - start));
            res += std::format("---{}{}\n", line.empty() ? "" : " ", line);
            start = end + 1;
        }
        return res;
    }

    std::string singleLine(std::string_view text) {
        std::string res;
        bool hasPendingSpace = false;
        for (const char c : text) {
            if (::isspace(static_cast<unsigned char>(c))) {
                hasPendingSpace = !res.empty();
                continue;
            }
            if (hasPendingSpace) {
                res += ' ';
                hasPendingSpace = false;
            }
            res += c;
        }
        return res;
    }

    bool isLuaIdentifier(std::string_view name) {
        if (name.empty() || ::isdigit(static_cast<unsigned char>(name.front()))) {
            return false;
        }
        return std::all_of(
            name.begin(),
            name.end(),
            [](char c) { return ::isalnum(static_cast<unsigned char>(c)) || c == '_'; }
        );
    }

    // Removes everything that is not allowed in a LuaLS class name
    std::string luaLsClassName(std::string_view name) {
        std::string res;
        for (const char c : name) {
            if (::isalnum(static_cast<unsigned char>(c)) || c == '_') {
                res += c;
            }
        }
        if (res.empty()) {
            return "Unnamed";
        }
        if (::isdigit(static_cast<unsigned char>(res.front()))) {
            res.insert(res.begin(), '_');
        }
        return res;
    }

    // Maps the Verifier::type of the verifiers that are not handled separately
    std::string luaLsVerifierType(const std::string& type) {
        if (type == "Boolean") {
            return "boolean";
        }
        // Lua only has one number type and the engine accepts doubles for integers
        if (type == "Double" || type == "Integer") {
            return "number";
        }
        if (type == "String" || type == "Identifier" || type == "File" ||
            type == "Directory" || type == "Date and time")
        {
            return "string";
        }
        if (type.starts_with("Vector")) {
            if (type.ends_with("<bool>")) {
                return "boolean[]";
            }
            return "number[]";
        }
        if (type.starts_with("Matrix") || type.starts_with("Color")) {
            return "number[]";
        }
        return "any";
    }

    // Generates the LuaLS classes for the Documentation%s and the factories
    struct LuaLsTypeWriter {
        /// Maps the identifier of a Documentation to the LuaLS type that describes it
        std::map<std::string, std::string> idToType;

        // Nested tables are written as classes into `out` and referenced by name
        std::string typeOf(const Verifier& verifier, const std::string& nestedName,
                           std::string& out) const
        {
            if (const auto* v = dynamic_cast<const OrVerifier*>(&verifier)) {
                std::string res;
                for (size_t i = 0; i < v->values.size(); i++) {
                    if (i != 0) {
                        res += '|';
                    }
                    res +=
                        typeOf(*v->values[i], std::format("{}_{}", nestedName, i), out);
                }
                return res;
            }

            // Has to be before the TableVerifier as it is a subclass of it
            if (const auto* v = dynamic_cast<const ReferencingVerifier*>(&verifier)) {
                const auto it = idToType.find(v->identifier);
                return it != idToType.end() ? it->second : "table";
            }

            if (const auto* v = dynamic_cast<const StringInListVerifier*>(&verifier)) {
                std::string res;
                for (const std::string& value : v->values) {
                    if (value.find_first_of("\"\\") != std::string::npos) {
                        return "string";
                    }
                    res += std::format("{}\"{}\"", res.empty() ? "" : "|", value);
                }
                return res.empty() ? "string" : res;
            }

            if (dynamic_cast<const StringListVerifier*>(&verifier)) {
                return "string[]";
            }
            if (dynamic_cast<const IntListVerifier*>(&verifier)) {
                return "number[]";
            }

            if (const auto* v = dynamic_cast<const TableVerifier*>(&verifier)) {
                if (v->documentations.empty()) {
                    return "table";
                }

                const bool onlyWildcards = std::all_of(
                    v->documentations.begin(),
                    v->documentations.end(),
                    [](const DocumentationEntry& e) {
                        return e.key == DocumentationEntry::Wildcard;
                    }
                );
                if (!onlyWildcards) {
                    writeClass(out, nestedName, "", "", v->documentations, std::nullopt);
                    return nestedName;
                }

                // Lists and dictionaries are indistinguishable in the documentation
                std::string element;
                for (size_t i = 0; i < v->documentations.size(); i++) {
                    if (i != 0) {
                        element += '|';
                    }
                    element += typeOf(
                        *v->documentations[i].verifier,
                        std::format("{}.Element{}", nestedName, i == 0 ? "" : "2"),
                        out
                    );
                }
                const std::string array =
                    element.find('|') != std::string::npos ?
                    std::format("({})[]", element) :
                    std::format("{}[]", element);
                return std::format("{}|table<string, {}>", array, element);
            }

            return luaLsVerifierType(verifier.type());
        }

        // The `typeField` is the type of the `Type` key, which is used by factories
        void writeClass(std::string& out, const std::string& name,
                        const std::string& base, std::string_view description,
                        const std::vector<DocumentationEntry>& entries,
                        const std::optional<std::string>& typeField) const
        {
            std::string fields;
            if (typeField.has_value()) {
                fields += std::format("---@field Type {}\n", *typeField);
            }

            for (const DocumentationEntry& e : entries) {
                if (e.isPrivate || (typeField.has_value() && e.key == "Type")) {
                    continue;
                }

                const bool isWildcard = e.key == DocumentationEntry::Wildcard;
                if (!isWildcard && !isLuaIdentifier(e.key)) {
                    continue;
                }

                const std::string nested = std::format(
                    "{}.{}",
                    name, isWildcard ? "Entry" : e.key
                );
                const std::string type = typeOf(*e.verifier, nested, out);
                const std::string doc = singleLine(e.documentation);
                const std::string key =
                    isWildcard ? "[string]" : e.key + (e.optional ? "?" : "");
                fields += std::format(
                    "---@field {} {}{}{}\n", key, type, doc.empty() ? "" : " ", doc
                );
            }

            out += luaLsComment(description);
            out += base.empty() ?
                std::format("---@class {}\n", name) :
                std::format("---@class {} : {}\n", name, base);
            out += fields;
            out += '\n';
        }
    };

    // Returns the content of one file per factory and one for all remaining
    // documentations
    std::map<std::string, std::string> luaLsTypeFiles(
                                    const std::vector<Documentation>& docs,
                                const std::vector<FactoryManager::FactoryInfo>& factories)
    {
        constexpr size_t None = std::numeric_limits<size_t>::max();

        std::set<std::string> usedNames;
        auto uniqueName = [&usedNames](const std::string& name) {
            std::string res = name;
            int i = 2;
            while (!usedNames.insert(res).second) {
                res = std::format("{}_{}", name, i);
                i++;
            }
            return res;
        };

        std::vector<bool> isConsumed = std::vector<bool>(docs.size(), false);
        auto findDoc = [&](const std::string& name) {
            for (size_t i = 0; i < docs.size(); i++) {
                if (!isConsumed[i] && docs[i].name == name) {
                    isConsumed[i] = true;
                    return i;
                }
            }
            return None;
        };

        struct Class {
            std::string registeredName;
            std::string luaName;
            size_t doc = None;
        };
        struct Factory {
            std::string registeredName;
            std::string alias;
            std::string base;
            // The class of the global table that contains the constructor functions
            std::string constructors;
            size_t baseDoc = None;
            std::vector<Class> classes;
        };

        LuaLsTypeWriter writer;
        auto registerId = [&](size_t doc, const std::string& type) {
            if (doc != None && !docs[doc].id.empty()) {
                writer.idToType[docs[doc].id] = type;
            }
        };

        // All names have to be known before writing as documentations reference others
        std::vector<Factory> factoryEntries;
        for (const FactoryManager::FactoryInfo& info : factories) {
            if (info.name.empty()) {
                continue;
            }
            Factory f = {
                .registeredName = info.name,
                .alias = uniqueName(luaLsClassName(info.name)),
                .base = uniqueName(f.alias + "Base"),
                .constructors = uniqueName(f.alias + "Constructors"),
                .baseDoc = findDoc(info.name)
            };
            registerId(f.baseDoc, f.alias);

            for (const std::string& c : info.factory->registeredClasses()) {
                if (c.empty()) {
                    continue;
                }
                Class cls = {
                    .registeredName = c,
                    .luaName = uniqueName(luaLsClassName(c)),
                    .doc = findDoc(c)
                };
                registerId(cls.doc, cls.luaName);
                f.classes.push_back(std::move(cls));
            }
            factoryEntries.push_back(std::move(f));
        }

        std::vector<std::pair<std::string, size_t>> others;
        for (size_t i = 0; i < docs.size(); i++) {
            if (isConsumed[i] || docs[i].id.empty()) {
                continue;
            }
            std::string name = uniqueName(luaLsClassName(docs[i].name));
            registerId(i, name);
            others.emplace_back(std::move(name), i);
        }

        const std::vector<DocumentationEntry> noEntries;
        std::map<std::string, std::string> files;
        for (const Factory& f : factoryEntries) {
            std::string out = "---@meta\n\n";
            if (f.baseDoc != None) {
                writer.writeClass(
                    out,
                    f.base,
                    "",
                    docs[f.baseDoc].description,
                    docs[f.baseDoc].entries,
                    "string"
                );
            }
            else {
                writer.writeClass(out, f.base, "", "", noEntries, "string");
            }

            std::string alias;
            std::string constructors;
            for (const Class& c : f.classes) {
                const std::string_view description =
                    c.doc != None ? std::string_view(docs[c.doc].description) : "";
                writer.writeClass(
                    out,
                    c.luaName,
                    f.base,
                    description,
                    c.doc != None ? docs[c.doc].entries : noEntries,
                    std::format("\"{}\"", c.registeredName)
                );
                alias += std::format("{}{}", alias.empty() ? "" : "|", c.luaName);

                // Has to match the functions created in ScriptEngine::initializeLuaState
                if (isLuaIdentifier(f.registeredName) &&
                    isLuaIdentifier(c.registeredName))
                {
                    constructors += luaLsComment(description);
                    constructors += std::format(
                        "---@return {}\nfunction {}.{}() end\n\n",
                        c.luaName, f.registeredName, c.registeredName
                    );
                }
            }
            out += std::format(
                "---@alias {} {}\n",
                f.alias, alias.empty() ? f.base : alias
            );

            if (isLuaIdentifier(f.registeredName)) {
                out += std::format(
                    "\n---@class {}\n{} = {{}}\n\n{}",
                    f.constructors, f.registeredName, constructors
                );
            }
            files[f.alias] = std::move(out);
        }

        std::string out = "---@meta\n\n";
        for (const std::pair<std::string, size_t>& o : others) {
            writer.writeClass(
                out,
                o.first,
                "",
                docs[o.second].description,
                docs[o.second].entries,
                std::nullopt
            );

            // Has to match the function created in ScriptEngine::initializeLuaState
            if (o.first == "SceneGraphNode") {
                out += "---@return SceneGraphNode\nfunction SceneGraphNode() end\n\n";
            }
        }
        files["Other"] = std::move(out);

        return files;
    }

    std::string luaLsBaseType(std::string_view type) {
        // First deal with the individual types
        if (type.ends_with("[]")) {
            type.remove_suffix(2);
            return luaLsBaseType(trimView(type)) + "[]";
        }
        else if (type == "String" || type == "Path") {  return "string"; }
        else if (type == "Number") { return "number"; }
        else if (type == "Integer") { return "integer"; }
        else if (type == "Boolean") { return "boolean"; }
        else if (type == "Table") { return "table"; }
        else if (type == "Function") { return "function"; }
        else if (type == "Nil") { return "nil"; }
        else if (type == "vec2" || type == "vec3" || type == "vec4" ||
            type == "dvec2" || type == "dvec3" || type == "dvec4" ||
            type == "ivec2" || type == "ivec3" || type == "ivec4" ||
            type == "mat2x2" || type == "mat3x3" || type == "mat4x4" ||
            type == "dmat2x2" || type == "dmat3x3" || type == "dmat4x4")
        {
            return "number[]";
        }

        // If we got here there is a chance we are dealing with a multiple return value
        if (type.starts_with('(') && type.ends_with(')')) {
            // We have a (Number, Number, Numer
            type.remove_prefix(1);
            type.remove_suffix(1);

            std::vector<std::string_view> parts = tokenizeString(type, ',');
            std::string result = std::accumulate(
                parts.begin(),
                parts.end(),
                std::string(),
                [](std::string lhs, std::string_view rhs) {
                    return std::format(
                        "{} {}", std::move(lhs), luaLsBaseType(trimView(rhs))
                    );
                }
            );

            // We accidentally add a leading space with the accumulate call
            return result.substr(1);
        }


        // Named types have no LuaLS counterpart
        return "any";
    }

    struct LuaLsType {
        std::string type;
        bool isOptional = false;
    };

    // Converts OpenSpace type strings into LuaLS format
    LuaLsType toLuaLsType(std::string_view type) {
        LuaLsType result;
        size_t start = 0;
        while (start <= type.size()) {
            size_t end = type.find('|', start);
            if (end == std::string_view::npos) {
                end = type.size();
            }
            std::string_view part = trimView(type.substr(start, end - start));
            if (part.ends_with('?')) {
                result.isOptional = true;
                part = trimView(part.substr(0, part.size() - 1));
            }
            if (!part.empty()) {
                if (!result.type.empty()) {
                    result.type += '|';
                }
                result.type += luaLsBaseType(part);
            }
            start = end + 1;
        }
        if (result.type.empty()) {
            result.type = "any";
        }
        return result;
    }

    // Escape parameter names that would be reserved keywords in Lua
    std::string luaLsParameterName(std::string_view name) {
        constexpr std::array<std::string_view, 22> Keywords = {
            "and", "break", "do", "else", "elseif", "end", "false", "for", "function",
            "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then",
            "true", "until", "while"
        };
        if (name.empty()) {
            return "arg";
        }
        if (std::find(Keywords.begin(), Keywords.end(), name) != Keywords.end()) {
            return std::format("{}_", name);
        }
        return std::string(name);
    }

    std::string luaLsFunction(const LuaLibrary::Function& f, const std::string& table) {
        std::string res = luaLsComment(cleanHelpText(f.helpText));

        std::string parameters;
        for (const LuaLibrary::Function::Argument& arg : f.arguments) {
            const LuaLsType t = toLuaLsType(arg.type);
            const std::string name = luaLsParameterName(arg.name);
            const bool optional = t.isOptional || arg.defaultValue.has_value();
            res += std::format("---@param {}{} {}\n", name, optional ? "?" : "", t.type);
            if (!parameters.empty()) {
                parameters += ", ";
            }
            parameters += name;
        }

        if (!f.returnType.empty()) {
            const LuaLsType t = toLuaLsType(f.returnType);
            res += std::format("---@return {}{}\n", t.type, t.isOptional ? "?" : "");
        }

        res += std::format("function {}.{}({}) end\n\n", table, f.name, parameters);
        return res;
    }

    // Recurses into sublibraries since each of them is a nested table in the Lua state
    void appendLuaLsLibrary(std::string& out, const LuaLibrary& library,
                            const std::string& parentTable)
    {
        std::string table = parentTable;
        if (!library.name.empty()) {
            table = std::format("{}.{}", parentTable, library.name);
            out += std::format("{} = {{}}\n\n", table);
        }

        std::vector<const LuaLibrary::Function*> functions;
        for (const LuaLibrary::Function& f : library.functions) {
            functions.push_back(&f);
        }
        for (const LuaLibrary::Function& f : library.documentations) {
            functions.push_back(&f);
        }
        std::sort(
            functions.begin(),
            functions.end(),
            [](const LuaLibrary::Function* lhs, const LuaLibrary::Function* rhs) {
                return lhs->name < rhs->name;
            }
        );
        for (const LuaLibrary::Function* f : functions) {
            out += luaLsFunction(*f, table);
        }

        for (const LuaLibrary& sub : library.subLibraries) {
            appendLuaLsLibrary(out, sub, table);
        }
    }
} // namespace

namespace openspace {

DocumentationEngine* DocumentationEngine::_instance = nullptr;

DocumentationEngine::DocumentationEngine() {}

void DocumentationEngine::initialize() {
    assert_msg(!isInitialized(), "DocumentationEngine is already initialized");
    _instance = new DocumentationEngine;
}

void DocumentationEngine::deinitialize() {
    assert_msg(isInitialized(), "DocumentationEngine is not initialized");
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
        registerCoreSchemas(*_instance);
    }
    return *_instance;
}

nlohmann::json DocumentationEngine::generateScriptEngineJson() const {
    ZoneScoped;

    const std::vector<LuaLibrary> libraries = global::scriptEngine->allLuaLibraries();
    nlohmann::json json;

    for (const LuaLibrary& l : libraries) {
        nlohmann::json library;
        std::string libraryName = l.name;
        library[NameKey] = libraryName;
        std::string os = std::string(OpenSpaceScriptingKey);
        library[FullNameKey] =
            libraryName.empty() ? os : std::format("{}.{}", os, libraryName);

        for (const LuaLibrary::Function& f : l.functions) {
            constexpr bool HasSourceLocation = true;
            library[FunctionsKey].push_back(luaFunctionToJson(f, HasSourceLocation));
        }

        for (const LuaLibrary::Function& f : l.documentations) {
            constexpr bool HasSourceLocation = false;
            library[FunctionsKey].push_back(luaFunctionToJson(f, HasSourceLocation));
        }
        sortJson(library[FunctionsKey], NameKey);
        json.push_back(library);

        sortJson(json, NameKey);
    }
    return json;
}

std::string DocumentationEngine::generateLuaDefinitions() const {
    ZoneScoped;

    const std::string os = std::string(OpenSpaceScriptingKey);
    std::string result = std::format(
        "---@meta\n\n---@class {0}\n{0} = {{}}\n\n", os
    );
    for (const LuaLibrary& l : global::scriptEngine->allLuaLibraries()) {
        appendLuaLsLibrary(result, l, os);
    }
    return result;
}

std::map<std::string, std::string> DocumentationEngine::generateLuaTypes() const {
    ZoneScoped;

    return luaLsTypeFiles(_documentations, FactoryManager::ref().factories());
}

nlohmann::json DocumentationEngine::generateLicenseGroupsJson() const {
    nlohmann::json json;

    if (global::profile->meta.has_value()) {
        Profile::Meta meta = *global::profile->meta;

        nlohmann::json metaJson;
        metaJson[NameKey] = ProfileName;
        metaJson[ProfileNameKey] = meta.name.value_or("");
        metaJson[VersionKey] = meta.version.value_or("");
        metaJson[DescriptionKey] = meta.description.value_or("");
        metaJson[AuthorKey] = meta.author.value_or("");
        metaJson[UrlKey] = meta.url.value_or("");
        metaJson[LicenseKey] = meta.license.value_or("");
        json.push_back(std::move(metaJson));
    }

    // Go through all assets and group them in a map with the key as the license name
    std::vector<const Asset*> assets =
        global::openSpaceEngine->assetManager().allAssets();

    std::map<std::string, nlohmann::json> assetLicenses;
    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();

        // Ensure the license is not going to be an empty string
        std::string licenseName = std::string(NoLicenseName);
        if (meta.has_value() && !meta->license.empty()) {
            licenseName = meta->license;
        }

        nlohmann::json assetJson;
        assetJson[NameKey] = meta.has_value() ? meta->name : "";
        assetJson[VersionKey] = meta.has_value() ? meta->version : "";
        assetJson[DescriptionKey] = meta.has_value() ? meta->description : "";
        assetJson[AuthorKey] = meta.has_value() ? meta->author : "";
        assetJson[UrlKey] = meta.has_value() ? meta->url : "";
        assetJson[LicenseKey] = licenseName;
        assetJson[PathKey] = asset->path().string();
        assetJson[IdKey] = asset->path().string();
        assetJson[IdentifiersKey] = meta.has_value() ? meta->identifiers :
            std::vector<std::string>();

        assetLicenses[licenseName].push_back(assetJson);
    }

    nlohmann::json assetsJson;
    assetsJson[NameKey] = AssetsName;
    assetsJson[TypeKey] = LicensesName;

    using K = std::string;
    using V = nlohmann::json;
    for (std::pair<const K, V>& assetLicense : assetLicenses) {
        nlohmann::json entry;
        entry[NameKey] = assetLicense.first;
        entry[AssetKey] = std::move(assetLicense.second);
        sortJson(entry[AssetKey], NameKey);
        assetsJson[LicensesKey].push_back(entry);
    }
    json.push_back(assetsJson);

    nlohmann::json result;
    result[NameKey] = LicensesTitle;
    result[DataKey] = json;
    return result;
}

nlohmann::json DocumentationEngine::generateLicenseListJson() const {
    nlohmann::json json;

    if (global::profile->meta.has_value()) {
        nlohmann::json profile;
        profile[NameKey] = global::profile->meta->name.value_or("");
        profile[VersionKey] = global::profile->meta->version.value_or("");
        profile[DescriptionKey] = global::profile->meta->description.value_or("");
        profile[AuthorKey] = global::profile->meta->author.value_or("");
        profile[UrlKey] = global::profile->meta->url.value_or("");
        profile[LicenseKey] = global::profile->meta->license.value_or("");
        json.push_back(profile);
    }

    std::vector<const Asset*> assets =
        global::openSpaceEngine->assetManager().allAssets();

    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();

        if (!meta.has_value()) {
            continue;
        }

        nlohmann::json assetJson;
        assetJson[NameKey] = meta->name;
        assetJson[VersionKey] = meta->version;
        assetJson[DescriptionKey] = meta->description;
        assetJson[AuthorKey] = meta->author;
        assetJson[UrlKey] = meta->url;
        assetJson[LicenseKey] = meta->license;
        assetJson[IdentifiersKey] = meta->identifiers;
        assetJson[PathKey] = asset->path().string();
        json.push_back(assetJson);
    }
    return json;
}

nlohmann::json DocumentationEngine::generateEventJson() const {
    using Type = Event::Type;
    const std::unordered_map<Type, std::vector<EventEngine::ActionInfo>>& eventActions =
        global::eventEngine->eventActions();
    nlohmann::json events;

    nlohmann::json data = nlohmann::json::array();

    // Group actions by events
    for (const auto& [eventType, actions] : eventActions) {
        nlohmann::json eventJson;

        eventJson[NameKey] = std::string(toString(eventType));
        nlohmann::json actionsJson = nlohmann::json::array();

        for (const EventEngine::ActionInfo& action : actions) {
            nlohmann::json actionJson;
            actionJson[NameKey] = eventJson[NameKey];
            actionJson[ActionKey] = action.action;
            // Create a unique ID
            actionJson[IdKey] = std::format("{}{}", action.action, action.id);

            // Output filters as a string
            if (action.filter.has_value()) {
                Dictionary filters = action.filter.value();
                std::vector<std::string_view> keys = filters.keys();
                nlohmann::json filtersJson = nlohmann::json::array();

                std::string filtersString = "";
                for (std::string_view key : keys) {
                    std::string value = filters.value<std::string>(key);
                    filtersString += std::format("{} = {}, ", key, value);
                }
                filtersString.pop_back(); // Remove last space from last entry
                filtersString.pop_back(); // Remove last comma from last entry

                actionJson[FiltersKey] = filtersString;

            }
            actionsJson.push_back(actionJson);
        }
        eventJson[ActionsKey] = actionsJson;
        data.push_back(eventJson);
    }

    // Format resulting json
    nlohmann::json result;
    result[NameKey] = EventsTitle;
    result[DataKey] = data;
    return result;
}

void DocumentationEngine::writeJsonSchema() {
    // Properties schema
    auto mergeDefs = [](nlohmann::json& target, const nlohmann::json& source) {
        for (const auto& [key, value] : source.items()) {
            if (target.contains(key)) {
                assert_msg(
                    target[key] == value,
                    std::format(
                        "Conflicting $def '{}': existing definition '{}' differs from "
                        "incomming definition '{}'. Each $def name must be unique and/or "
                        "identical across all property schemas.",
                        key, target[key].get<std::string>(), value.get<std::string>()
                    )
                );
                // identical, skip
                continue;
            }
            target[key] = value;
        }
    };

    nlohmann::json defs;
    nlohmann::json anyProperty = nlohmann::json::array();
    nlohmann::json anyPropertyMetaData = nlohmann::json::array();

    for (const nlohmann::json& propertySchema : _propertySchemas) {
        // Add any global $defs
        if (propertySchema.contains("$defs")) {
            mergeDefs(defs, propertySchema["$defs"]);
        }

        // Add property typedef
        if (propertySchema.contains("typedefs")) {
            // @TODO (anden88 2026-05-04): Should we guard for duplicate typeNames?
            for (const auto& [typeName, typeDef] : propertySchema["typedefs"].items()) {
                defs[typeName] = typeDef;
                anyProperty.push_back({
                    { "$ref", std::format("#/$defs/{}", typeName) }
                });

                // We also store the union of all properties final metadata shapes under
                // AnyPropertyMetaData this enables validation of the SubscriptionTopic,
                // otherwise the metaData shape would be defined as a plain JSON object

                // Add a named metaData def for this type so AnyPropertyMetaData generates
                // readable names like DoubleListPropertyMetaData
                const std::string metaDataName = std::format("{}MetaData", typeName);
                defs[metaDataName] = typeDef["properties"]["metaData"];
                anyPropertyMetaData.push_back({
                    { "$ref", std::format("#/$defs/{}", metaDataName) }
                });
            }
        }
    }

    defs["AnyPropertyMetaData"] = { { "anyOf", anyPropertyMetaData } };
    defs["AnyProperty"] = { { "anyOf", anyProperty } };
    defs["PropertyOwner"] = nlohmann::json::parse(R"(
        {
          "type": "object",
          "properties": {
            "identifier": { "type": "string" },
            "guiName": { "type": "string" },
            "description": { "type": "string" },
            "properties": {
              "type": "array",
              "items": { "$ref": "#/$defs/AnyProperty" }
            },
            "subowners": {
              "type": "array",
              "items": { "$ref": "#/$defs/PropertyOwner" }
            },
            "tags": {
              "type": "array",
              "items": { "type": "string" }
            },
            "uri": { "type": "string" }
          },
          "additionalProperties": false,
          "required": [
            "identifier",
            "guiName",
            "description",
            "properties",
            "subowners",
            "tags",
            "uri"
          ]
        }
    )");
    defs["JsonValue"] = nlohmann::json::parse(R"(
        {
          "anyOf": [
            { "type": "string" },
            { "type": "number" },
            { "type": "boolean" },
            {
              "type": "array",
              "items": { "$ref": "#/$defs/JsonValue" }
            },
            {
              "type": "object",
              "additionalProperties": { "$ref": "#/$defs/JsonValue" }
            },
            { "type": "null" }
          ]
        }
    )");

    nlohmann::json propertiesJson;
    propertiesJson["$schema"] = "https://json-schema.org/draft/2020-12/schema";
    propertiesJson["$defs"] = defs;
    // Generating a TypeScript .ts file will always create a default interface. We use a
    // dummy interface since all other properties and types are defined in $defs and does
    // not show up in the generated output ts file
    propertiesJson["title"] = "DummyInterface";
    propertiesJson["additionalProperties"] = false;
    const std::filesystem::path propertiesPath =
        absPath("${BASE}/support/types/properties.json");
    std::ofstream propertiesFile = std::ofstream(propertiesPath);

    if (!propertiesFile.good()) {
        throw RuntimeError(std::format(
            "Could not open properties file: '{}'", propertiesPath
        ));
    }
    propertiesFile << propertiesJson.dump(2);

    for (Schema& schema : _schemas) {
        const std::string file = std::format(
            "{}/support/types/{}.json", "${BASE}", schema.id
        );
        std::filesystem::path path = absPath(file);
        std::ofstream out = std::ofstream(path);
        if (out) {
            // Add which schema version we're targeting, see
            // https://json-schema.org/understanding-json-schema/reference/schema#schema
            schema.schema["$schema"] = "https://json-schema.org/draft/2020-12/schema";
            out << schema.schema.dump(2);
            out << '\n';
        }
    }
}

nlohmann::json DocumentationEngine::generateFactoryManagerJson() const {
    nlohmann::json json;

    std::vector<Documentation> docs = _documentations; // Copy the documentations
    const std::vector<FactoryManager::FactoryInfo>& factories =
        FactoryManager::ref().factories();

    for (const FactoryManager::FactoryInfo& factoryInfo : factories) {
        if (factoryInfo.name == "") {
            LERROR("Factory documentation without identifier");
            continue;
        }
        nlohmann::json factory;
        factory[NameKey] = factoryInfo.name;
        factory[IdentifierKey] = std::format("{}{}", CategoryName, factoryInfo.name);

        TemplateFactoryBase* f = factoryInfo.factory.get();
        // Add documentation about base class
        auto factoryDoc = std::find_if(
            docs.begin(),
            docs.end(),
            [&factoryInfo](const Documentation& d) { return d.name == factoryInfo.name; }
        );
        if (factoryDoc != docs.end()) {
            nlohmann::json documentation = documentationToJson(*factoryDoc);
            factory[ClassesKey].push_back(documentation);
            // Remove documentation from list check at the end if all docs got put in
            docs.erase(factoryDoc);
        }
        else {
            nlohmann::json documentation;
            documentation[NameKey] = factoryInfo.name;
            documentation[IdentifierKey] = factoryInfo.name;
            documentation[MembersKey] = nlohmann::json::array();
            factory[ClassesKey].push_back(documentation);
        }

        // Add documentation about derived classes
        const std::vector<std::string>& registeredClasses = f->registeredClasses();
        for (const std::string& c : registeredClasses) {
            if (c.empty()) {
                LERROR("Factory documentation, derived class, without identifier");
                continue;
            }
            auto found = std::find_if(
                docs.begin(),
                docs.end(),
                [&c](const Documentation& d) { return d.name == c; }
            );
            if (found != docs.end()) {
                nlohmann::json documentation = documentationToJson(*found);
                factory[ClassesKey].push_back(documentation);
                docs.erase(found);
            }
            else {
                nlohmann::json documentation;
                documentation[NameKey] = c;
                documentation[IdentifierKey] = c;
                documentation[MembersKey] = nlohmann::json::array();
                factory[ClassesKey].push_back(documentation);
            }
        }
        sortJson(factory[ClassesKey], NameKey);
        json.push_back(factory);
    }

    // Add all leftover docs
    nlohmann::json leftovers;
    leftovers[NameKey] = OtherName;
    leftovers[IdentifierKey] = OtherIdentifierName;

    for (const Documentation& doc : docs) {
        if (doc.id.empty()) {
            continue;
        }
        leftovers[ClassesKey].push_back(documentationToJson(doc));
    }
    sortJson(leftovers[ClassesKey], NameKey);
    json.push_back(leftovers);
    sortJson(json, NameKey);

    return json;
}

nlohmann::json DocumentationEngine::generateKeybindingsJson() const {
    ZoneScoped;

    nlohmann::json json;
    const std::multimap<KeyWithModifier, std::string>& luaKeys =
        global::keybindingManager->keyBindings();

    for (const std::pair<const KeyWithModifier, std::string>& p : luaKeys) {
        nlohmann::json keybind;
        keybind[NameKey] = to_string(p.first);
        keybind[ActionKey] = p.second;
        json.push_back(std::move(keybind));
    }
    sortJson(json, NameKey);

    nlohmann::json result;
    result[NameKey] = KeybindingsTitle;
    result[KeybindingsKey] = json;
    return result;
}

nlohmann::json DocumentationEngine::generatePropertyOwnerJson(PropertyOwner* owner) const
{
    ZoneScoped;

    assert_msg(owner, "Owner must not be nullptr");

    nlohmann::json json;
    std::vector<PropertyOwner*> subOwners = owner->propertySubOwners();
    for (PropertyOwner* o : subOwners) {
        if (o->identifier() != SceneTitle) {
            nlohmann::json jsonOwner = propertyOwnerToJson(o);

            json.push_back(jsonOwner);
        }
    }
    sortJson(json, NameKey);

    nlohmann::json result;
    result[NameKey] = PropertyOwnerName;
    result[DataKey] = json;

    return result;
}

void DocumentationEngine::writeJavascriptDocumentation() const {
    ZoneScoped;

    // Write documentation to json files if config file supplies path for doc files
    if (global::configuration->documentation.path.empty()) {
        // If path was empty, that means that no documentation is requested
        return;
    }

    // Start the async requests as soon as possible so they are finished when we need them
    std::future<nlohmann::json> settings = std::async(
        &DocumentationEngine::generatePropertyOwnerJson,
        this,
        global::rootPropertyOwner
    );

    std::future<nlohmann::json> sceneJson = std::async(
        &DocumentationEngine::generatePropertyOwnerJson,
        this,
        global::renderEngine->scene()
    );

    nlohmann::json keybindings = generateKeybindingsJson();
    nlohmann::json license = generateLicenseGroupsJson();
    nlohmann::json sceneProperties = settings.get();
    nlohmann::json sceneGraph = sceneJson.get();
    nlohmann::json actions = generateActionJson();
    nlohmann::json events = generateEventJson();

    sceneProperties[NameKey] = SettingsTitle;
    sceneGraph[NameKey] = SceneTitle;

    nlohmann::json documentation = {
        sceneGraph, sceneProperties, actions, events, keybindings, license
    };

    nlohmann::json result;
    result[DocumentationKey] = documentation;

    // Make into a JavaScript variable so that it is possible to open with static HTML
    std::ofstream out = std::ofstream(absPath("${DOCUMENTATION}/documentationData.js"));
    out << "var data = " << result.dump();
}

void DocumentationEngine::writeJsonDocumentation() const {
    // Write two json files for the static docs page - asset components and scripting API

    std::ofstream outFactory(absPath("${DOCUMENTATION}/assetComponents.json"));
    if (outFactory.good()) {
        nlohmann::json factory = generateFactoryManagerJson();
        outFactory << factory.dump();
    }

    std::ofstream outScription(absPath("${DOCUMENTATION}/scriptingApi.json"));
    if (outScription.good()) {
        nlohmann::json scripting = generateScriptEngineJson();
        outScription << scripting.dump();
    }

    // Definition files for the Lua Language Server, used for editor support in assets
    const std::filesystem::path luaDirectory = absPath("${DOCUMENTATION}/lua");
    std::error_code ec;
    std::filesystem::create_directories(luaDirectory, ec);

    std::ofstream outLuaDefinitions(luaDirectory / "openspace.d.lua");
    if (outLuaDefinitions.good()) {
        outLuaDefinitions << generateLuaDefinitions();
    }

    for (const std::pair<const std::string, std::string>& f : generateLuaTypes()) {
        std::ofstream outLuaTypes(luaDirectory / std::format("{}.d.lua", f.first));
        if (outLuaTypes.good()) {
            outLuaTypes << f.second;
        }
    }
}

nlohmann::json DocumentationEngine::generateActionJson() const {
    nlohmann::json res;
    res[NameKey] = ActionTitle;
    res[DataKey] = nlohmann::json::array();
    std::vector<Action> actions = global::actionManager->actions();

    for (const Action& action : actions) {
        nlohmann::json d;
        // Use identifier as name to make it more similar to the scripting API
        d[NameKey] = action.identifier;
        d[GuiNameKey] = action.name;
        d[DocumentationKey] = action.documentation;
        d[CommandKey] = action.command;
        if (action.color.has_value()) {
            d[ColorKey] = std::format("{}", action.color);
        }
        if (action.textColor.has_value()) {
            d[TextColorKey] = std::format("{}", action.textColor);
        }
        res[DataKey].push_back(d);
    }
    sortJson(res[DataKey], NameKey);
    return res;
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
            throw RuntimeError(std::format(
                "Duplicate Documentation with name '{}' and id '{}'",
                documentation.name, documentation.id
            ));
        }
        else {
            _documentations.push_back(std::move(documentation));
        }
    }
}

void DocumentationEngine::addSchema(Schema schema) {
    if (schema.id.empty()) {
        _schemas.push_back(std::move(schema));
    }
    else {
        auto it = std::find_if(
            _schemas.begin(),
            _schemas.end(),
            [schema](const Schema& s) { return schema.id == s.id; }
        );

        if (it != _schemas.end()) {
            throw RuntimeError(std::format("Duplicate Schema with id '{}'", schema.id));
        }
        else {
            _schemas.push_back(std::move(schema));
        }
    }
}

void DocumentationEngine::addPropertySchema(const nlohmann::json& schema) {
    _propertySchemas.push_back(schema);
}

std::vector<Documentation> DocumentationEngine::documentations() const {
    return _documentations;
}

} // namespace openspace
