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

#include <openspace/documentation/documentationengine.h>

#include <openspace/openspace.h>
#include <openspace/documentation/core_registration.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/configuration.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/action.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/json.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/asset.h>
#include <openspace/scene/assetmanager.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/json_helper.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/misc/profiling.h>
#include <fstream>
#include <future>

namespace {
    constexpr std::string_view _loggerCat = "DocumentationEngine";

    // General keys
    constexpr const char* NameKey = "name";
    constexpr const char* IdentifierKey = "identifier";
    constexpr const char* DescriptionKey = "description";
    constexpr const char* DataKey = "data";
    constexpr const char* TypeKey = "type";
    constexpr const char* DocumentationKey = "documentation";
    constexpr const char* ActionKey = "action";
    constexpr const char* IdKey = "id";

    // Actions
    constexpr const char* ActionTitle = "Actions";
    constexpr const char* GuiNameKey = "guiName";
    constexpr const char* CommandKey = "command";

    // Factory
    constexpr const char* FactoryTitle = "Asset Components";
    constexpr const char* MembersKey = "members";
    constexpr const char* OptionalKey = "optional";
    constexpr const char* ReferenceKey = "reference";
    constexpr const char* FoundKey = "found";
    constexpr const char* RestrictionsKey = "restrictions";
    constexpr const char* ClassesKey = "classes";

    constexpr const char* OtherName = "Other";
    constexpr const char* OtherIdentifierName = "other";
    constexpr const char* propertyOwnerName = "propertyOwner";
    constexpr const char* categoryName = "category";

    // Properties
    constexpr const char* SettingsTitle = "Settings";
    constexpr const char* SceneTitle = "Scene";
    constexpr const char* PropertiesKeys = "properties";
    constexpr const char* PropertyOwnersKey = "propertyOwners";
    constexpr const char* TagsKey = "tags";
    constexpr const char* UriKey = "uri";

    // Scripting
    constexpr const char* ScriptingTitle = "Scripting API";
    constexpr const char* DefaultValueKey = "defaultValue";
    constexpr const char* ArgumentsKey = "arguments";
    constexpr const char* ReturnTypeKey = "returnType";
    constexpr const char* HelpKey = "help";
    constexpr const char* FileKey = "file";
    constexpr const char* LineKey = "line";
    constexpr const char* LibraryKey = "library";
    constexpr const char* FullNameKey = "fullName";
    constexpr const char* FunctionsKey = "functions";
    constexpr const char* SourceLocationKey = "sourceLocation";
    constexpr const char* OpenSpaceScriptingKey = "openspace";

    // Licenses
    constexpr const char* LicensesTitle = "Licenses";
    constexpr const char* ProfileName = "Profile";
    constexpr const char* AssetsName = "Assets";
    constexpr const char* LicensesName = "Licenses";
    constexpr const char* NoLicenseName = "No License";
    constexpr const char* NoDataName = "";

    constexpr const char* ProfileNameKey = "profileName";
    constexpr const char* VersionKey = "version";
    constexpr const char* AuthorKey = "author";
    constexpr const char* UrlKey = "url";
    constexpr const char* LicenseKey = "license";
    constexpr const char* NoLicenseKey = "noLicense";
    constexpr const char* IdentifiersKey = "identifiers";
    constexpr const char* PathKey = "path";
    constexpr const char* AssetKey = "assets";
    constexpr const char* LicensesKey = "licenses";

    // Keybindings
    constexpr const char* KeybindingsTitle = "Keybindings";
    constexpr const char* KeybindingsKey = "keybindings";

    // Events
    constexpr const char* EventsTitle = "Events";
    constexpr const char* FiltersKey = "filters";
    constexpr const char* ActionsKey = "actions";

    nlohmann::json documentationToJson(const openspace::documentation::Documentation& d) {
        using namespace openspace::documentation;

        nlohmann::json json;

        json[NameKey] = d.name;
        json[IdentifierKey] = d.id;
        json[DescriptionKey] = d.description;
        json[MembersKey] = nlohmann::json::array();

        for (const DocumentationEntry& p : d.entries) {
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
                    [rv](const Documentation& doc) { return doc.id == rv->identifier; }
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

                // Set the members entry to the members of the table
                // to remove unnecessary nestling
                entry[MembersKey] = tableDocs[MembersKey];
            }
            else {
                entry[DescriptionKey] = p.verifier->documentation();
            }
            json[MembersKey].push_back(entry);
        }
        openspace::sortJson(json[MembersKey], NameKey);

        return json;
    }

    nlohmann::json propertyOwnerToJson(openspace::properties::PropertyOwner* owner) {
        ZoneScoped;

        using namespace openspace;
        nlohmann::json json;
        json[NameKey] = !owner->guiName().empty() ? owner->guiName() : owner->identifier();

        json[DescriptionKey] = owner->description();
        json[PropertiesKeys] = nlohmann::json::array();
        json[PropertyOwnersKey] = nlohmann::json::array();
        json[TypeKey] = owner->type();
        json[TagsKey] = owner->tags();

        for (properties::Property* p : owner->properties()) {
            nlohmann::json propertyJson;
            std::string name = !p->guiName().empty() ? p->guiName() : p->identifier();
            propertyJson[NameKey] = name;
            propertyJson[TypeKey] = p->className();
            propertyJson[UriKey] = p->fullyQualifiedIdentifier();
            propertyJson[IdentifierKey] = p->identifier();
            propertyJson[DescriptionKey] = p->description();

            json[PropertiesKeys].push_back(propertyJson);
        }
        sortJson(json[PropertiesKeys], NameKey);

        for (properties::PropertyOwner* o : owner->propertySubOwners()) {
            nlohmann::json propertyOwner;
            json[PropertyOwnersKey].push_back(propertyOwnerToJson(o));
        }
        sortJson(json[PropertyOwnersKey], NameKey);

        return json;
    }

    nlohmann::json luaFunctionToJson(const openspace::scripting::LuaLibrary::Function& f,
                                     bool includeSourceLocation)
    {
        using namespace openspace::scripting;

        nlohmann::json function;
        function[NameKey] = f.name;
        nlohmann::json arguments = nlohmann::json::array();

        for (const LuaLibrary::Function::Argument& arg : f.arguments) {
            nlohmann::json argument;
            argument[NameKey] = arg.name;
            argument[TypeKey] = arg.type;
            argument[DefaultValueKey] = arg.defaultValue.value_or(NoDataName);
            arguments.push_back(argument);
        }

        function[ArgumentsKey] = arguments;
        function[ReturnTypeKey] = f.returnType;
        function[HelpKey] = f.helpText;

        if (includeSourceLocation) {
            nlohmann::json sourceLocation;
            sourceLocation[FileKey] = f.sourceLocation.file;
            sourceLocation[LineKey] = f.sourceLocation.line;
            function[SourceLocationKey] = sourceLocation;
        }

        return function;
    }
} // namespace

namespace openspace::documentation {

DocumentationEngine* DocumentationEngine::_instance = nullptr;

DocumentationEngine::DuplicateDocumentationException::DuplicateDocumentationException(
                                                                        Documentation doc)
    : ghoul::RuntimeError(std::format(
        "Duplicate Documentation with name '{}' and id '{}'", doc.name, doc.id
    ))
    , documentation(std::move(doc))
{}

DocumentationEngine::DocumentationEngine() {}

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

nlohmann::json DocumentationEngine::generateScriptEngineJson() const {
    ZoneScoped;

    using namespace openspace::scripting;
    const std::vector<LuaLibrary> libraries = global::scriptEngine->allLuaLibraries();
    nlohmann::json json;

    for (const LuaLibrary& l : libraries) {
        nlohmann::json library;
        std::string libraryName = l.name;
        // Keep the library key for backwards compatability
        library[LibraryKey] = libraryName;
        library[NameKey] = libraryName;
        std::string os = OpenSpaceScriptingKey;
        library[FullNameKey] = libraryName.empty() ? os : os + "." + libraryName;

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

        sortJson(json, LibraryKey);
    }
    return json;
}

nlohmann::json DocumentationEngine::generateLicenseGroupsJson() const {
    nlohmann::json json;

    if (global::profile->meta.has_value()) {
        nlohmann::json metaJson;
        metaJson[NameKey] = ProfileName;
        metaJson[ProfileNameKey] = global::profile->meta->name.value_or(NoDataName);
        metaJson[VersionKey] = global::profile->meta->version.value_or(NoDataName);
        metaJson[DescriptionKey] = global::profile->meta->description.value_or(NoDataName);
        metaJson[AuthorKey] = global::profile->meta->author.value_or(NoDataName);
        metaJson[UrlKey] = global::profile->meta->url.value_or(NoDataName);
        metaJson[LicenseKey] = global::profile->meta->license.value_or(NoDataName);
        json.push_back(std::move(metaJson));
    }

    // Go through all assets and group them in a map with the key as the license name
    std::vector<const Asset*> assets =
        global::openSpaceEngine->assetManager().allAssets();

    std::map<std::string, nlohmann::json> assetLicenses;
    for (const Asset* asset : assets) {
        std::optional<Asset::MetaInformation> meta = asset->metaInformation();

        // Ensure the license is not going to be an empty string
        std::string licenseName = NoLicenseName;
        if (meta.has_value() && meta->license != NoDataName) {
            licenseName = meta->license;
        }

        nlohmann::json assetJson;
        assetJson[NameKey] = meta.has_value() ? meta->name : NoDataName;
        assetJson[VersionKey] = meta.has_value() ? meta->version : NoDataName;
        assetJson[DescriptionKey] = meta.has_value() ? meta->description : NoDataName;
        assetJson[AuthorKey] = meta.has_value() ? meta->author : NoDataName; 
        assetJson[UrlKey] = meta.has_value() ? meta->url : NoDataName; 
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
        profile[NameKey] = global::profile->meta->name.value_or(NoDataName);
        profile[VersionKey] = global::profile->meta->version.value_or(NoDataName);
        profile[DescriptionKey] = global::profile->meta->description.value_or(NoDataName);
        profile[AuthorKey] = global::profile->meta->author.value_or(NoDataName);
        profile[UrlKey] = global::profile->meta->url.value_or(NoDataName);
        profile[LicenseKey] = global::profile->meta->license.value_or(NoDataName);
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
    using Type = events::Event::Type;
    const std::unordered_map<Type, std::vector<EventEngine::ActionInfo>>& eventActions =
        global::eventEngine->eventActions();
    nlohmann::json events;

    nlohmann::json data = nlohmann::json::array();

    // Group actions by events
    for (const auto& [eventType, actions] : eventActions) {
        nlohmann::json eventJson;

        eventJson[NameKey] = std::string(events::toString(eventType));
        nlohmann::json actionsJson = nlohmann::json::array();

        for (const EventEngine::ActionInfo& action : actions) {
            nlohmann::json actionJson;
            actionJson[NameKey] = eventJson[NameKey];
            actionJson[ActionKey] = action.action;
            // Create a unique ID
            actionJson[IdKey] = std::format("{}{}", action.action, action.id);

            // Output filters as a string
            if (action.filter.has_value()) {
                ghoul::Dictionary filters = action.filter.value();
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

nlohmann::json DocumentationEngine::generateFactoryManagerJson() const {                 
    nlohmann::json json;

    std::vector<Documentation> docs = _documentations; // Copy the documentations
    const std::vector<FactoryManager::FactoryInfo>& factories =
        FactoryManager::ref().factories();

    for (const FactoryManager::FactoryInfo& factoryInfo : factories) {
        nlohmann::json factory;
        factory[NameKey] = factoryInfo.name;
        factory[IdentifierKey] = categoryName + factoryInfo.name;

        ghoul::TemplateFactoryBase* f = factoryInfo.factory.get();
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
        keybind[NameKey] = ghoul::to_string(p.first);
        keybind[ActionKey] = p.second;
        json.push_back(std::move(keybind));
    }
    sortJson(json, NameKey);

    nlohmann::json result;
    result[NameKey] = KeybindingsTitle;
    result[KeybindingsKey] = json;
    return result;
}

nlohmann::json DocumentationEngine::generatePropertyOwnerJson(
                                                   properties::PropertyOwner* owner) const
{
    ZoneScoped;

    ghoul_assert(owner, "Owner must not be nullptr");

    nlohmann::json json;
    std::vector<properties::PropertyOwner*> subOwners = owner->propertySubOwners();
    for (properties::PropertyOwner* o : subOwners) {
        if (o->identifier() != SceneTitle) {
            nlohmann::json jsonOwner = propertyOwnerToJson(o);

            json.push_back(jsonOwner);
        }
    }
    sortJson(json, NameKey);

    nlohmann::json result;
    result[NameKey] = propertyOwnerName;
    result[DataKey] = json;

    return result;
}

void DocumentationEngine::writeJavascriptDocumentation() const {
    ZoneScoped;

    // Write documentation to json files if config file supplies path for doc files
    if (global::configuration->documentation.path.empty()) {
        // if path was empty, that means that no documentation is requested
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

    // Make into a javascript variable so that it is possible to open with static html
    std::ofstream out = std::ofstream(absPath("${DOCUMENTATION}/documentationData.js"));
    out << "var data = " << result.dump();
    out.close();
}

void DocumentationEngine::writeJsonDocumentation() const {
    nlohmann::json factory = generateFactoryManagerJson();
    nlohmann::json scripting = generateScriptEngineJson();

    // Write two json files for the static docs page - asset components and scripting api
    std::ofstream out = std::ofstream(absPath("${DOCUMENTATION}/assetComponents.json"));
    if (out) {
        out << factory.dump();
    }
    out.close();

    out.open(absPath("${DOCUMENTATION}/scriptingApi.json"));
    if (out) {
        out << scripting.dump();
    }
    out.close();
}

nlohmann::json DocumentationEngine::generateActionJson() const {
    using namespace interaction;

    nlohmann::json res;
    res[NameKey] = ActionTitle;
    res[DataKey] = nlohmann::json::array();
    std::vector<Action> actions = global::actionManager->actions();

    for (const Action& action : actions) {
        nlohmann::json d;
        // Use identifier as name to make it more similar to scripting api
        d[NameKey] = action.identifier;
        d[GuiNameKey] = action.name;
        d[DocumentationKey] = action.documentation;
        d[CommandKey] = action.command;
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
            throw DuplicateDocumentationException(std::move(documentation));
        }
        else {
            _documentations.push_back(std::move(documentation));
        }
    }
}

std::vector<Documentation> DocumentationEngine::documentations() const {
    return _documentations;
}
} // namespace openspace::documentation
