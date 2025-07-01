/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/properties/property.h>

#include <openspace/engine/globals.h>
#include <openspace/events/eventengine.h>
#include <openspace/properties/propertyowner.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <algorithm>

namespace openspace::properties {

const char* Property::ViewOptions::Color = "Color";
const char* Property::ViewOptions::MinMaxRange = "MinMaxRange";

#ifdef _DEBUG
uint64_t Property::Identifier = 0;
#endif

Property::Property(PropertyInfo info)
    : _identifier(info.identifier)
    , _guiName(info.guiName)
    , _description(info.description)
#ifdef _DEBUG
    , _id(Identifier++)
#endif
{
    ghoul_assert(!_identifier.empty(), "Identifier must not be empty");
    ghoul_assert(!_guiName.empty(), "guiName must not be empty");

    setVisibility(info.visibility);
}

Property::~Property() {
    for (const std::pair<OnDeleteHandle, std::function<void()>>& p : _onDeleteCallbacks) {
        p.second();
    }
}

const std::string& Property::identifier() const {
    return _identifier;
}

std::string_view Property::uri() const {
    ZoneScoped;
    return _uriCache;
}

const std::type_info& Property::type() const {
    return typeid(void);
}

ghoul::lua::LuaTypes Property::typeLua() const {
    return ghoul::lua::LuaTypes::None;
}

std::string Property::stringValue() const {
    return "";
}

const std::string& Property::guiName() const {
    return _guiName;
}

const std::string& Property::description() const {
    return _description;
}

void Property::setGroupIdentifier(std::string groupId) {
    _metaData.group = std::move(groupId);
    notifyMetaDataChangeListeners();
}

std::string Property::groupIdentifier() const {
    return _metaData.group.value_or("");
}

void Property::setVisibility(Visibility visibility) {
    _metaData.visibility = visibility;
    notifyMetaDataChangeListeners();

    // We only subscribe to meta data changes for visible properties, so if the
    // visibility changes during runtime, we need to notify the property owner
    // about the change for it to affect properties that are currently hidden
    if (_owner) {
        global::eventEngine->publishEvent<events::EventPropertyTreeUpdated>(
            _owner->uri()
        );
    }
}

Property::Visibility Property::visibility() const {
    return _metaData.visibility;
}

void Property::setReadOnly(bool state) {
    _metaData.readOnly = state;
    notifyMetaDataChangeListeners();
}

bool Property::isReadOnly() const {
    return _metaData.readOnly.value_or(false);
}

void Property::setNeedsConfirmation(bool state) {
    _metaData.needsConfirmation = state;
    notifyMetaDataChangeListeners();
}

void Property::setViewOption(std::string option, bool value) {
    _metaData.viewOptions[std::move(option)] = value;
    notifyMetaDataChangeListeners();
}

bool Property::viewOption(const std::string& option, bool defaultValue) const {
    auto it = _metaData.viewOptions.find(option);
    return it != _metaData.viewOptions.end() ? it->second : defaultValue;
}

std::string Property::jsonValue() const {
    std::string value = stringValue();
    if (value[0] == '{') {
        value.replace(0, 1, "[");
    }
    if (value[value.size() - 1] == '}') {
        value.replace(value.size() - 1, 1, "]");
    }
    return value;
}

Property::OnChangeHandle Property::onChange(std::function<void()> callback) {
    ghoul_assert(callback, "The callback must not be empty");

    const OnChangeHandle handle = _currentHandleValue++;
    _onChangeCallbacks.emplace_back(handle, std::move(callback));
    return handle;
}

Property::OnChangeHandle Property::onDelete(std::function<void()> callback) {
    ghoul_assert(callback, "The callback must not be empty");

    const OnDeleteHandle handle = _currentHandleValue++;
    _onDeleteCallbacks.emplace_back(handle, std::move(callback));
    return handle;
}

Property::OnMetaDataChangeHandle Property::onMetaDataChange(
                                                           std::function<void()> callback)
{
    ghoul_assert(callback, "The callback must not be empty");

    const OnMetaDataChangeHandle handle = _currentHandleValue++;
    _onMetaDataChangeCallbacks.emplace_back(handle, std::move(callback));
    return handle;
}

void Property::removeOnChange(OnChangeHandle handle) {
    if (handle == OnChangeHandleAll) {
        _onChangeCallbacks.clear();
    }
    else {
        auto it = std::find_if(
            _onChangeCallbacks.begin(),
            _onChangeCallbacks.end(),
            [handle](const std::pair<OnChangeHandle, std::function<void()>>& p) {
                return p.first == handle;
            }
        );

        ghoul_assert(
            it != _onChangeCallbacks.end(),
            "handle must be a valid callback handle"
        );

        _onChangeCallbacks.erase(it);
    }
}

void Property::removeOnDelete(OnDeleteHandle handle) {
    auto it = std::find_if(
        _onDeleteCallbacks.begin(),
        _onDeleteCallbacks.end(),
        [handle](const std::pair<OnDeleteHandle, std::function<void()>>& p) {
            return p.first == handle;
        }
    );

    ghoul_assert(
        it != _onDeleteCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _onDeleteCallbacks.erase(it);
}

void Property::removeOnMetaDataChange(OnMetaDataChangeHandle handle) {
    if (handle == OnMetaDataChangeHandleAll) {
        _onMetaDataChangeCallbacks.clear();
    }
    else {
        auto it = std::find_if(
            _onMetaDataChangeCallbacks.begin(),
            _onMetaDataChangeCallbacks.end(),
            [handle](const std::pair<OnChangeHandle, std::function<void()>>& p) {
                return p.first == handle;
            }
        );

        ghoul_assert(
            it != _onMetaDataChangeCallbacks.end(),
            "handle must be a valid callback handle"
        );

        _onMetaDataChangeCallbacks.erase(it);
    }
}

const PropertyOwner* Property::owner() const {
    return _owner;
}

void Property::setPropertyOwner(PropertyOwner* owner) {
    _owner = owner;
    updateUriCache();
}

void Property::notifyChangeListeners() {
    for (const std::pair<OnChangeHandle, std::function<void()>>& p : _onChangeCallbacks) {
        p.second();
    }
}

void Property::notifyMetaDataChangeListeners() {
    using Callback = const std::pair<OnMetaDataChangeHandle, std::function<void()>>;
    for (Callback& p : _onMetaDataChangeCallbacks) {
        p.second();
    }
}

bool Property::hasChanged() const {
    return _isValueDirty;
}

void Property::resetToUnchanged() {
    _isValueDirty = false;
}

void Property::updateUriCache() {
    const std::string& ownerUri = _owner ? _owner->uri() : "";
    _uriCache = !ownerUri.empty() ? std::format("{}.{}", ownerUri, _identifier) : "";
}

nlohmann::json Property::generateJsonDescription() const {
    static const std::unordered_map<Visibility, std::string> VisibilityConverter = {
       { Visibility::Always, "Always" },
       { Visibility::NoviceUser, "NoviceUser" },
       { Visibility::User, "User" },
       { Visibility::AdvancedUser, "AdvancedUser" },
       { Visibility::Developer, "Developer" },
       { Visibility::Hidden, "Hidden" }
    };
    const std::string& vis = VisibilityConverter.at(_metaData.visibility);
    const bool isReadOnly = _metaData.readOnly.value_or(false);
    const bool needsConfirmation = _metaData.needsConfirmation.value_or(false);
    const std::string groupId = groupIdentifier();

    nlohmann::json json = {
        { "identifier", _identifier },
        { "description", _description },
        { "guiName", _guiName },
        { "group", groupId },
        { "isReadOnly", isReadOnly },
        { "needsConfirmation", needsConfirmation },
        { "type", className() },
        { "visibility", vis }
    };

    if (_metaData.viewOptions.size() > 0) {
        nlohmann::json viewOptions = nlohmann::json::object();
        for (const std::pair<const std::string, bool>& p : _metaData.viewOptions) {
            viewOptions[p.first] = p.second;
        }
        json["viewOptions"] = viewOptions;
    }

    const nlohmann::json data = generateAdditionalJsonDescription();
    if (!data.empty()) {
        json["additionalData"] = data;
    }

    return json;
}

nlohmann::json Property::generateAdditionalJsonDescription() const {
    return nlohmann::json::object();
}

void Property::setLuaInterpolationTarget(lua_State*) {}
void Property::interpolateValue(float, ghoul::EasingFunc<float>) {}

} // namespace openspace::properties
