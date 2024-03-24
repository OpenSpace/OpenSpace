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

#include <openspace/properties/property.h>

#include <openspace/properties/propertyowner.h>
#include <openspace/util/json_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <algorithm>

namespace {
    constexpr std::string_view MetaDataKeyGroup = "Group";
    constexpr std::string_view MetaDataKeyReadOnly = "isReadOnly";
    constexpr std::string_view MetaDataKeyNeedsConfirmation = "needsConfirmation";
    constexpr std::string_view MetaDataKeyViewOptions = "ViewOptions";
    constexpr std::string_view MetaDataKeyVisibility = "Visibility";

    constexpr std::string_view IdentifierKey = "Identifier";
    constexpr std::string_view NameKey = "Name";
    constexpr std::string_view TypeKey = "Type";
    constexpr std::string_view MetaDataKey = "MetaData";
    constexpr std::string_view AdditionalDataKey = "AdditionalData";
} // namespace

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
    notifyDeleteListeners();
}

const std::string& Property::identifier() const {
    return _identifier;
}

std::string Property::fullyQualifiedIdentifier() const {
    std::string identifier = _identifier;
    PropertyOwner* currentOwner = owner();
    while (currentOwner) {
        const std::string& ownerId = currentOwner->identifier();
        if (!ownerId.empty()) {
            identifier = std::format("{}.{}", ownerId, identifier);
        }
        currentOwner = currentOwner->owner();
    }
    return identifier;
}

std::any Property::get() const {
    return std::any();
}

bool Property::getLuaValue(lua_State*) const {
    return false;
}

void Property::set(std::any) {}

void Property::setLuaValue(lua_State*) {}

const std::type_info& Property::type() const {
    return typeid(void);
}

int Property::typeLua() const {
    return LUA_TNIL;
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
    _metaData.setValue(std::string(MetaDataKeyGroup), std::move(groupId));
}

std::string Property::groupIdentifier() const {
    if (_metaData.hasValue<std::string>(MetaDataKeyGroup)) {
        return _metaData.value<std::string>(MetaDataKeyGroup);
    }
    else {
        return "";
    }
}

void Property::setVisibility(Visibility visibility) {
    _metaData.setValue(
        std::string(MetaDataKeyVisibility),
        static_cast<std::underlying_type_t<Visibility>>(visibility)
    );
}

Property::Visibility Property::visibility() const {
    return static_cast<Visibility>(
        _metaData.value<std::underlying_type_t<Visibility>>(MetaDataKeyVisibility)
    );
}

void Property::setReadOnly(bool state) {
    _metaData.setValue(std::string(MetaDataKeyReadOnly), state);
}

void Property::setNeedsConfirmation(bool state) {
    _metaData.setValue(std::string(MetaDataKeyNeedsConfirmation), state);
}

void Property::setViewOption(std::string option, bool value) {
    ghoul::Dictionary d;
    d.setValue(std::move(option), value);
    _metaData.setValue(std::string(MetaDataKeyViewOptions), d);
}

bool Property::viewOption(const std::string& option, bool defaultValue) const {
    if (!_metaData.hasValue<ghoul::Dictionary>(MetaDataKeyViewOptions)) {
        return defaultValue;
    }
    const ghoul::Dictionary d =
        _metaData.value<ghoul::Dictionary>(MetaDataKeyViewOptions);
    if (d.hasKey(option)) {
        return d.value<bool>(option);
    }
    else {
        return defaultValue;
    }
}

const ghoul::Dictionary& Property::metaData() const {
    return _metaData;
}

std::string Property::jsonValue() const {
    return stringValue();
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

PropertyOwner* Property::owner() const {
    return _owner;
}

void Property::setPropertyOwner(PropertyOwner* owner) {
    _owner = owner;
}

void Property::notifyChangeListeners() {
    for (const std::pair<OnChangeHandle, std::function<void()>>& p : _onChangeCallbacks) {
        p.second();
    }
}

void Property::notifyDeleteListeners() {
    for (const std::pair<OnDeleteHandle, std::function<void()>>& p : _onDeleteCallbacks) {
        p.second();
    }
}

bool Property::hasChanged() const {
    return _isValueDirty;
}

void Property::resetToUnchanged() {
    _isValueDirty = false;
}

std::string Property::generateJsonDescription() const {
    const std::string cName = escapedJson(std::string(className()));
    const std::string identifier = fullyQualifiedIdentifier();
    const std::string identifierSan = escapedJson(identifier);
    const std::string gName = guiName();
    const std::string gNameSan = escapedJson(gName);
    const std::string metaData = generateMetaDataJsonDescription();
    const std::string description = generateAdditionalJsonDescription();

    return std::format(
        R"({{"{}":"{}","{}":"{}","{}":"{}","{}":{},"{}":{}}})",
        TypeKey, cName, IdentifierKey, identifierSan, NameKey, gNameSan, MetaDataKey,
        metaData, AdditionalDataKey, description
    );
}

std::string Property::generateMetaDataJsonDescription() const {
    static const std::map<Visibility, std::string> VisibilityConverter = {
        { Visibility::Always, "Always" },
        { Visibility::NoviceUser, "NoviceUser" },
        { Visibility::User, "User" },
        { Visibility::AdvancedUser, "AdvancedUser" },
        { Visibility::Developer, "Developer" },
        { Visibility::Hidden, "Hidden" }
    };
    const Visibility visibility = static_cast<Visibility>(
        _metaData.value<std::underlying_type_t<Visibility>>(MetaDataKeyVisibility)
    );
    const std::string& vis = VisibilityConverter.at(visibility);

    bool isReadOnly = false;
    if (_metaData.hasValue<bool>(MetaDataKeyReadOnly)) {
        isReadOnly = _metaData.value<bool>(MetaDataKeyReadOnly);
    }
    std::string isReadOnlyString = (isReadOnly ? "true" : "false");

    bool needsConfirmation = false;
    if (_metaData.hasValue<bool>(MetaDataKeyNeedsConfirmation)) {
        needsConfirmation = _metaData.value<bool>(MetaDataKeyNeedsConfirmation);
    }
    std::string needsConfirmationString = (needsConfirmation ? "true" : "false");

    const std::string groupId = groupIdentifier();
    const std::string sanitizedGroupId = escapedJson(groupId);

    std::string viewOptions = "{}";
    if (_metaData.hasValue<ghoul::Dictionary>(MetaDataKeyViewOptions)) {
        viewOptions = ghoul::formatJson(
            _metaData.value<ghoul::Dictionary>(MetaDataKeyViewOptions)
        );
    }

    std::string result = std::format(
        R"({{"{}":"{}","{}":"{}","{}":{},"{}":{},"{}":{}}})",
        MetaDataKeyGroup, sanitizedGroupId,
        MetaDataKeyVisibility, vis,
        MetaDataKeyReadOnly, isReadOnlyString,
        MetaDataKeyNeedsConfirmation, needsConfirmationString,
        MetaDataKeyViewOptions, viewOptions
    );
    return result;
}

std::string Property::generateAdditionalJsonDescription() const {
    return "{}";
}

void Property::setInterpolationTarget(std::any) {}
void Property::setLuaInterpolationTarget(lua_State*) {}
void Property::interpolateValue(float, ghoul::EasingFunc<float>) {}

} // namespace openspace::properties
