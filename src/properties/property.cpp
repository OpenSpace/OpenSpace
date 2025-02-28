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

#include <openspace/properties/propertyowner.h>
#include <openspace/util/json_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <algorithm>

namespace {
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
    for (const std::pair<OnDeleteHandle, std::function<void()>>& p : _onDeleteCallbacks) {
        p.second();
    }
}

const std::string& Property::identifier() const {
    return _identifier;
}

std::string Property::uri() const {
    const std::string& ownerUri = owner()->uri();
    return !ownerUri.empty() ? std::format("{}.{}", ownerUri, _identifier) : "";
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
}

std::string Property::groupIdentifier() const {
    return _metaData.group.value_or("");
}

void Property::setVisibility(Visibility visibility) {
    _metaData.visibility = visibility;
}

Property::Visibility Property::visibility() const {
    return _metaData.visibility;
}

void Property::setReadOnly(bool state) {
    _metaData.readOnly = state;
}

bool Property::isReadOnly() const {
    return _metaData.readOnly.value_or(false);
}

void Property::setNeedsConfirmation(bool state) {
    _metaData.needsConfirmation = state;
}

void Property::setViewOption(std::string option, bool value) {
    _metaData.viewOptions[std::move(option)] = value;
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

const PropertyOwner* Property::owner() const {
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

bool Property::hasChanged() const {
    return _isValueDirty;
}

void Property::resetToUnchanged() {
    _isValueDirty = false;
}

std::string Property::generateJsonDescription() const {
    const std::string cName = escapedJson(std::string(className()));
    const std::string identifier = uri();
    const std::string identifierSan = escapedJson(identifier);
    const std::string& gName = guiName();
    const std::string gNameSan = escapedJson(gName);
    const std::string metaData = generateMetaDataJsonDescription();
    const std::string description = generateAdditionalJsonDescription();

    return std::format(
        R"(
{{"Type":"{}","Identifier":"{}","Name":"{}","MetaData":{},"AdditionalData":{}}}
        )", cName, identifierSan, gNameSan, metaData, description
    );
}

std::string Property::generateMetaDataJsonDescription() const {
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
    std::string isReadOnlyString = (isReadOnly ? "true" : "false");

    const bool needsConfirmation = _metaData.needsConfirmation.value_or(false);
    std::string needsConfirmationString = (needsConfirmation ? "true" : "false");

    const std::string groupId = groupIdentifier();
    const std::string sanitizedGroupId = escapedJson(groupId);

    std::string viewOptions = "{}";
    {
        ghoul::Dictionary d;
        for (const std::pair<std::string, bool>& p : _metaData.viewOptions) {
            d.setValue(p.first, p.second);
        }
        viewOptions = ghoul::formatJson(d);
    }

    std::string result = std::format(
        R"({{"Group":"{}","Visibility":"{}","isReadOnly":{},"needsConfirmation":{},
             "ViewOptions":{}}})",
        sanitizedGroupId, vis, isReadOnlyString, needsConfirmationString, viewOptions
    );
    return result;
}

std::string Property::generateAdditionalJsonDescription() const {
    return "{}";
}

void Property::setLuaInterpolationTarget(lua_State*) {}
void Property::interpolateValue(float, ghoul::EasingFunc<float>) {}

} // namespace openspace::properties
