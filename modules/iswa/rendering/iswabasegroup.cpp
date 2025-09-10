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

#include <modules/iswa/rendering/iswabasegroup.h>

#include <openspace/json.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "IswaBaseGroup";
    using json = nlohmann::json;

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo AlphaInfo = {
        "Alpha",
        "Alpha",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DeleteInfo = {
        "Delete",
        "Delete",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };
} // namespace

namespace openspace {

IswaBaseGroup::IswaBaseGroup(std::string name, std::string type)
    : properties::PropertyOwner({ std::move(name) })
    , _enabled(EnabledInfo, true)
    , _alpha(AlphaInfo, 0.9f, 0.f, 1.f)
    , _delete(DeleteInfo)
    , _iswaType(std::move(type))
{
    addProperty(_enabled);
    addProperty(_alpha);
    addProperty(_delete);

    registerProperties();
}

IswaBaseGroup::~IswaBaseGroup() {}

bool IswaBaseGroup::isType(const std::string& type) const {
    return (_iswaType == type);
}

void IswaBaseGroup::updateGroup() {
    LDEBUG("Group " + identifier() + " published updateGroup");
    _groupEvent.publish("updateGroup", ghoul::Dictionary());
}

void IswaBaseGroup::clearGroup() {
    _groupEvent.publish("clearGroup", ghoul::Dictionary());
    LDEBUG("Group " + identifier() + " published clearGroup");
    unregisterProperties();
}

std::shared_ptr<DataProcessor> IswaBaseGroup::dataProcessor() {
    return _dataProcessor;
}

ghoul::Event<ghoul::Dictionary>& IswaBaseGroup::groupEvent() {
    return _groupEvent;
}

void IswaBaseGroup::registerProperties() {
    _enabled.onChange([this]() {
        LDEBUG("Group " + identifier() + " published enabledChanged");
        ghoul::Dictionary d;
        d.setValue("enabled", _enabled.value());
        _groupEvent.publish("enabledChanged", d);
    });

    _alpha.onChange([this]() {
        LDEBUG("Group " + identifier() + " published alphaChanged");
        ghoul::Dictionary d;
        d.setValue("alpha", static_cast<double>(_alpha));
        _groupEvent.publish("alphaChanged", d);
    });


    _delete.onChange([this]() { clearGroup(); });

    _registered = true;
}

void IswaBaseGroup::unregisterProperties() {
    _registered = false;
}

} //namespace openspace
