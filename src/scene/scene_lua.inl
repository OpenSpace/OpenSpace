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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/matrix/dmat2property.h>
#include <openspace/properties/matrix/dmat3property.h>
#include <openspace/properties/matrix/dmat4property.h>
#include <openspace/properties/matrix/mat2property.h>
#include <openspace/properties/matrix/mat3property.h>
#include <openspace/properties/matrix/mat4property.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/longproperty.h>
#include <openspace/properties/scalar/shortproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <openspace/properties/scalar/ulongproperty.h>
#include <openspace/properties/scalar/ushortproperty.h>
#include <openspace/properties/vector/dvec2property.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/dvec4property.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/ivec3property.h>
#include <openspace/properties/vector/ivec4property.h>
#include <openspace/properties/vector/uvec2property.h>
#include <openspace/properties/vector/uvec3property.h>
#include <openspace/properties/vector/uvec4property.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <algorithm>
#include <execution>
#include <cctype>

namespace {

/**
 * Returns the Property that matches the provided tag. First the provided owner is checked
 * and if that does not contain the requested tag, its own owners are checked recursively.
 */
bool ownerMatchesGroupTag(const openspace::properties::PropertyOwner* owner,
                          std::string_view tagToMatch, bool recursive = true)
{
    using namespace openspace;

    constexpr char Intersection = '&';
    constexpr char Negation = '~';
    constexpr char Union = '|';

    if (!owner) {
        return false;
    }

    const std::vector<std::string>& tags = owner->tags();

    if (size_t i = tagToMatch.find(Intersection);  i != std::string_view::npos) {
        // We have an intersection instruction
        if (tagToMatch.find(Negation) != std::string_view::npos) {
            throw ghoul::RuntimeError(std::format(
                "Only a single instruction to combine tags is supported. Found an "
                "intersection ('{}') and a negation instruction ('{}') in the query: "
                "'{}'", Intersection, Negation, tagToMatch
            ));
        }
        if (tagToMatch.find(Union) != std::string_view::npos) {
            throw ghoul::RuntimeError(std::format(
                "Only a single instruction to combine tags is supported. Found an "
                "intersection ('{}') and a union instruction ('{}') in the query: "
                "'{}'", Intersection, Union, tagToMatch
            ));
        }

        std::string_view t1 = tagToMatch.substr(0, i);
        auto t1It = std::find(tags.begin(), tags.end(), t1);
        std::string_view t2 = tagToMatch.substr(i + 1);
        auto t2It = std::find(tags.begin(), tags.end(), t2);
        if (t1It != tags.end() && t2It != tags.end()) {
            return true;
        }
    }
    if (size_t i = tagToMatch.find(Negation);  i != std::string_view::npos) {
        // We have an negation instruction
        if (tagToMatch.find(Intersection) != std::string_view::npos) {
            throw ghoul::RuntimeError(std::format(
                "Only a single instruction to combine tags is supported. Found a "
                "negation ('{}') and an intersection instruction ('{}') in the query: "
                "'{}'", Negation, Intersection, tagToMatch
            ));
        }
        if (tagToMatch.find(Union) != std::string_view::npos) {
            throw ghoul::RuntimeError(std::format(
                "Only a single instruction to combine tags is supported. Found a "
                "negation ('{}') and a union instruction ('{}') in the query: "
                "'{}'", Negation, Union, tagToMatch
            ));
        }

        std::string_view t1 = tagToMatch.substr(0, i);
        auto t1It = std::find(tags.begin(), tags.end(), t1);
        std::string_view t2 = tagToMatch.substr(i + 1);
        auto t2It = std::find(tags.begin(), tags.end(), t2);
        if (t1It != tags.end() && t2It == tags.end()) {
            return true;
        }
    }
    if (size_t i = tagToMatch.find(Union);  i != std::string_view::npos) {
        // We have an union instruction
        if (tagToMatch.find(Negation) != std::string_view::npos) {
            throw ghoul::RuntimeError(std::format(
                "Only a single instruction to combine tags is supported. Found a union "
                "('{}') and a negation instruction ('{}') in the query: "
                "'{}'", Union, Negation, tagToMatch
            ));
        }
        if (tagToMatch.find(Intersection) != std::string_view::npos) {
            throw ghoul::RuntimeError(std::format(
                "Only a single instruction to combine tags is supported. Found a union "
                "('{}') and an intersection instruction ('{}') in the query: "
                "'{}'", Union, Intersection, tagToMatch
            ));
        }

        std::string_view t1 = tagToMatch.substr(0, i);
        auto t1It = std::find(tags.begin(), tags.end(), t1);
        std::string_view t2 = tagToMatch.substr(i + 1);
        auto t2It = std::find(tags.begin(), tags.end(), t2);
        if (t1It != tags.end() || t2It != tags.end()) {
            return true;
        }
    }

    // We are dealing with a tag without any combinations
    auto match = std::find(tags.begin(), tags.end(), tagToMatch);
    if (match != tags.end()) {
        return true;
    }

    // If we got this far, we have an owner and we haven't found a match, so we have to
    // try one level higher if we are checking recursively
    if (recursive) {
        return ownerMatchesGroupTag(owner->owner(), tagToMatch);
    }
    return false;
}

// Checks to see if URI contains a group tag (with { } around the first term)
std::string groupTag(const std::string& command) {
    const std::string name = command.substr(0, command.find_first_of("."));
    if (name.front() == '{' && name.back() == '}') {
        return name.substr(1, name.length() - 2);
    }
    else {
        return "";
    }
}

std::string_view removeGroupTagFromUri(std::string_view uri) {
    size_t pos = uri.find_first_of(".");
    return pos == std::string::npos ? uri : uri.substr(pos);
}

/**
 * Parses the provided regex and splits it based on the location of the optional wildcard
 * character (*). If a wildcard existed in the regex, the returned tuple will be the
 * substring prior to the wildcard, the substring following to the wildcard, and `false`
 * as the first value. If there was no wildcard, the first return value is the empty
 * string, the second value is the full regular expression, and the third value is `true`,
 * indicating that it was a literal value.
 */
std::tuple<std::string_view, std::string_view, bool> parseRegex(std::string_view regex) {
    if (size_t wildPos = regex.find_first_of("*");  wildPos != std::string::npos) {
        std::string_view preName = regex.substr(0, wildPos);
        std::string_view postName = regex.substr(wildPos + 1);

        // If none then malformed regular expression
        if (preName.empty() && postName.empty()) [[unlikely]] {
            throw ghoul::lua::LuaError(std::format(
                "Malformed regular expression: '{}': Empty both before and after '*'",
                regex
            ));
        }

        // Currently do not support several wildcards
        if (regex.find_first_of("*", wildPos + 1) != std::string::npos) [[unlikely]] {
            throw ghoul::lua::LuaError(std::format(
                "Malformed regular expression: '{}': Currently only one '*' is supported",
                regex
            ));
        }

        return { preName, postName, false };
    }
    else {
        // Literal or tag
        return { "", regex, true };
    }
}

bool checkUriMatchFromRegexResults(std::string_view uri,
                        std::tuple<std::string_view, std::string_view, bool> regexResults,
                                   std::string_view groupTag,
                                   const openspace::properties::PropertyOwner* parentOwner
    )
{
    const bool isGroupMode = !groupTag.empty();
    auto [parentUri, identifier, isLiteral] = regexResults;

    // Literal check
    if (isLiteral && uri != identifier) {
        return false;
    }

    if (!identifier.empty()) {
        const size_t propertyPos = uri.find(identifier);
        if (
            // Check if the identifier appears in the URI at all
            (propertyPos == std::string::npos) ||
            // Check that the identifier fully matches the identifier in URI
            ((propertyPos + identifier.length() + 1) < uri.length()) ||
            // Match parent URI
            (!parentUri.empty() && uri.find(parentUri) == std::string::npos))
        {
            return false;
        }

        // At this point we know that the identifier matches, so another way this
        // property or property owner to fail is if we provided a tag and the owner
        // doesn't match it
        if (isGroupMode && !ownerMatchesGroupTag(parentOwner, groupTag)) {
            return false;
        }
    }
    else if (!parentUri.empty()) {
        const size_t parentPos = uri.find(parentUri);
        if (parentPos == std::string::npos) {
            return false;
        }

        // Check tag
        if (isGroupMode) {
            if (!ownerMatchesGroupTag(parentOwner, groupTag)) {
                return false;
            }
        }
        else if (parentPos != 0) {
            // Check that the node identifier fully matches the node in URI
            return false;
        }
    }

    return true;
}

std::vector<openspace::properties::Property*> findMatchesInAllProperties(
                                                                   std::string_view regex,
                                                                std::string_view groupTag)
{
    using namespace openspace;
    using namespace properties;

    auto [parentUri, propertyIdentifier, isLiteral] = parseRegex(regex);

    const bool isGroupMode = !groupTag.empty();
    if (parentUri.empty() && isGroupMode) {
        isLiteral = false;
    }

    const std::vector<Property*>& properties = allProperties();

    std::vector<Property*> matches;

    std::mutex mutex;
    std::for_each(
        std::execution::par_unseq,
        properties.cbegin(),
        properties.cend(),
        [&](Property* prop) {
            const std::string_view uri = prop->uri();

            bool isMatch = checkUriMatchFromRegexResults(
                uri,
                { parentUri, propertyIdentifier, isLiteral },
                groupTag,
                prop->owner()
            );

            if (isMatch) {
                std::lock_guard g(mutex);
                matches.push_back(prop);
            }
        }
    );

    return matches;
}

std::vector<openspace::properties::PropertyOwner*> findMatchesInAllPropertyOwners(
                                                                   std::string_view regex,
                                                                std::string_view groupTag)
{
    using namespace openspace;
    using namespace properties;

    auto [parentUri, ownerIdentifier, isLiteral] = parseRegex(regex);

    const bool isGroupMode = !groupTag.empty();
    if (isGroupMode) {
        isLiteral = false;
    }

    // If we are in group mode, no parent URI is found, and there is no punctuation
    // in the returned owner identifier, we only got the group - no identifier
    const bool inputIsOnlyTag = isGroupMode && parentUri.empty() &&
        ownerIdentifier.find(".") == std::string::npos;

    const std::vector<PropertyOwner*>& propertyOwners = allPropertyOwners();

    std::vector<PropertyOwner*> matches;

    std::mutex mutex;
    std::for_each(
        std::execution::par_unseq,
        propertyOwners.cbegin(),
        propertyOwners.cend(),
        [&](PropertyOwner* propOwner) {
            if (inputIsOnlyTag) {
                // If we only got a tag as input, the result is all owners that directly
                // match the group tag (without looking recusively in parent owners)
                if (!ownerMatchesGroupTag(propOwner, groupTag, false)) {
                    return;
                }
            }
            else {
                const std::string uri = propOwner->uri();

                bool isMatch = checkUriMatchFromRegexResults(
                    uri,
                    { parentUri, ownerIdentifier, isLiteral },
                    groupTag,
                    propOwner->owner()
                );

                if (!isMatch) {
                    return;
                }
            }

            std::lock_guard g(mutex);
            matches.push_back(propOwner);
        }
    );

    return matches;
}

void applyRegularExpression(lua_State* L, std::string_view regex,
                            double interpolationDuration, std::string_view groupTag,
                            ghoul::EasingFunction easingFunction, std::string postScript)
{
    using namespace openspace;
    using namespace properties;

    //
    // 1. Retrieve all properties that match the regex
    std::vector<Property*> matchingProps = findMatchesInAllProperties(regex, groupTag);

    //
    // 2. Remove all properties that don't match the provided type
    std::erase_if(
        matchingProps,
        [L, type = ghoul::lua::fromLuaType(lua_type(L, -1))](Property* prop) {
            const bool typeMatches = typeMatch(type, prop->typeLua());
            if (!typeMatches) [[unlikely]] {
                LERRORC(
                    "property_setValue",
                    std::format(
                        "{}: Property '{}' does not accept input of type '{}'. Requested "
                        "type: {}",
                        ghoul::lua::errorLocation(L), prop->uri(),
                        luaTypeToString(type), luaTypeToString(prop->typeLua())
                    )
                );
            }

            return !typeMatches;
        }
    );

    if (matchingProps.empty()) [[unlikely]] {
        LERRORC(
            "property_setValue",
            std::format(
                "{}: No property matched the requested URI '{}'",
                ghoul::lua::errorLocation(L), regex
            )
        );
        return;
    }

    for (Property* prop : matchingProps) {
        // If the fully qualified id matches the regular expression, we queue the
        // value change if the types agree

        // The setLuaInterpolationTarget and setLuaValue functions will remove the
        // value from the stack, so we need to push it to the end
        lua_pushvalue(L, -1);

        if (global::sessionRecordingHandler->isRecording()) {
            global::sessionRecordingHandler->savePropertyBaseline(*prop);
        }

        if (interpolationDuration == 0.0) {
            if (Scene* scene = global::renderEngine->scene();  scene) {
                scene->removePropertyInterpolation(prop);
            }
            prop->setLuaValue(L);
        }
        else {
            prop->setLuaInterpolationTarget(L);
            if (Scene* scene = global::renderEngine->scene();  scene) {
                scene->addPropertyInterpolation(
                    prop,
                    static_cast<float>(interpolationDuration),
                    postScript,
                    easingFunction
                );
            }
        }
    }

    // We need to do this check outside the for loop since the results of the postscript
    // script might otherwise be overwritten by the following property values
    if (interpolationDuration == 0.0 && !postScript.empty()) {
        global::scriptEngine->runScript({ std::move(postScript) });
    }
}

int setPropertyCallSingle(openspace::properties::Property& prop, const std::string& uri,
                          lua_State* L, double duration,
                          ghoul::EasingFunction easingFunction, std::string postScript)
{
    using namespace openspace;
    using ghoul::lua::errorLocation;
    using ghoul::lua::luaTypeToString;

    const ghoul::lua::LuaTypes type = ghoul::lua::fromLuaType(lua_type(L, -1));
    if (!typeMatch(type, prop.typeLua())) {
        throw ghoul::lua::LuaError(std::format(
            "{}: Property '{}' does not accept input of type '{}'. Requested type: {}",
            errorLocation(L), uri, luaTypeToString(type), luaTypeToString(prop.typeLua())
        ));
    }

    if (global::sessionRecordingHandler->isRecording()) {
        global::sessionRecordingHandler->savePropertyBaseline(prop);
    }
    if (duration == 0.0) {
        if (Scene* scene = global::renderEngine->scene();  scene) {
            scene->removePropertyInterpolation(&prop);
        }
        prop.setLuaValue(L);

        if (!postScript.empty()) {
            global::scriptEngine->runScript({ std::move(postScript) });
        }
    }
    else {
        prop.setLuaInterpolationTarget(L);
        if (Scene* scene = global::renderEngine->scene();  scene) {
            scene->addPropertyInterpolation(
                &prop,
                static_cast<float>(duration),
                std::move(postScript),
                easingFunction
            );
        }
    }
    return 0;
}

template <typename T>
void createCustomProperty(openspace::properties::Property::PropertyInfo info,
                          std::optional<std::string> onChange)
{
    T* p = new T(info);
    if (onChange.has_value() && !onChange->empty()) {
        p->onChange(
            [p, script = *onChange]() {
                using namespace ghoul::lua;
                LuaState s;
                openspace::global::scriptEngine->initializeLuaState(s);
                ghoul::lua::push(s, p->value());
                lua_setglobal(s, "value");
                ghoul::lua::runScript(s, script);
            }
        );
    }
    openspace::global::userPropertyOwner->addProperty(p);
}

template <>
void createCustomProperty<openspace::properties::TriggerProperty>(
                                       openspace::properties::Property::PropertyInfo info,
                                                      std::optional<std::string> onChange)
{
    using namespace openspace::properties;
    TriggerProperty* p = new TriggerProperty(info);
    if (onChange.has_value() && !onChange->empty()) {
        p->onChange(
            [script = *onChange]() {
                using namespace ghoul::lua;
                LuaState s;
                openspace::global::scriptEngine->initializeLuaState(s);
                ghoul::lua::runScript(s, script);
            }
        );
    }
    openspace::global::userPropertyOwner->addProperty(p);
}

} // namespace

namespace openspace::luascriptfunctions {

template <bool optimization>
int propertySetValue(lua_State* L) {
    ZoneScoped;

    int nParameters = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 2, 6 },
        "lua::property_setValue"
    );
    defer { lua_settop(L, 0); };

    std::string uriOrRegex =
        ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::No);
    double interpolationDuration = 0.0;
    std::string easingMethodName;
    ghoul::EasingFunction easingMethod = ghoul::EasingFunction::Linear;
    std::string postScript;

    // Extracting the parameters. These are the options that we have:
    // 1. <uri> <value>
    // 2. <uri> <value> <duration>
    // 3. <uri> <value> <duration> <easing>
    // 4. <uri> <value> <duration> <easing> <postscript>

    if (nParameters >= 3) {
        // Later functions expect the value to be at the last position on the stack
        lua_pushvalue(L, 2);

        if (ghoul::lua::hasValue<double>(L, 3)) {
            interpolationDuration =
                ghoul::lua::value<double>(L, 3, ghoul::lua::PopValue::No);
        }
        else {
            std::string msg = std::format(
                "Unexpected type '{}' in argument 3",
                ghoul::lua::luaTypeToString(lua_type(L, 3))
            );
            return ghoul::lua::luaError(L, msg);
        }
    }
    if (nParameters >= 4) {
        if (ghoul::lua::hasValue<std::string>(L, 4)) {
            easingMethodName =
                ghoul::lua::value<std::string>(L, 4, ghoul::lua::PopValue::No);
        }
        else {
            std::string msg = std::format(
                "Unexpected type '{}' in argument 4",
                ghoul::lua::luaTypeToString(lua_type(L, 4))
            );
            return ghoul::lua::luaError(L, msg);
        }
    }
    if (nParameters == 5) {
        if (ghoul::lua::hasValue<std::string>(L, 5)) {
            postScript = ghoul::lua::value<std::string>(L, 5, ghoul::lua::PopValue::No);
        }
        else {
            std::string msg = std::format(
                "Unexpected type '{}' in argument 5",
                ghoul::lua::luaTypeToString(lua_type(L, 5))
            );
            return ghoul::lua::luaError(L, msg);
        }
    }

    if (!easingMethodName.empty()) {
        bool correctName = ghoul::isValidEasingFunctionName(easingMethodName);
        if (!correctName) {
            throw ghoul::lua::LuaError(std::format(
                "'{}' is not a valid easing method", easingMethodName
            ));
        }

        easingMethod = ghoul::easingFunctionFromName(easingMethodName);
    }

    if constexpr (optimization) {
        properties::Property* prop = property(uriOrRegex);
        if (!prop) {
            LERRORC(
                "property_setValue",
                std::format(
                    "{}: Property with URI '{}' was not found",
                    ghoul::lua::errorLocation(L), uriOrRegex
                )
            );
            return 0;
        }

        return setPropertyCallSingle(
            *prop,
            uriOrRegex,
            L,
            interpolationDuration,
            easingMethod,
            std::move(postScript)
        );
    }
    else {
        std::string tag = groupTag(uriOrRegex);
        if (!tag.empty()) {
            // Remove group tag from start of regex and replace with '*'
            uriOrRegex = removeGroupTagFromUri(uriOrRegex);
        }

        applyRegularExpression(
            L,
            uriOrRegex,
            interpolationDuration,
            tag,
            easingMethod,
            std::move(postScript)
        );
    }
    return 0;
}

int propertyGetValue(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::propertyGetValue");
    const std::string uri = ghoul::lua::value<std::string>(L);

    properties::Property* prop = property(uri);
    if (!prop) {
        LERRORC(
            "propertyGetValue",
            std::format(
                "{}: Property with URI '{}' was not found",
                ghoul::lua::errorLocation(L), uri
            )
        );
        return 0;
    }

    prop->getLuaValue(L);
    return 1;
}

}  // namespace openspace::luascriptfunctions

namespace {

/**
 * Returns whether a property with the given URI exists. The `uri` identifies the property
 * or properties that are checked by this function and can include both wildcards `*`
 * which match anything, as well as tags (`{tag}`) which match scene graph nodes that have
 * this tag. There is also the ability to combine two tags through the `&`, `|`, and `~`
 * operators. `{tag1&tag2}` will match anything that has the tag1 and the tag2.
 * `{tag1|tag2}` will match anything that has the tag1 or the tag 2, and `{tag1~tag2}`
 * will match anything that has tag1 but not tag2. If no wildcards or tags are provided at
 * most one property value will be changed. With wildcards or tags all properties that
 * match the URI are changed instead.
 *
 * \param uri The URI that identifies the property or properties whose values should be
 *            changed. The URI can contain 0 or 1 wildcard `*` characters or a tag
 *            expression (`{tag}`) that identifies a property owner.
 */
[[codegen::luawrap]] bool hasProperty(std::string uri) {
    openspace::properties::Property* prop = openspace::property(uri);
    return prop != nullptr;
}

/**
 * Returns a list of property identifiers that match the passed regular expression. The
 * `uri` identifies the property or properties that are returned by this function and can
 * include both wildcards `*` which match anything, as well as tags (`{tag}`) which match
 * scene graph nodes that have this tag. There is also the ability to combine two tags
 * through the `&`, `|`, and `~` operators. `{tag1&tag2}` will match anything that has
 * both tags `tag1` and `tag2`. `{tag1|tag2}` will match anything that has `tag1` or
 * `tag2`, and `{tag1~tag2}` will match anything that has `tag1` but not `tag2`. If no
 * wildcards or tags are provided at most one property identifier will be returned. With
 * wildcards or tags, the identifiers of all properties that match the URI are returned
 * instead.
 *
 * \param uri The URI that identifies the property or properties to get. The URI can
 *            contain 0 or 1 wildcard `*` characters or a tag expression (`{tag}`) that
 *            identifies a property owner.
 * \ return A list of property URIs
 */
[[codegen::luawrap]] std::vector<std::string> property(std::string uri) {
    using namespace openspace;

    std::string tag = groupTag(uri);
    if (!tag.empty()) {
        // Remove group name from start of regex and replace with '*'
        uri = removeGroupTagFromUri(uri);
    }

    std::vector<properties::Property*> props = findMatchesInAllProperties(uri, tag);

    std::vector<std::string> matches;
    matches.reserve(props.size());
    for (properties::Property* prop : props) {
        matches.emplace_back(prop->uri());
    }
    return matches;
}

/**
 * Returns a list of property owner identifiers that match the passed regular expression.
 * The `uri` identifies the property owner or owner that are returned by this function and
 * can include both wildcards `*` which match anything, as well as tags (`{tag}`) which
 * match scene graph nodes that have this tag. There is also the ability to combine two
 * tags through the `&`, `|`, and `~` operators. `{tag1&tag2}` will match anything that
 * has both tags `tag1` and `tag2`. `{tag1|tag2}` will match anything that has the tag
 * `tag1` or `tag2`, * and `{tag1~tag2}` will match anything that has `tag1` but not
 * `tag2`. If no wildcards or tags are provided at most one property owner identifier
 * will be returned. With wildcards or tags, the identifiers of all property owners that
 * match the URI are returned instead.
 *
 * \param uri The URI that identifies the property owner or owners to get. The URI can
 *            contain 0 or 1 wildcard `*` characters or a tag expression (`{tag}`) that
 *            identifies a property owner.
 * \ return A list of property owner URIs
 */
[[codegen::luawrap]] std::vector<std::string> propertyOwner(std::string uri) {
    using namespace openspace;

    std::string tag = groupTag(uri);
    if (!tag.empty()) {
        // Remove group name from start of regex and replace with '*'
        uri = removeGroupTagFromUri(uri);
    }

    std::vector<properties::PropertyOwner*> owners =
        findMatchesInAllPropertyOwners(uri, tag);

    std::vector<std::string> matches;
    matches.reserve(owners.size());
    for (properties::PropertyOwner* owner : owners) {
        matches.emplace_back(owner->uri());
    }
    return matches;
}

/**
 * Loads the SceneGraphNode described in the table and adds it to the SceneGraph.
 */
[[codegen::luawrap]] void addSceneGraphNode(ghoul::Dictionary node) {
    ZoneScoped;

    using namespace openspace;
    try {
        SceneGraphNode* n = global::renderEngine->scene()->loadNode(node);
        if (!n) {
            LERRORC("Scene", "Could not load scene graph node");
            throw ghoul::lua::LuaError("Error loading scene graph node");
        }

        global::renderEngine->scene()->initializeNode(n);
    }
    catch (const documentation::SpecificationError& e) {
        std::string cat =
            node.hasValue<std::string>("Identifier") ?
            node.value<std::string>("Identifier") :
            "Scene";
        logError(e, cat);

        throw ghoul::lua::LuaError(std::format(
            "Error loading scene graph node: {}", e.what()
        ));
    }
    catch (const ghoul::RuntimeError& e) {
        throw ghoul::lua::LuaError(std::format(
            "Error loading scene graph node: {}", e.what()
        ));
    }
}

/**
 * Removes the SceneGraphNode identified by name or by extracting the 'Identifier' key if
 * the parameter is a table.
 */
[[codegen::luawrap]] void removeSceneGraphNode(
                                        std::variant<std::string, ghoul::Dictionary> node)
{
    using namespace openspace;
    std::string identifier;
    if (std::holds_alternative<std::string>(node)) {
        identifier = std::get<std::string>(node);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(node);
        if (!d.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError(
                "Table passed to removeSceneGraphNode does not contain an Identifier"
            );
        }
        identifier = d.value<std::string>("Identifier");
    }

    if (identifier == "Root") {
        throw ghoul::lua::LuaError("Cannot remove the 'Root' scene graph node");
    }

    SceneGraphNode* foundNode = sceneGraphNode(identifier);
    if (!foundNode) {
        throw ghoul::lua::LuaError(
            std::format("Did not find a match for identifier: {}", identifier)
        );
    }

    SceneGraphNode* parent = foundNode->parent();
    if (!parent) {
        throw ghoul::lua::LuaError("Cannot remove root node");
    }

    // Remove the node and all its children
    std::function<void(SceneGraphNode*)> removeNode =
        [&removeNode](SceneGraphNode* localNode) {
        if (localNode == global::navigationHandler->anchorNode()) {
            global::navigationHandler->orbitalNavigator().setFocusNode(
                sceneGraph()->root()
            );
            global::navigationHandler->orbitalNavigator().startRetargetAnchor();
        }

        if (localNode == global::navigationHandler->orbitalNavigator().aimNode()) {
            global::navigationHandler->orbitalNavigator().setAimNode("");
        }

        std::vector<SceneGraphNode*> children = localNode->children();

        ghoul::mm_unique_ptr<SceneGraphNode> n = localNode->parent()->detachChild(
            *localNode
        );
        ghoul_assert(n.get() == localNode, "Wrong node returned from detaching");

        for (SceneGraphNode* c : children) {
            removeNode(c);
        }

        localNode->deinitializeGL();
        localNode->deinitialize();
        n = nullptr;
    };

    removeNode(foundNode);
}

/**
 * Removes all SceneGraphNodes with identifiers matching the input regular expression.
 */
[[codegen::luawrap]] void removeSceneGraphNodesFromRegex(std::string regex) {
    using namespace openspace;
    const std::vector<SceneGraphNode*>& nodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    auto [nodeIdentifier, propertyIdentifier, isLiteral] = parseRegex(regex);

    std::vector<SceneGraphNode*> markedList;
    for (SceneGraphNode* node : nodes) {
        const std::string& identifier = node->identifier();

        if (isLiteral && identifier != propertyIdentifier) {
            continue;
        }

        if (!propertyIdentifier.empty()) {
            const size_t propertyPos = identifier.find(propertyIdentifier);
            if (
                // Check if the propertyIdentifier appears in the URI at all
                (propertyPos == std::string::npos) ||
                // Check that the propertyIdentifier fully matches the property in uri
                ((propertyPos + propertyIdentifier.length() + 1) < identifier.length()) ||
                // Match node name
                (!nodeIdentifier.empty() &&
                    identifier.find(nodeIdentifier) == std::string::npos))
            {
                continue;
            }
        }
        else if (!nodeIdentifier.empty()) {
            size_t nodePos = identifier.find(nodeIdentifier);
            if (nodePos == std::string::npos) {
                continue;
            }

            // Check that the nodeName fully matches the node in id
            if (nodePos != 0) {
                continue;
            }
        }

        SceneGraphNode* parent = node->parent();
        if (!parent) {
            throw ghoul::lua::LuaError("Cannot remove root node");
        }

        markedList.push_back(node);
    }

    if (markedList.empty()) {
        throw ghoul::lua::LuaError(std::format(
            "Did not find a match for identifier: {}", nodeIdentifier
        ));
    }

    // Add all the children
    std::function<void(SceneGraphNode*, std::vector<SceneGraphNode*>&)> markNode =
        [&markNode](SceneGraphNode* node, std::vector<SceneGraphNode*>& marked)
    {
        for (SceneGraphNode* child : node->children()) {
            markNode(child, marked);
        }

        const auto it = std::find(marked.cbegin(), marked.cend(), node);
        if (it == marked.end()) {
            marked.push_back(node);
        }
    };
    for (SceneGraphNode* node : markedList) {
        markNode(node, markedList);
    }

    // Remove all marked nodes
    std::function<void(SceneGraphNode*)> removeNode =
        [&removeNode, &markedList](SceneGraphNode* localNode) {

        if (localNode == global::navigationHandler->anchorNode()) {
            global::navigationHandler->setFocusNode(sceneGraph()->root());
        }

        if (localNode == global::navigationHandler->orbitalNavigator().aimNode()) {
            global::navigationHandler->orbitalNavigator().setAimNode("");
        }

        std::vector<SceneGraphNode*> children = localNode->children();

        ghoul::mm_unique_ptr<SceneGraphNode> n = localNode->parent()->detachChild(
            *localNode
        );
        ghoul_assert(n.get() == localNode, "Wrong node returned from detaching");

        for (SceneGraphNode* c : children) {
            removeNode(c);
        }

        markedList.erase(
            std::remove(markedList.begin(), markedList.end(), localNode),
            markedList.end()
        );

        localNode->deinitializeGL();
        localNode->deinitialize();
        n = nullptr;
    };

    while (!markedList.empty()) {
        removeNode(markedList[0]);
    }
}

// Checks whether the specifies SceneGraphNode is present in the current scene.
[[codegen::luawrap]] bool hasSceneGraphNode(std::string nodeName) {
    using namespace openspace;
    SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(nodeName);
    return node != nullptr;
}

// Returns a list of all scene graph nodes in the scene
[[codegen::luawrap]] std::vector<std::string> sceneGraphNodes() {
    using namespace openspace;

    const std::vector<SceneGraphNode*>& nodes =
        global::renderEngine->scene()->allSceneGraphNodes();
    std::vector<std::string> res;
    res.reserve(nodes.size());
    for (SceneGraphNode* node : nodes) {
        res.push_back(node->identifier());
    }
    return res;
}

// Returns a list of all scene graph nodes in the scene that have a renderable of the
// specific type
[[codegen::luawrap]] std::vector<std::string> nodeByRenderableType(std::string type) {
    using namespace openspace;

    const std::vector<SceneGraphNode*>& nodes =
        global::renderEngine->scene()->allSceneGraphNodes();
    std::vector<std::string> res;
    for (SceneGraphNode* node : nodes) {
        Renderable* renderable = node->renderable();
        if (renderable && renderable->typeAsString() == type) {
            res.push_back(node->identifier());
        }
    }
    return res;
}

// Returns a list of all screen-space renderables
[[codegen::luawrap]] std::vector<std::string> screenSpaceRenderables() {
    using namespace openspace;

    const std::vector<ScreenSpaceRenderable*>& ssrs =
        global::renderEngine->screenSpaceRenderables();
    std::vector<std::string> res;
    res.reserve(ssrs.size());
    for (ScreenSpaceRenderable* ssr : ssrs) {
        res.push_back(ssr->identifier());
    }
    return res;
}

/**
 * Returns the world position of the scene graph node with the given string as identifier.
 */
[[codegen::luawrap]] glm::dvec3 worldPosition(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(std::format(
            "Did not find a match for identifier: {} ", identifier
        ));
    }

    glm::dvec3 pos = node->worldPosition();
    return pos;
}

/**
 * Returns the world rotation matrix of the scene graph node with the given string as
 * identifier.
 */
[[codegen::luawrap]] glm::dmat3 worldRotation(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(std::format(
            "Did not find a match for identifier: {} ", identifier
        ));
    }

    glm::dmat3 rot = node->worldRotationMatrix();
    return rot;
}

/**
 * The scene graph node identified by the first string is reparented to be a child of the
 * scene graph node identified by the second string.
 */
[[codegen::luawrap]] void setParent(std::string identifier, std::string newParent) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(std::format(
            "Did not find a match for identifier: {}", identifier
        ));
    }
    SceneGraphNode* newParentNode = sceneGraphNode(newParent);
    if (!newParentNode) {
        throw ghoul::lua::LuaError(std::format(
            "Did not find a match for new parent identifier: {}", newParent
        ));
    }

    node->setParent(*newParentNode);
    global::renderEngine->scene()->markNodeRegistryDirty();
}

/**
 * Returns the bounding sphere of the scene graph node with the given string as
 * identifier.
 */
[[codegen::luawrap]] double boundingSphere(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(std::format(
            "Did not find a match for identifier: {}", identifier
        ));
    }

    double bs = node->boundingSphere();
    return bs;
}

/**
 * Returns the interaction sphere of the scene graph node with the given string as
 * identifier.
 */
[[codegen::luawrap]] double interactionSphere(std::string identifier) {
    using namespace openspace;
    SceneGraphNode* node = sceneGraphNode(identifier);
    if (!node) {
        throw ghoul::lua::LuaError(std::format(
            "Did not find a match for identifier: {}", identifier
        ));
    }

    double is = node->interactionSphere();
    return is;
}

enum class [[codegen::enum]] CustomPropertyType {
    BoolProperty,
    DoubleProperty,
    DMat2Property,
    DMat3Property,
    DMat4Property,
    DVec2Property,
    DVec3Property,
    DVec4Property,
    FloatProperty,
    IntProperty,
    IVec2Property,
    IVec3Property,
    IVec4Property,
    LongProperty,
    Mat2Property,
    Mat3Property,
    Mat4Property,
    ShortProperty,
    StringProperty,
    StringListProperty,
    TriggerProperty,
    UShortProperty,
    UIntProperty,
    ULongProperty,
    UVec2Property,
    UVec3Property,
    UVec4Property,
    Vec2Property,
    Vec3Property,
    Vec4Property
};

/**
 * Creates a new property that lives in the `UserProperty` group.
 *
 * \param identifier The identifier that is going to be used for the new property
 * \param type The type of the property, has to be one of "DMat2Property",
 *        "DMat3Property", "DMat4Property", "Mat2Property", "Mat3Property",
 *        "Mat4Property", "BoolProperty", "DoubleProperty", "FloatProperty",
 *        "IntProperty", "StringProperty", "StringListProperty", "LongProperty",
 *        "ShortProperty", "UIntProperty", "ULongProperty", "DVec2Property",
 *        "DVec3Property", "DVec4Property", "IVec2Property", "IVec3Property",
 *        "IVec4Property", "UVec2Property", "UVec3Property", "UVec4Property",
 *        "Vec2Property", "Vec3Property", "Vec4Property"
 * \param guiName The name that the property uses in the user interface. If this value is
 *        not provided, the `identifier` is used instead
 * \param description A description what the property is used for
 * \param onChange A Lua script that will be executed whenever the property changes
 */
[[codegen::luawrap]] void addCustomProperty(std::string identifier,
                                            CustomPropertyType type,
                                            std::optional<std::string> guiName,
                                            std::optional<std::string> description,
                                            std::optional<std::string> onChange)
{
    using namespace openspace;
    using namespace openspace::properties;

    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Identifier must not empty");
    }

    if (global::userPropertyOwner->hasProperty(identifier)) {
        throw ghoul::lua::LuaError(std::format(
            "Failed to register property '{}' since a user-defined property with that "
            "name already exists",
            identifier
        ));
    }

    // @TODO (abock, 2022-05-01)  These if statements here are a bit gnarly since it
    // requires us to update them as soon as we add a new property type. It would be nicer
    // to have a factory function for this but right now this is the only place where that
    // factory would be used.

    const char* gui =
        guiName.has_value() && !guiName->empty() ?
        guiName->c_str() :
        identifier.c_str();

    Property::PropertyInfo info = {
        identifier.c_str(),
        gui,
        description.has_value() ? description->c_str() : ""
    };
    switch (type) {
        case CustomPropertyType::BoolProperty:
            createCustomProperty<BoolProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::DMat2Property:
            createCustomProperty<DMat2Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::DMat3Property:
            createCustomProperty<DMat3Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::DMat4Property:
            createCustomProperty<DMat4Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::DoubleProperty:
            createCustomProperty<DoubleProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::DVec2Property:
            createCustomProperty<DVec2Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::DVec3Property:
            createCustomProperty<DVec3Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::DVec4Property:
            createCustomProperty<DVec4Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::FloatProperty:
            createCustomProperty<FloatProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::IntProperty:
            createCustomProperty<IntProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::IVec2Property:
            createCustomProperty<IVec2Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::IVec3Property:
            createCustomProperty<IVec3Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::IVec4Property:
            createCustomProperty<IVec4Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::LongProperty:
            createCustomProperty<LongProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::Mat2Property:
            createCustomProperty<Mat2Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::Mat3Property:
            createCustomProperty<Mat3Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::Mat4Property:
            createCustomProperty<Mat4Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::ShortProperty:
            createCustomProperty<ShortProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::StringProperty:
            createCustomProperty<StringProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::StringListProperty:
            createCustomProperty<StringListProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::TriggerProperty:
            createCustomProperty<TriggerProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::UIntProperty:
            createCustomProperty<UIntProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::ULongProperty:
            createCustomProperty<ULongProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::UShortProperty:
            createCustomProperty<UShortProperty>(info, std::move(onChange));
            return;
        case CustomPropertyType::UVec2Property:
            createCustomProperty<UVec2Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::UVec3Property:
            createCustomProperty<UVec3Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::UVec4Property:
            createCustomProperty<UVec4Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::Vec2Property:
            createCustomProperty<Vec2Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::Vec3Property:
            createCustomProperty<Vec3Property>(info, std::move(onChange));
            return;
        case CustomPropertyType::Vec4Property:
            createCustomProperty<Vec4Property>(info, std::move(onChange));
            return;
    }
    throw std::runtime_error("Missing case label");
}

[[codegen::luawrap]] void removeCustomProperty(std::string identifier) {
    using namespace openspace;
    properties::Property* p = global::userPropertyOwner->property(identifier);
    if (!p) {
        throw ghoul::lua::LuaError(std::format(
            "Could not find user-defined property '{}'", identifier
        ));
    }

    global::userPropertyOwner->removeProperty(p);
    delete p;
}

/**
 * Create a valid identifier from the provided input string. Will replace invalid
 * characters like whitespaces and some punctuation marks with valid alternatives
 */
[[codegen::luawrap]] std::string makeIdentifier(std::string input) {
    return openspace::makeIdentifier(input);
}

/**
 * Set a custom ordering of the items in a specific branch in the Scene GUI tree, i.e.
 * for a specific GUI path.
 *
 * \param guiPath The GUI path for which the order should be set.
 * \param list A list of names of scene graph nodes or subgroups in the GUI, in the order
 *             of which they should appear in the tree. The list does not have to include
 *             all items in the given GUI path. Any excluded items will be placed after
 *             the ones in the list.
 */
[[codegen::luawrap]] void setGuiOrder(std::string guiPath, std::vector<std::string> list)
{
    return openspace::global::renderEngine->scene()->setGuiTreeOrder(guiPath, list);
}

/**
 * Get a dictionary containing the current map with custom orderings for the Scene GUI
 * tree. Each key in the dictionary corresponds to a branch in the tree, i.e. a specific
 * GUI path.
 */
[[codegen::luawrap]] ghoul::Dictionary guiOrder() {
    return openspace::global::renderEngine->scene()->guiTreeOrder();
}

} // namespace

#include "scene_lua_codegen.cpp"
