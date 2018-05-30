/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <ghoul/lua/ghoul_lua.h>

namespace openspace::properties {

#define REGISTER_NUMERICALPROPERTY_HEADER(CLASS_NAME, TYPE)                              \
    using CLASS_NAME = NumericalProperty<TYPE>;                                          \
                                                                                         \
    template <>                                                                          \
    std::string PropertyDelegate<NumericalProperty<TYPE>>::className();                  \
                                                                                         \
    template <>                                                                          \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className();                   \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultValue<TYPE>();                \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMinimumValue<TYPE>();         \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMaximumValue<TYPE>();         \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultSteppingValue<TYPE>();        \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromLuaValue(lua_State* state,        \
                                                                bool& success);          \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromLuaValue(lua_State* state,       \
                                                                 bool& success);         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toLuaValue(lua_State* state,          \
                                                              const TYPE& value);        \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toLuaValue(lua_State* state,         \
                                                               const TYPE& value);       \
    template <>                                                                          \
    int PropertyDelegate<TemplateProperty<TYPE>>::typeLua();                             \
    template <>                                                                          \
    int PropertyDelegate<NumericalProperty<TYPE>>::typeLua();                            \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromString(const std::string& value,  \
                                                              bool& success);            \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromString(const std::string& value, \
                                                               bool& success);           \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toString(std::string& outValue,       \
                                                            const TYPE& inValue);        \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toString(std::string& outValue,      \
                                                            const TYPE& inValue);


#define REGISTER_NUMERICALPROPERTY_SOURCE(CLASS_NAME, TYPE, DEFAULT_VALUE,               \
                                          DEFAULT_MIN_VALUE, DEFAULT_MAX_VALUE,          \
                                          DEFAULT_STEPPING, FROM_LUA_LAMBDA_EXPRESSION,  \
                                          TO_LUA_LAMBDA_EXPRESSION,                      \
                                          FROM_STRING_LAMBDA_EXPRESSION,                 \
                                          TO_STRING_LAMBDA_EXPRESSION, LUA_TYPE)         \
    template <>                                                                          \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className()                    \
    {                                                                                    \
        return #CLASS_NAME;                                                              \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    std::string PropertyDelegate<NumericalProperty<TYPE>>::className()                   \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::className();                    \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultValue<TYPE>()                 \
    {                                                                                    \
        return DEFAULT_VALUE;                                                            \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMinimumValue<TYPE>()          \
    {                                                                                    \
        return DEFAULT_MIN_VALUE;                                                        \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMaximumValue<TYPE>()          \
    {                                                                                    \
        return DEFAULT_MAX_VALUE;                                                        \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultSteppingValue<TYPE>()         \
    {                                                                                    \
        return DEFAULT_STEPPING;                                                         \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromLuaValue<TYPE>(lua_State* state,  \
                                                                      bool& success)     \
    {                                                                                    \
        return FROM_LUA_LAMBDA_EXPRESSION(state, success);                               \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromLuaValue<TYPE>(lua_State* state, \
                                                                       bool& success)    \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::fromLuaValue<TYPE>(             \
          state, success);                                                               \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toLuaValue<TYPE>(lua_State* state,    \
                                                                    const TYPE& value)   \
    {                                                                                    \
        return TO_LUA_LAMBDA_EXPRESSION(state, value);                                   \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toLuaValue<TYPE>(lua_State* state,   \
                                                                     const TYPE& value)  \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::toLuaValue<TYPE>(state, value); \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    int PropertyDelegate<TemplateProperty<TYPE>>::typeLua()                              \
    {                                                                                    \
        return LUA_TYPE;                                                                 \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    int PropertyDelegate<NumericalProperty<TYPE>>::typeLua()                             \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::typeLua();                      \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromString(const std::string& value,  \
                                                              bool& success)             \
    {                                                                                    \
        return FROM_STRING_LAMBDA_EXPRESSION(value, success);                            \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromString(const std::string& value, \
                                                               bool& success)            \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::fromString<TYPE>(               \
          value,                                                                         \
          success                                                                        \
        );                                                                               \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toString(std::string& outValue,       \
                                                            const TYPE& inValue)         \
    {                                                                                    \
        return TO_STRING_LAMBDA_EXPRESSION(outValue, inValue);                           \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toString(std::string& outValue,      \
                                                             const TYPE& inValue)        \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::toString(outValue, inValue);    \
    }


template <typename T>
const std::string NumericalProperty<T>::MinimumValueKey = "MinimumValue";

template <typename T>
const std::string NumericalProperty<T>::MaximumValueKey = "MaximumValue";

template <typename T>
const std::string NumericalProperty<T>::SteppingValueKey = "SteppingValue";

template <typename T>
const std::string NumericalProperty<T>::ExponentValueKey = "Exponent";

// Delegating constructors are necessary; automatic template deduction cannot
// deduce template argument for 'U' if 'default' methods are used as default values in
// a single constructor

template <typename T>
NumericalProperty<T>::NumericalProperty(Property::PropertyInfo info)
    : NumericalProperty<T>(
        std::move(info),
        PropertyDelegate<NumericalProperty<T>>::template defaultValue<T>(),
        PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
        PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>(),
        PropertyDelegate<NumericalProperty<T>>::template defaultSteppingValue<T>(),
        1.f
    )
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(Property::PropertyInfo info, T value)
    : NumericalProperty<T>(
        std::move(info),
        std::move(value),
        PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
        PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>(),
        PropertyDelegate<NumericalProperty<T>>::template defaultSteppingValue<T>(),
        1.f
    )
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(Property::PropertyInfo info, T value,
                                        T minimumValue, T maximumValue)
    : NumericalProperty<T>(
        std::move(info),
        std::move(value),
        std::move(minimumValue),
        std::move(maximumValue),
        PropertyDelegate<NumericalProperty<T>>::template defaultSteppingValue<T>(),
        1.f
    )
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(Property::PropertyInfo info, T value,
                                        T minimumValue, T maximumValue, T steppingValue)
    : NumericalProperty<T>(
        std::move(info),
        std::move(value),
        std::move(minimumValue),
        std::move(maximumValue),
        std::move(steppingValue),
        1.f
    )
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(Property::PropertyInfo info, T value,
                                        T minimumValue, T maximumValue, T steppingValue,
                                        float exponent)
    : TemplateProperty<T>(std::move(info), std::move(value))
    , _minimumValue(std::move(minimumValue))
    , _maximumValue(std::move(maximumValue))
    , _stepping(std::move(steppingValue))
    , _exponent(exponent)
{}

template <typename T>
std::string NumericalProperty<T>::className() const {
    return PropertyDelegate<NumericalProperty<T>>::className();
}

template <typename T>
bool NumericalProperty<T>::setLuaValue(lua_State* state) {
    bool success = false;
    T value = PropertyDelegate<NumericalProperty<T>>::template fromLuaValue<T>(
        state, success
    );
    if (success)
        TemplateProperty<T>::setValue(std::move(value));
    return success;
}

template <typename T>
bool NumericalProperty<T>::getLuaValue(lua_State* state) const {
    bool success = PropertyDelegate<NumericalProperty<T>>::template toLuaValue<T>(
        state, TemplateProperty<T>::_value
    );
    return success;
}

template <typename T>
int NumericalProperty<T>::typeLua() const {
    return PropertyDelegate<NumericalProperty<T>>::typeLua();
}

template <typename T>
bool NumericalProperty<T>::getStringValue(std::string& value) const {
    bool success = PropertyDelegate<NumericalProperty<T>>::template toString<T>(
        value, TemplateProperty<T>::_value
    );
    return success;
}

template <typename T>
bool NumericalProperty<T>::setStringValue(std::string value) {
    bool success = false;
    T thisValue = PropertyDelegate<NumericalProperty<T>>::template fromString<T>(
        value, success
    );
    if (success)
        TemplateProperty<T>::set(ghoul::any(std::move(thisValue)));
    return success;
}

template <typename T>
T NumericalProperty<T>::minValue() const {
    return _minimumValue;
}

template <typename T>
void NumericalProperty<T>::setMinValue(T value) {
    _minimumValue = std::move(value);
}

template <typename T>
T NumericalProperty<T>::maxValue() const {
    return _maximumValue;
}

template <typename T>
void NumericalProperty<T>::setMaxValue(T value) {
    _maximumValue = std::move(value);
}

template <typename T>
T NumericalProperty<T>::steppingValue() const {
    return _stepping;
}

template <typename T>
void NumericalProperty<T>::setSteppingValue(T value) {
    _stepping = std::move(value);
}

template <typename T>
float NumericalProperty<T>::exponent() const {
    return _exponent;
}

template <typename T>
void NumericalProperty<T>::setExponent(float exponent) {
    _exponent = exponent;
}

template <typename T>
std::string NumericalProperty<T>::generateAdditionalJsonDescription() const {
    std::string result = "{ ";
    result +=
      "\"" + MinimumValueKey + "\": " + luaToJson(std::to_string(_minimumValue)) + ",";
    result +=
      "\"" + MaximumValueKey + "\": " + luaToJson(std::to_string(_maximumValue)) + ",";
    result +=
      "\"" + SteppingValueKey + "\": " + luaToJson(std::to_string(_stepping)) + ",";
    result +=
      "\"" + ExponentValueKey + "\": " + luaToJson(std::to_string(_exponent));
    result += " }";
    return result;
}

template <typename T>
std::string NumericalProperty<T>::luaToJson(std::string luaValue) const {
    if(luaValue[0] == '{') {
        luaValue.replace(0, 1, "[");
    }
    if (luaValue[luaValue.size() - 1] == '}') {
        luaValue.replace(luaValue.size() - 1, 1, "]");
    }
    return luaValue;
}

template <typename T>
std::string NumericalProperty<T>::jsonValue() const {
    std::string value;
    getStringValue(value);
    return luaToJson(value);
}

template <typename T>
void NumericalProperty<T>::setInterpolationTarget(ghoul::any value) {
    T v = ghoul::any_cast<T>(std::move(value));

    _interpolationStart = TemplateProperty<T>::_value;
    _interpolationEnd = std::move(v);
}

template <typename T>
void NumericalProperty<T>::setLuaInterpolationTarget(lua_State* state) {
    bool success = false;
    T thisValue = PropertyDelegate<NumericalProperty<T>>::template fromLuaValue<T>(
        state,
        success
    );
    if (success) {
        _interpolationStart = TemplateProperty<T>::_value;
        _interpolationEnd = std::move(thisValue);
    }
}

template <typename T>
void NumericalProperty<T>::setStringInterpolationTarget(std::string value) {
    bool success = false;
    T thisValue = PropertyDelegate<NumericalProperty<T>>::template fromString<T>(
        value,
        success
    );
    if (success) {
        _interpolationStart = TemplateProperty<T>::_value;
        _interpolationEnd = std::move(thisValue);
    }
}

template <typename T>
void NumericalProperty<T>::interpolateValue(float t,
                                            ghoul::EasingFunc<float> easingFunction)
{
    if (easingFunction) {
        t = easingFunction(t);
    }
    TemplateProperty<T>::setValue(static_cast<T>(
        glm::mix(_interpolationStart, _interpolationEnd, t)
    ));
}

} // namespace openspace::properties
