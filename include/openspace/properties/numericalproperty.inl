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

#include <ghoul/lua/ghoul_lua.h>

namespace openspace {
namespace properties {

#define REGISTER_NUMERICALPROPERTY_HEADER(CLASS_NAME, TYPE)                              \
    typedef NumericalProperty<TYPE> CLASS_NAME;                                          \
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
	template <>																		     \
	template <>																			 \
	TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultSteppingValue<TYPE>();		 \
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
                                                              TYPE value);               \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toLuaValue(lua_State* state,         \
                                                               TYPE value);              \
    template <>                                                                          \
    int PropertyDelegate<TemplateProperty<TYPE>>::typeLua();                             \
    template <>                                                                          \
    int PropertyDelegate<NumericalProperty<TYPE>>::typeLua();                            \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromString(std::string value,         \
                                                              bool& success);            \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromString(std::string value,        \
                                                              bool& success);            \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toString(std::string& outValue,       \
                                                            TYPE inValue);               \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toString(std::string& outValue,      \
                                                            TYPE inValue);


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
	template <>																		     \
	template <>																			 \
	TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultSteppingValue<TYPE>()		 \
	{ 																					 \
		return DEFAULT_STEPPING;														 \
	}																					 \
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
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromLuaValue<TYPE>(                  \
          lua_State* state, bool& success)                                               \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::fromLuaValue<TYPE>(state,       \
                                                                            success);    \
	}                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toLuaValue<TYPE>(lua_State * state,   \
                                                                    TYPE value)          \
    {                                                                                    \
        return TO_LUA_LAMBDA_EXPRESSION(state, value);                                   \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toLuaValue<TYPE>(lua_State * state,  \
                                                                     TYPE value)         \
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
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromString(std::string value,         \
                                                              bool& success)             \
    {                                                                                    \
        return FROM_STRING_LAMBDA_EXPRESSION(value, success);                            \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromString(std::string value,        \
                                                              bool& success)             \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::fromString<TYPE>(value,         \
                                                                          success);      \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toString(std::string& outValue,       \
                                                            TYPE inValue)                \
    {                                                                                    \
        return TO_STRING_LAMBDA_EXPRESSION(outValue, inValue);                           \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<NumericalProperty<TYPE>>::toString(std::string& outValue,      \
                                                             TYPE inValue)               \
    {                                                                                    \
        return PropertyDelegate<TemplateProperty<TYPE>>::toString(outValue, inValue);    \
    }


template <typename T>
const std::string NumericalProperty<T>::MinimumValueKey = "MinimumValue";

template <typename T>
const std::string NumericalProperty<T>::MaximumValueKey = "MaximumValue";

template <typename T>
const std::string NumericalProperty<T>::SteppingValueKey = "SteppingValue";

// Delegating constructors are necessary; automatic template deduction cannot
// deduce template argument for 'U' if 'default' methods are used as default values in
// a single constructor    
        
template <typename T>
NumericalProperty<T>::NumericalProperty(std::string identifier, std::string guiName)
    : NumericalProperty<T>(
		std::move(identifier), std::move(guiName),
		PropertyDelegate<NumericalProperty<T>>::template defaultValue<T>(),
		PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
		PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>(),
		PropertyDelegate<NumericalProperty<T>>::template defaultSteppingValue<T>()
	)
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(std::string identifier,
                                        std::string guiName, T value)
    : NumericalProperty<T>(
		std::move(identifier), std::move(guiName), std::move(value),
		PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
		PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>(),
		PropertyDelegate<NumericalProperty<T>>::template defaultSteppingValue<T>()
	)
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(std::string identifier, std::string guiName,
										T value, T minimumValue, T maximumValue)
	: NumericalProperty<T>(
		std::move(identifier) , std::move(guiName), std::move(value),
		std::move(minimumValue), std::move(maximumValue),
		PropertyDelegate<NumericalProperty<T>>::template defaultSteppingValue<T>()
	)
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(std::string identifier,
                                        std::string guiName, T value,
                                        T minimumValue, T maximumValue, T steppingValue)
    : TemplateProperty<T>(std::move(identifier), std::move(guiName), std::move(value))
    , _minimumValue(std::move(minimumValue))
    , _maximumValue(std::move(maximumValue))
	, _stepping(std::move(steppingValue))
{}

template <typename T>
std::string NumericalProperty<T>::className() const {
    return PropertyDelegate<NumericalProperty<T>>::className();
}

template <typename T>
bool NumericalProperty<T>::setLuaValue(lua_State* state)
{
	bool success = false;
	T value = PropertyDelegate<NumericalProperty<T>>::template fromLuaValue<T>(state, success);
	if (success)
		TemplateProperty<T>::setValue(value);
	return success;
}

template <typename T>
bool NumericalProperty<T>::getLuaValue(lua_State* state) const
{
	bool success = PropertyDelegate<NumericalProperty<T>>::template toLuaValue<T>(state, TemplateProperty<T>::_value);
	return success;
}

template <typename T>
int NumericalProperty<T>::typeLua() const {
	return PropertyDelegate<NumericalProperty<T>>::typeLua();
}

template <typename T>
bool NumericalProperty<T>::getStringValue(std::string& value) const {
    bool success = PropertyDelegate<NumericalProperty<T>>::template toString<T>(value, TemplateProperty<T>::_value);
    return success;
}

template <typename T>
bool NumericalProperty<T>::setStringValue(std::string value) {
    bool success = false;
    T thisValue = PropertyDelegate<NumericalProperty<T>>::template fromString<T>(value, success);
    if (success)
        TemplateProperty<T>::set(ghoul::any(thisValue));
    return success;
}

template <typename T>
T NumericalProperty<T>::minValue() const {
	return _minimumValue;
}

template <typename T>
T NumericalProperty<T>::maxValue() const {
	return _maximumValue;
}

template <typename T>
std::string NumericalProperty<T>::generateAdditionalDescription() const {
	std::string result;
	result += MinimumValueKey  + " = " + std::to_string(_minimumValue) + ",";
	result += MaximumValueKey  + " = " + std::to_string(_maximumValue) + ",";
	result += SteppingValueKey + " = " + std::to_string(_stepping);
	return result;
}

} // namespace properties
} // namespace openspace
