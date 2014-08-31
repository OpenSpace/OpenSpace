/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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

#define REGISTER_NUMERICALPROPERTY_HEADER(CLASS_NAME, TYPE) \
    typedef NumericalProperty<TYPE> CLASS_NAME; \
    template <> std::string PropertyDelegate<NumericalProperty<TYPE>>::className(); \
    template <> std::string PropertyDelegate<TemplateProperty<TYPE>>::className(); \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultValue<TYPE>(); \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMinimumValue<TYPE>(); \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMaximumValue<TYPE>(); \
	template <> template <> \
	TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromLuaValue( \
									lua_State* state, bool& success); \
	template <> template <> \
	bool PropertyDelegate<NumericalProperty<TYPE>>::toLuaValue( \
									lua_State* state, TYPE value);


#define REGISTER_NUMERICALPROPERTY_SOURCE(CLASS_NAME, TYPE, \
    DEFAULT_VALUE, DEFAULT_MIN_VALUE, DEFAULT_MAX_VALUE, DEFAULT_STEPPING, \
	FROM_LUA_LAMBDA_EXPRESSION, TO_LUA_LAMBDA_EXPRESSION) \
    template <> \
    std::string PropertyDelegate<NumericalProperty<TYPE>>::className() { \
    return #CLASS_NAME; \
} \
    template <> \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className() { \
		return #CLASS_NAME; \
} \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultValue<TYPE>() { \
		return DEFAULT_VALUE; \
} \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMinimumValue<TYPE>() { \
		return DEFAULT_MIN_VALUE; \
} \
    template <> template <> \
    TYPE PropertyDelegate<NumericalProperty<TYPE>>::defaultMaximumValue<TYPE>() { \
		return DEFAULT_MAX_VALUE; \
} \
	template <> template <> \
	TYPE PropertyDelegate<NumericalProperty<TYPE>>::fromLuaValue<TYPE>( \
													lua_State* state, bool& success) { \
		return FROM_LUA_LAMBDA_EXPRESSION(state, success); \
} \
	template <> template <> \
	bool PropertyDelegate<NumericalProperty<TYPE>>::toLuaValue<TYPE>( \
													lua_State* state, TYPE value) { \
		return TO_LUA_LAMBDA_EXPRESSION(state, value); \
}
	

// Delegating constructors are necessary; automatic template deduction cannot
// deduce template argument for 'U' if 'default' methods are used as default values in
// a single constructor    
        
template <typename T>
NumericalProperty<T>::NumericalProperty(std::string identifier, std::string guiName)
    : NumericalProperty<T>(std::move(identifier), std::move(guiName),
    PropertyDelegate<NumericalProperty<T>>::template defaultValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>())
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(std::string identifier,
                                        std::string guiName, T value)
    : NumericalProperty<T>(std::move(identifier), std::move(guiName), std::move(value),
    PropertyDelegate<NumericalProperty<T>>::template defaultMinimumValue<T>(),
    PropertyDelegate<NumericalProperty<T>>::template defaultMaximumValue<T>())
{}

template <typename T>
NumericalProperty<T>::NumericalProperty(std::string identifier,
                                        std::string guiName, T value,
                                        T minimumValue, T maximumValue)
    : TemplateProperty<T>(std::move(identifier), std::move(guiName), std::move(value))
    , _minimumValue(std::move(minimumValue))
    , _maximumValue(std::move(maximumValue))
{}


template <typename T>
std::string NumericalProperty<T>::className() const {
    return PropertyDelegate<NumericalProperty<T>>::className();
}

template <typename T>
bool NumericalProperty<T>::setLua(lua_State* state)
{
	bool success;
	T value = PropertyDelegate<NumericalProperty<T>>::fromLuaValue<T>(state, success);
	if (success)
		set(value);
	return success;
}

template <typename T>
bool NumericalProperty<T>::getLua(lua_State* state) const
{
	bool success = PropertyDelegate<NumericalProperty<T>>::toLuaValue<T>(state, TemplateProperty::_value);
	return success;
}

template <typename T>
int openspace::properties::NumericalProperty<T>::typeLua() const {
	return LUA_TNUMBER;
}


} // namespace properties
} // namespace openspace
