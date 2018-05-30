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

namespace openspace::properties {

    // The following macros can be used to quickly generate the necessary PropertyDelegate
    // specializations required by the TemplateProperty class. Use the
    // REGISTER_TEMPLATEPROPERTY_HEADER    macro in the header file and the
    // REGISTER_TEMPLATEPROPERTY_SOURCE macro in the source file of your new
    // specialization of a TemplateProperty


    // CLASS_NAME = The string that the Property::className() should return as well as the
    //              C++ class name for which a typedef will be created
    // TYPE       = The template parameter T for which the TemplateProperty is specialized
#define REGISTER_TEMPLATEPROPERTY_HEADER(CLASS_NAME, TYPE)                               \
    using CLASS_NAME = TemplateProperty<TYPE>;                                           \
                                                                                         \
    template <>                                                                          \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className();                   \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::defaultValue<TYPE>();                 \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromLuaValue(lua_State* state,        \
                                                                bool& success);          \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toLuaValue(lua_State* state,          \
                                                              const TYPE& value);        \
                                                                                         \
    template <>                                                                          \
    int PropertyDelegate<TemplateProperty<TYPE>>::typeLua();                             \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromString(const std::string& value,  \
                                                              bool& success);            \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toString(std::string& outValue,       \
                                                            const TYPE& inValue);


// CLASS_NAME = The string that the Property::className() should return as well as the
//              C++ class name for which a typedef will be created
// TYPE       = The template parameter T for which the TemplateProperty is specialized
// DEFAULT_VALUE = The value (as type T) which should be used as a default value
// FROM_LUA_LAMBDA_EXPRESSION = A lambda expression receiving a lua_State* as the first
//                              parameter, a bool& as the second parameter and returning
//                                a value T. It is used by the fromLua method of
//                                TemplateProperty. The lambda expression must extract the
//                                stored value from the lua_State, return the value and
//                                report success in the second argument
// TO_LUA_LAMBDA_EXPRESSION = A lambda expression receiving a lua_State*, a value T and
//                            returning a bool. The lambda expression must encode the
//                              value T onto the lua_State stack and return the success
// LUA_TYPE                 = The Lua type that will be produced/consumed by the previous
//                            Lambda expressions
#define REGISTER_TEMPLATEPROPERTY_SOURCE(CLASS_NAME, TYPE, DEFAULT_VALUE,                \
                                         FROM_LUA_LAMBDA_EXPRESSION,                     \
                                         TO_LUA_LAMBDA_EXPRESSION,                       \
                                         FROM_STRING_LAMBDA_EXPRESSION,                  \
                                         TO_STRING_LAMBDA_EXPRESSION, LUA_TYPE)          \
    template <>                                                                          \
    std::string PropertyDelegate<TemplateProperty<TYPE>>::className()                    \
    {                                                                                    \
        return #CLASS_NAME;                                                              \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::defaultValue<TYPE>()                  \
    {                                                                                    \
        return DEFAULT_VALUE;                                                            \
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
    bool PropertyDelegate<TemplateProperty<TYPE>>::toLuaValue<TYPE>(lua_State* state,    \
                                                                    const TYPE& value)   \
    {                                                                                    \
        return TO_LUA_LAMBDA_EXPRESSION(state, value);                                   \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    int PropertyDelegate<TemplateProperty<TYPE>>::typeLua() {                            \
        return LUA_TYPE;                                                                 \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    TYPE PropertyDelegate<TemplateProperty<TYPE>>::fromString(const std::string& value,  \
                                                                 bool& success)          \
    {                                                                                    \
        return FROM_STRING_LAMBDA_EXPRESSION(value, success);                            \
    }                                                                                    \
                                                                                         \
    template <>                                                                          \
    template <>                                                                          \
    bool PropertyDelegate<TemplateProperty<TYPE>>::toString(std::string& outValue,       \
                                                            const TYPE& inValue)         \
    {                                                                                    \
        return TO_STRING_LAMBDA_EXPRESSION(outValue, inValue);                           \
    }                                                                                    \


template <typename T>
TemplateProperty<T>::TemplateProperty(Property::PropertyInfo info, T value)
    : Property(std::move(info))
    , _value(std::move(value))
{}

template <typename T>
std::string TemplateProperty<T>::className() const {
    return PropertyDelegate<TemplateProperty<T>>::className();
}

template <typename T>
TemplateProperty<T>::operator T() {
    return _value;
}

template <typename T>
TemplateProperty<T>::operator T() const {
    return _value;
}

template <typename T>
TemplateProperty<T>& TemplateProperty<T>::operator=(T val) {
    setValue(val);
    return *this;
}

template <typename T>
T openspace::properties::TemplateProperty<T>::value() const {
    return _value;
}

template <typename T>
void openspace::properties::TemplateProperty<T>::setValue(T val) {
    if (val != _value) {
        _value = std::move(val);
        notifyChangeListeners();
    }
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const TemplateProperty<T>& obj) {
    os << obj.value();
    return os;
}

template <typename T>
ghoul::any TemplateProperty<T>::get() const {
    return ghoul::any(_value);
}

template <typename T>
void TemplateProperty<T>::set(ghoul::any value) {
    T v = ghoul::any_cast<T>(std::move(value));
    if (v != _value) {
        _value = std::move(v);
        notifyChangeListeners();
    }
}

template <typename T>
const std::type_info& TemplateProperty<T>::type() const {
    return typeid(T);
}

template <typename T>
bool TemplateProperty<T>::getLuaValue(lua_State* state) const {
    bool success = PropertyDelegate<TemplateProperty<T>>::template toLuaValue<T>(
        state,
        _value
    );
    return success;
}

template <typename T>
bool TemplateProperty<T>::setLuaValue(lua_State* state) {
    bool success = false;
    T thisValue = PropertyDelegate<TemplateProperty<T>>::template fromLuaValue<T>(
        state,
        success
    );
    if (success) {
        set(ghoul::any(thisValue));
    }
    return success;
}

template <typename T>
int TemplateProperty<T>::typeLua() const {
    return PropertyDelegate<TemplateProperty<T>>::typeLua();
}

template <typename T>
bool TemplateProperty<T>::getStringValue(std::string& value) const {
    bool success = PropertyDelegate<TemplateProperty<T>>::template toString<T>(
        value,
        _value
    );
    return success;
}

template <typename T>
bool TemplateProperty<T>::setStringValue(std::string value) {
    bool success = false;
    T thisValue = PropertyDelegate<TemplateProperty<T>>::template fromString<T>(
        value,
        success
    );
    if (success) {
        set(ghoul::any(thisValue));
    }
    return success;
}

}  // namespace openspace::properties
