/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#define REGISTER_TEMPLATEPROPERTY_HEADER(CLASS_NAME, TYPE, DEFAULT_VALUE)                \
                                                                                         \
    class CLASS_NAME : public TemplateProperty<TYPE> {                                   \
    public:                                                                              \
        CLASS_NAME(Property::PropertyInfo info, TYPE value = DEFAULT_VALUE);             \
                                                                                         \
        std::string className() const override;                                          \
                                                                                         \
        bool setLuaValue(lua_State* state) override;                                     \
                                                                                         \
        bool getLuaValue(lua_State* state) const override;                               \
                                                                                         \
        int typeLua() const override;                                                    \
                                                                                         \
        bool getStringValue(std::string& outValue) const override;                       \
                                                                                         \
        using TemplateProperty<TYPE>::operator=;                                         \
    };                                                                                   \


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
#define REGISTER_TEMPLATEPROPERTY_SOURCE(CLASS_NAME, TYPE,                               \
                                         FROM_LUA_LAMBDA_EXPRESSION,                     \
                                         TO_LUA_LAMBDA_EXPRESSION,                       \
                                         TO_STRING_LAMBDA_EXPRESSION, LUA_TYPE)          \
                                                                                         \
    CLASS_NAME::CLASS_NAME(Property::PropertyInfo info, TYPE value)                      \
        : TemplateProperty<TYPE>(info, value)                                            \
    {}                                                                                   \
                                                                                         \
    std::string CLASS_NAME::className() const                                            \
    {                                                                                    \
        return #CLASS_NAME;                                                              \
    }                                                                                    \
                                                                                         \
    bool CLASS_NAME::setLuaValue(lua_State* state)                                       \
    {                                                                                    \
        bool success = false;                                                            \
        TYPE thisValue = FROM_LUA_LAMBDA_EXPRESSION(state, success);                     \
        if (success) {                                                                   \
            set(std::any(thisValue));                                                    \
        }                                                                                \
        return success;                                                                  \
    }                                                                                    \
                                                                                         \
    bool CLASS_NAME::getLuaValue(lua_State* state) const                                 \
    {                                                                                    \
        bool success = TO_LUA_LAMBDA_EXPRESSION(state, _value);                          \
        return success;                                                                  \
    }                                                                                    \
                                                                                         \
    int CLASS_NAME::typeLua() const {                                                    \
        return LUA_TYPE;                                                                 \
    }                                                                                    \
                                                                                         \
    bool CLASS_NAME::getStringValue(std::string& outValue) const                         \
    {                                                                                    \
        bool success = TO_STRING_LAMBDA_EXPRESSION(outValue, _value);                    \
        return success;                                                                  \
    }                                                                                    \


template <typename T>
TemplateProperty<T>::TemplateProperty(Property::PropertyInfo info, T value)
    : Property(std::move(info))
    , _value(std::move(value))
{}

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
        _isValueDirty = true;
    }
}

template <typename T>
std::ostream& operator<<(std::ostream& os, const TemplateProperty<T>& obj) {
    os << obj.value();
    return os;
}

template <typename T>
std::any TemplateProperty<T>::get() const {
    return std::any(_value);
}

template <typename T>
void TemplateProperty<T>::set(std::any value) {
    T v = std::any_cast<T>(std::move(value));
    setValue(v);
}

template <typename T>
const std::type_info& TemplateProperty<T>::type() const {
    return typeid(T);
}

}  // namespace openspace::properties
