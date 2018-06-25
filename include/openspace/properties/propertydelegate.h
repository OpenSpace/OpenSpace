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

#ifndef __OPENSPACE_CORE___PROPERTYDELEGATE___H__
#define __OPENSPACE_CORE___PROPERTYDELEGATE___H__

#include <string>

struct lua_State;

namespace openspace::properties {

/**
 * The PropertyDelegate class is used by (among others) the TemplateProperty and the
 * NumericalProperty classes to outsource the definitions of class names, default values,
 * etc. Using the PropertyDelegate, it is possible to create new TemplateProperty types
 * without subclassing the TemplateProperty, but rather creating a specialized instance
 * of PropertyDelegate. See
 * (https://github.com/OpenSpace/OpenSpace/wiki/Concepts-Properties) for more detailed
 * information.
 * \see TemplateProperty
 * \see NumericalProperty
 * \tparam T The full class for which this specialized instance of PropertyDelegate is
 * responsible. For example <code>T = TemplateProperty<std::string></code>.
 */
template <typename T>
class PropertyDelegate {
public:
    /**
     * This method returns the class name for the class <code>T</code>. The default
     * implementation will lead to a compile-time error if the class method is not
     * specialized.
     * \return The class name for the class <code>T</code>
     */
    static std::string className();

    /**
     * This method will return the preferred default value for the class <code>T</code>.
     * The default implementation will lead to a compile-time error if the class method is
     * not specialized.
     * \return The default value that the class <code>T</code> should use
     * \tparam U The type by which the class T is specialized. If
     * <code>T = TemplateProperty<std::string></code>, then <code>U = std::string</code>
     */
    template <typename U>
    static U defaultValue();

    /**
     * This method will return the preferred default minimum value for the class
     * <code>T</code>. The default implementation will lead to a compile-time error if the
     * class method is not specialized. This method is not used in TemplateProperty, but
     * only NumericalProperty, so the TemplateProperty does not require this method to be
     * specialized.
     * \return The default minimum value that the class <code>T</code> should use
     * \tparam U The type by which the class T is specialized. If
     * <code>T = NumericalProperty<int></code>, then <code>U = int</code>
     */
    template <typename U>
    static U defaultMinimumValue();

    /**
     * This method will return the preferred default maximum value for the class
     * <code>T</code>. The default implementation will lead to a compile-time error if the
     * class method is not specialized. This method is not used in TemplateProperty, but
     * only NumericalProperty, so the TemplateProperty does not require this method to be
     * specialized.
     * \return The default maximum value that the class <code>T</code> should use
     * \tparam U The type by which the class T is specialized. If
     * <code>T = NumericalProperty<int></code>, then <code>U = int</code>
     */
    template <typename U>
    static U defaultMaximumValue();

    /**
     * The method returns the default stepping value for the class <code>T</code> used in
     * GUI elements. The default implementation will lead to a compile-time error if the
     * class method is not specialized. This method is not used in TemplateProperty, but
     * only NumericalProperty, so the TemplateProperty does not require this method to be
     * specialized.
     * \return The default stepping that the class <code>T</code> should use
     * \tparam U The type by which the class T is specialized. If
     * <code>T = NumericalProperty<int></code>, then <code>U = int</code>
     */
    template <typename U>
    static U defaultSteppingValue();

    /**
     * This method converts the top value from the Lua stack into a value of type
     * <code>U</code> and reports the <code>success</code> back to the caller. The default
     * implementation will lead to a compile-time error if the class method is not
     * specialized.
     * \param state The Lua state from which the value is retrieved
     * \param success Will be <code>true</code> if the conversion succeeded;
     * <code>false</code> otherwise
     * \return The value that was created by converting the top value from the stack
     * \tparam U The type by which the class T is specialized. If
     * <code>T = TemplateProperty<std::string></code>, then <code>U = std::string</code>
     */
    template <typename U>
    static U fromLuaValue(lua_State* state, bool& success);

    /**
     * This method converts the passed <code>value</code>, encodes it and places it on the
     * top value of the Lua stack and returns the success back to the caller. The default
     * implementation will lead to a compile-time error if the class method is not
     * specialized.
     * \param state The Lua state from which the value is retrieved
     * \param value The value that will be converted into a Lua object
     * \return <code>true</code> if the conversion succeeded; <code>false</code> otherwise
     * \tparam U The type by which the class T is specialized. If
     * <code>T = TemplateProperty<std::string></code>, then <code>U = std::string</code>
     */
    template <typename U>
    static bool toLuaValue(lua_State* state, const U& value);

    /**
     * Returns the Lua type that will be put onto the stack in the
     * PropertyDelegate::toLuaValue method and which will be consumed by the
     * PropertyDelegate::fromLuaValue method. The returned value can belong to the set of
     * Lua types: <code>LUA_TNONE</code>, <code>LUA_TNIL</code>,
     * <code>LUA_TBOOLEAN</code>, <code>LUA_TLIGHTUSERDATA</code>,
     * <code>LUA_TNUMBER</code>, <code>LUA_TSTRING</code>, <code>LUA_TTABLE</code>,
     * <code>LUA_TFUNCTION</code>, <code>LUA_TUSERDATA</code>, or
     * <code>LUA_TTHREAD</code>. The default implementation will return
     * <code>LUA_TNONE</code>. The default implementation will lead to a compile-time
     * error if the class method is not specialized.
     * \return The Lua type that will be consumed or produced by the
     * PropertyDelegate::toLuaValue and PropertyDelegate::fromLuaValue methods.
     */
    static int typeLua();

    template <typename U>
    static U fromString(const std::string& value, bool& success);

    template <typename U>
    static bool toString(std::string& outValue, const U& inValue);
};

} // namespace openspace::properties

#include <openspace/properties/propertydelegate.inl>

#endif // __OPENSPACE_CORE___PROPERTYDELEGATE___H__
