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

#ifndef __OPENSPACE_CORE___TEMPLATEPROPERTY___H__
#define __OPENSPACE_CORE___TEMPLATEPROPERTY___H__

#include <openspace/properties/property.h>

#include <openspace/properties/propertydelegate.h>

namespace openspace::properties {

/**
 * This concrete subclass of Property handles a single parameter value that is of type
 * <code>T</code>. It provides all the necessary methods to automatically access the
 * value. One notable instantiation of this class is StringProperty, using
 * <code>T = std::string</code> while NumericalProperty is a templated subclass dealing
 * with numerical parameter types.
 * The accessor operator and assignment operators are overloaded, so that the
 * TemplateProperty can be used just in the same way as a regular member variable. In the
 * case that these cannot not be used inline, the Property::get method will work.
 * The default value for the stored value of this TemplateProperty is retrieved via a call
 * to the PropertyDelegate::defaultValue method, providing the template parameter
 * <code>T</code> as argument. When a new TemplateProperty is required, that method needs
 * to be specialized for the new type or a compile-time error will occur
 * (See https://github.com/OpenSpace/OpenSpace/wiki/Concepts-Properties).
 *
 * \tparam T The type of value that is stored in this TemplateProperty
 * \see Property
 * \see NumericalProperty
 * \see PropertyDelegate
 */
template <typename T>
class TemplateProperty : public Property {
public:
    using ValueType = T;

    /**
     * The constructor initializing the TemplateProperty with the provided PropertyInfo
     * \p info and and the default value \p value.
     *
     * \param info The PropertyInfo structure that contains all the required static
     *        information for initializing this Property.
     * \param value The default value of the Property
     *
     * \pre \p info.identifier must not be empty
     * \pre \p info.guiName must not be empty
     */
    TemplateProperty(Property::PropertyInfo info,
        T value = PropertyDelegate<TemplateProperty<T>>::template defaultValue<T>());

    /**
     * Returns the class name for this TemplateProperty. The default implementation makes
     * a call to the PropertyDelegate::className method with the template parameter
     * <code>T</code> as argument. For this to work, that method needs to be specialized
     * to return the correct class name for the new template parameter T, or a
     * compile-time error will occur.
     *
     * \return The class name for the TemplateProperty
     */
    virtual std::string className() const override;

    /**
     * Returns the stored value packed into a ghoul::any object.
     *
     * \return The stored value packed into a ghoul::any object
     */
    virtual ghoul::any get() const override;

    /**
     * Sets the value from the provided ghoul::any object. If the types between
     * <code>T</code> and <code>value</code> disagree, an error is logged and the stored
     * value remains unchanged.
     *
     * \param value The value that is used to set this Property
     */
    virtual void set(ghoul::any value) override;

    /**
     * Returns the <code>std::type_info</code> describing the template parameter
     * <code>T</code>. It can be used to test against a ghoul::any value before trying to
     * assign it.
     *
     * \return The type info object describing the template parameter <code>T</code>
     */
    virtual const std::type_info& type() const override;

    /**
     * This method encodes the stored value into a Lua object and pushes that object onto
     * the stack. The encoding is performed by calling PropertyDelegate::toLuaValue with
     * the template parameter <code>T</code> as an argument. This method has to be
     * specialized for each new type, or a compile-time error will occur.
     *
     * \param state The Lua state onto which the encoded object will be pushed
     * \return \c true if the encoding succeeded; \c false otherwise
     */
    bool getLuaValue(lua_State* state) const override;

    /**
     * Sets the value of this TemplateProprty by decoding the object at the top of the Lua
     * stack and, if successful, assigning it using the Property::set method. The decoding
     * is performed by calling the PropertyDelegate::fromLuaValue with the template
     * parameter <code>T</code> as argument. If the decoding is successful, the new value
     * is set, otherwise it remains unchanged.
     *
     * \param state The Lua state from which the value will be decoded
     * \return \c true if the decoding succeeded; \c false otherwise
     */
    bool setLuaValue(lua_State* state) override;

    /// \see Property::typeLua
    int typeLua() const override;

    bool getStringValue(std::string& value) const override;

    bool setStringValue(std::string value) override;

    /**
     * Returns the description for this TemplateProperty as a Lua script that returns a
     * table on execution.
     *
     * \return The description for this TemplateProperty
     */
    //virtual std::string description() override;

    /**
     * This operator allows the TemplateProperty to be used almost transparently as if it
     * was of the type <code>T</code>. It makes assignments such as
     * <code>T v = property;</code> possible by allowing implicit casts (even though,
     * internally, not casts are performed. This method is next to zero overhead).
     *
     * \return The internal representation of the Property
     */
    operator T();

    /**
     * This operator allows the TemplateProperty to be used almost transparently as if it
     * was of the type <code>T</code>. It makes assignments such as
     * <code>T v = property;</code> possible by allowing implicit casts (even though,
     * internally, not casts are performed. This method is next to zero overhead).
     *
     * \return The internal representation of the Property
     */
    operator T() const;

    /**
     * The assignment operator allows the TemplateProperty's value to be set without using
     * the TemplateProperty::set method. It will be done internally by this method and it
     * allows assignments such as <code>prop = T(1)</code>.
     *
     * \param val The value that should be set.
     */
    TemplateProperty<T>& operator=(T val);

    /**
     * These method sets the stored value to the provided value <code>val</code>,
     * moving it into place. This move only happens if the provided value <code>val</code>
     * is different from the stored value, which needs an operator== to exist for the type
     * <code>T</code>. If the value are different, the listeners are notified.
     *
     * \param val The new value for this TemplateProperty
     */
    virtual void setValue(T val);

    /**
     * Returns the currently stored value.
     *
     * \return The currently stored value
     */
    T value() const;

protected:
    /// The value that this TemplateProperty currently stores
    T _value;
};

} // namespace openspace::properties

#include "openspace/properties/templateproperty.inl"

#endif // __OPENSPACE_CORE___TEMPLATEPROPERTY___H__
