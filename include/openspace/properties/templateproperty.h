/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

namespace openspace::properties {

/**
 * This subclass of Property handles a single parameter value that is of type
 * `T`. It provides all the necessary methods to automatically access the
 * value. One notable instantiation of this class is StringProperty, using
 * `T = std::string` while NumericalProperty is a templated subclass dealing
 * with numerical parameter types.
 * The accessor operator and assignment operators are overloaded, so that the
 * TemplateProperty can be used just in the same way as a regular member variable. In the
 * case that these cannot not be used inline, the Property::get method will work.
 *
 * Each instantiation of this class should provide a constructor that deals with the
 * default value for that specific type `T`, so that a property can be
 * created from just a Property::PropertyInfo object.
 *
 * \tparam T The type of value that is stored in this TemplateProperty
 * \see Property
 * \see NumericalProperty
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
    TemplateProperty(Property::PropertyInfo info, T value);

    /**
     * Returns the class name for this TemplateProperty. This method has to be
     * specialized for each new type.
     *
     * \return The class name for the TemplateProperty
     */
    virtual std::string_view className() const override = 0;

    /**
     * Returns the stored value packed into a ghoul::any object.
     *
     * \return The stored value packed into a ghoul::any object
     */
    virtual std::any get() const override;

    /**
     * Sets the value from the provided ghoul::any object. If the types between
     * `T` and `value` disagree, an error is logged and the stored
     * value remains unchanged.
     *
     * \param value The value that is used to set this Property
     */
    virtual void set(std::any value) final;

    /**
     * Returns the `std::type_info` describing the template parameter
     * `T`. It can be used to test against a ghoul::any value before trying to
     * assign it.
     *
     * \return The type info object describing the template parameter `T`
     */
    virtual const std::type_info& type() const override;

    /**
     * This method encodes the stored value into a Lua object and pushes that object onto
     * the stack.
     *
     * \param state The Lua state onto which the encoded object will be pushed
     * \return `true` if the encoding succeeded; `false` otherwise
     */
    virtual bool getLuaValue(lua_State* state) const override;

    /**
     * Sets the value of this TemplateProperty by decoding the object at the top of the
     * stack and, if successful, assigning it using the Property::set method. If the
     * decoding is successful, the new value is set, otherwise it remains unchanged.
     *
     * \param state The Lua state from which the value will be decoded
     * \return `true` if the decoding succeeded; `false` otherwise
     */
    virtual bool setLuaValue(lua_State* state) override;

    /// \see Property::typeLua
    virtual int typeLua() const override = 0;

    /**
     * This method encodes the stored value into a std::string object. The resulting
     * encoding must also be a valid JSON representation fo the property.
     *
     * \return The string representation of the stored property value
     */
    virtual std::string stringValue() const override;

    /**
     * Returns the description for this TemplateProperty as a Lua script that returns a
     * table on execution.
     *
     * \return The description for this TemplateProperty
     */
    //virtual std::string description() override;

    /**
     * This operator allows the TemplateProperty to be used almost transparently as if it
     * was of the type `T`. It makes assignments such as
     * `T v = property;` possible by allowing implicit casts (even though,
     * internally, not casts are performed. This method is next to zero overhead).
     *
     * \return The internal representation of the Property
     */
    operator T();

    /**
     * This operator allows the TemplateProperty to be used almost transparently as if it
     * was of the type `T`. It makes assignments such as
     * `T v = property;` possible by allowing implicit casts (even though,
     * internally, not casts are performed. This method is next to zero overhead).
     *
     * \return The internal representation of the Property
     */
    operator T() const;

    /**
     * The assignment operator allows the TemplateProperty's value to be set without using
     * the TemplateProperty::set method. It will be done internally by this method and it
     * allows assignments such as `prop = T(1)`.
     *
     * \param val The value that should be set
     */
    TemplateProperty<T>& operator=(T val);

    /**
     * This method sets the stored value to the provided value `val`,
     * moving it into place. The move only happens if the provided value `val`
     * is different from the stored value, which needs an operator== to exist for the type
     * `T`. If the value is different, the listeners are notified.
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
    /**
     * Decodes the object at the top of the stack to a value of the type `T`
     * and returns it. This method has to be specialized for each new type.
     *
     * \param state The Lua state from which the value will be decoded
     * \return the decoded value
     */
    virtual T fromLuaConversion(lua_State* state) const = 0;

    /**
     * Encodes the stored value into a Lua object and pushes that object onto
     * the stack. This method has to be specialized for each new type.
     *
     * \param state The Lua state onto which the encoded object will be pushed
     */
    virtual void toLuaConversion(lua_State* state) const = 0;

    /**
     * Encodes the stored value into a std::string object, in a format that is a valid
     * JSON representation of the property. This method has to be specialized for each
     * new type.
     *
     * \return The resulting encoding
     */
    virtual std::string toStringConversion() const = 0;

    /// The value that this TemplateProperty currently stores
    T _value;
};

} // namespace openspace::properties

#include "openspace/properties/templateproperty.inl"

#endif // __OPENSPACE_CORE___TEMPLATEPROPERTY___H__
