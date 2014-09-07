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

#ifndef __TEMPLATEPROPERTY_H__
#define __TEMPLATEPROPERTY_H__

#include <openspace/properties/property.h>

namespace openspace {
namespace properties {

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
 * (See http://openspace.itn.liu.se/trac/wiki/guides/properties).
 * \tparam T The type of value that is stored in this TemplateProperty
 * \see Property
 * \see NumericalProperty
 * \see PropertyDelegate
 */
template <typename T>
class TemplateProperty : public Property {
public:
	/**
	 * The constructor initializing the TemplateProperty with the provided
	 * <code>identifier</code> and human-readable <code>guiName</code>. The default value
	 * for the stored type <code>T</code> is retrieved using the PropertyDelegate's
	 * PropertyDelegate::defaultValue method, which must be specialized for new types or
	 * a compile-error will occur.
	 * \param identifier The identifier that is used for this TemplateProperty
	 * \param guiName The human-readable GUI name for this TemplateProperty
	 */
    TemplateProperty(std::string identifier, std::string guiName);

	/**
	 * The constructor initializing the TemplateProperty with the provided
	 * <code>identifier</code>, human-readable <code>guiName</code> and provided
	 * <code>value</code>.
	 */
    TemplateProperty(std::string identifier, std::string guiName, T value);

	/**
	 * Returns the class name for this TemplateProperty. The default implementation makes
	 * a call to the PropertyDelegate::className method with the template parameter
	 * <code>T</code> as argument. For this to work, that method needs to be specialized
	 * to return the correct class name for the new template parameter T, or a
	 * compile-time error will occur.
	 * \return The class name for the TemplateProperty
	 */
    virtual std::string className() const override;

	/**
	 * Returns the stored value packed into a <code>boost::any</code> object.
	 * \return The stored value packed into a <code>boost::any</code> object
	 */
    virtual boost::any get() const override;

	/**
	 * Sets the value fro the provided <code>boost::any</code> object. If the types
	 * between <code>T</code> and <code>value</code> disagree, an error is logged and the
	 * stored value remains unchanged.
	 */
    virtual void set(boost::any value) override;

	/**
	 * Returns the <code>std::type_info</code> describing the template parameter
	 * <code>T</code>. It can be used to test against a <code>boost::any</code> value
	 * before trying to assign it.
	 * \return The type info object describing the template parameter <code>T</code>
	 */
    virtual const std::type_info& type() const override;

	/**
	 * This method encodes the stored value into a Lua object and pushes that object onto
	 * the stack. The encoding is performed by calling PropertyDelegate::toLuaValue with
	 * the template parameter <code>T</code> as an argument. This method has to be
	 * specialized for each new type, or a compile-time error will occur.
	 * \param state The Lua state onto which the encoded object will be pushed
	 * \return <code>true</code> if the encoding succeeded; <code>false</code> otherwise
	 */
	bool getLua(lua_State* state) const override;

	/**
	 * Sets the value of this TemplateProprty by decoding the object at the top of the Lua
	 * stack and, if successful, assigning it using the Property::set method. The decoding
	 * is performed by calling the PropertyDelegate::fromLuaValue with the template
	 * parameter <code>T</code> as argument. If the decoding is successful, the new value
	 * is set, otherwise it remains unchanged.
	 * \param state The Lua state from which the value will be decoded
	 * \return <code>true</code> if the decoding succeeded; <code>false</code> otherwise
	 */
	bool setLua(lua_State* state) override;

	/// \see Property::typeLua
	int typeLua() const override;

	/**
	 * This operator allows the TemplateProperty to be used almost transparently as if it
	 * was of the type <code>T</code>. It makes assignments such as
	 * <code>T v = property;</code> possible by allowing implicit casts (even though,
	 * internally, not casts are performed. This method is next to zero overhead).
	 */
    operator T();

	/**
	 * The assignment operator allows the TemplateProperty's value to be set without using
	 * the TemplateProperty::set method. It will be done internally by thos method and it
	 * allows assigments such as <code>prop = T(1)</code>.
	 * \param val The value that should be set.
	 */
    TemplateProperty<T>& operator=(T val);

	/**
	 * These method sets the stored value to the provided value <code>val</code>,
	 * moving it into place. This move only happens if the provided value <code>val</code>
	 * is different from the stored value, which needs an operator== to exist for the type
	 * <code>T</code>. If the value are different, the listeners are notified.
	 * \param val The new value for this TemplateProperty
	 */
    void setValue(T val);

	/**
	 * Returns the currently stored value.
	 * \return The currently stored value
	 */
    T value() const;

protected:
	/// The value that this TemplateProperty currently stores
    T _value;
};

} // namespace properties
} // namespace openspace

#include "openspace/properties/templateproperty.inl"

#endif // __TEMPLATEPROPERTY_H__
