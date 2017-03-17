/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_CORE___PROPERTY___H__
#define __OPENSPACE_CORE___PROPERTY___H__

#include <openspace/properties/propertydelegate.h>

#include <ghoul/misc/dictionary.h>
#include <functional>
#include <string>

struct lua_State;

namespace openspace {
namespace properties {

class PropertyOwner;

/**
 * A property encapsulates a value which should be user-changeable. A property almost
 * always belongs to a PropertyOwner who has taken ownership (setPropertyOwner) of the
 * Property. Per PropertyOwner, the <code>identifier</code> needs to be unique and can be
 * used as a URI. This class is an abstract base class and each subclass (most notable
 * TemplateProperty) needs to implement the methods Property::className, Property::get,
 * Property::set, Property::type(), Property::getLuaValue, Property::setLuaValue,
 * Property::getStringValue, Property::setStringValue and Property::typeLua to make full
 * use of the infrastructure.
 * The most common types can be implemented by creating a specialized instantiation of
 * TemplateProperty, which provides default implementations for these methods.
 *
 * The onChange method can be used by the PropertyOwner to listen to changes that happen
 * to the Property. The parameter is a function object that gets called after new value
 * has been set.
 * The metaData allows the developer to specify additional information about the Property
 * which might be used in GUI representations. One example would be a glm::vec4 property,
 * (Vec4Property) that can either represent a 4-dimensional position, a powerscaled
 * coordinate, a light position, or other things, requiring different GUI representations.
 * \see TemplateProperty
 * \see PropertyOwner
 */
class Property {
public:
    /**
     * The visibility classes for Property%s. The classes are strictly ordered as
     * All > Developer > User > Hidden
     */
    enum class Visibility {
        All = 3,  ///< Visible for all types, no matter what
        Developer = 2, ///< Visible in Developer mode
        User = 1, ///< Visible in User mode
        Hidden = 0 ///< Never visible
    };

    /**
     * The constructor for the property. The <code>identifier</code> needs to be unique
     * for each PropertyOwner. The <code>guiName</code> will be stored in the metaData
     * to be accessed by the GUI elements using the <code>guiName</code> key. The default
     * visibility settings is <code>true</code>, whereas the default read-only state is
     * <code>false</code>.
     * \param identifier A unique identifier for this property. It has to be unique to the
     * PropertyOwner and cannot contain any <code>.</code>s
     * \param guiName The human-readable GUI name for this Property
     * \pre \p identifier must not be empty
     * \pre \p guiName must not be empty
     */
    Property(std::string identifier, std::string guiName,
        Visibility visibility = Visibility::All);

    /**
     * The destructor taking care of deallocating all unused memory. This method will not
     * remove the Property from the PropertyOwner.
     */
    virtual ~Property();

    /**
     * This method returns the class name of the Property. The method is used by the
     * TemplateFactory to create new instances of Propertys. The returned value is almost
     * always identical to the C++ class name of the derived class.
     * \return The class name of the Property
     */
    virtual std::string className() const = 0;

    /**
     * This method returns the encapsulated value of the Property to the caller. The type
     * that is returned is determined by the type function and is up to the developer of
     * the derived class. The default implementation returns an empty ghoul::any object.
     * \return The value that is encapsulated by this Property, or an empty ghoul::any
     * object if the method was not overritten.
     */
    virtual ghoul::any get() const;

    /**
     * Sets the value encapsulated by this Property to the <code>value</code> passed to
     * this function. It is the caller's responsibility to ensure that the type contained
     * in <code>value</code> is compatible with the concrete subclass of the Property. The
     * method Property::type will return the desired type for the Property. The default
     * implementation of this method ignores the input.
     * \param value The new value that should be stored in this Property
     */
    virtual void set(ghoul::any value);

    /**
     * This method returns the type that is requested by this Property for the set method.
     * The default implementation returns the type of <code>void</code>.
     * \return The type that is requested by this Property's Property::set method
     */
    virtual const std::type_info& type() const;
    
    /**
     * This method encodes the encapsulated value of this Property at the top of the Lua
     * stack. The specific details of this serialization is up to the property developer
     * as long as the rest of the stack is unchanged. The implementation has to be
     * synchronized with the Property::setLuaValue method. The default implementation is a
     * no-op.
     * \param state The Lua state to which the value will be encoded
     * \return <code>true</code> if the encoding succeeded, <code>false</code> otherwise
     */
    virtual bool getLuaValue(lua_State* state) const;

    /**
     * This method sets the value encapsulated by this Property by deserializing the value
     * on top of the passed Lua stack. The specific details of the deserialization are up
     * to the Property developer, but they must only depend on the top element of the
     * stack and must leave all other elements unchanged. The implementation has to be
     * synchronized with the Property::getLuaValue method. The default implementation is a
     * no-op.
     * \param state The Lua state from which the value will be decoded
     * \return <code>true</code> if the decoding and setting of the value succeeded,
     * <code>false</code> otherwise
     */
    virtual bool setLuaValue(lua_State* state);

    /**
     * Returns the Lua type that will be put onto the stack in the Property::getLua method
     * and which will be consumed by the Property::setLuaValue method. The returned value
     * can belong to the set of Lua types: <code>LUA_TNONE</code>, <code>LUA_TNIL</code>,
     * <code>LUA_TBOOLEAN</code>, <code>LUA_TLIGHTUSERDATA</code>,
     * <code>LUA_TNUMBER</code>, <code>LUA_TSTRING</code>, <code>LUA_TTABLE</code>,
     * <code>LUA_TFUNCTION</code>, <code>LUA_TUSERDATA</code>, or
     * <code>LUA_TTHREAD</code>. The default implementation will return
     * <code>LUA_TNONE</code>.
     * \return The Lua type that will be consumed or produced by the Property::getLuaValue
     * and Property::setLuaValue methods.
     */
    virtual int typeLua() const;

    /**
     * This method encodes the encapsulated value of this Property as a
     * <code>std::string</code>. The specific details of this serialization is up to the
     * property developer. The implementation has to be synchronized with the
     Property::setStringValue method. The default implementation is a no-op.
     * \param value The value to which the Property will be encoded
     * \return <code>true</code> if the encoding succeeded, <code>false</code> otherwise
     */
    virtual bool getStringValue(std::string& value) const;

    /**
     * This method sets the value encapsulated by this Property by deserializing the
     * passed <code>std::string</code>. The specific details of the deserialization are up
     * to the Property developer. The implementation has to be synchronized with the
     * Property::getLuaValue method. The default implementation is a no-op.
     * \param value The value from which the Property will be decoded
     * \return <code>true</code> if the decoding and setting of the value succeeded,
     * <code>false</code> otherwise
     */
    virtual bool setStringValue(std::string value);

    /**
     * This method registers a <code>callback</code> function that will be called every
     * time if either Property:set or Property::setLuaValue was called with a value that
     * is different from the previously stored value. The callback can be removed by
     * passing an empty <code>std::function<void()></code> object.
     * \param callback The callback function that is called when the encapsulated type has
     * been successfully changed by either the Property::set or Property::setLuaValue
     * methods.
     */
    virtual void onChange(std::function<void()> callback);

    /**
     * This method returns the unique identifier of this Property.
     * \return The unique identifier of this Property
     */
    const std::string& identifier() const;
    
    /**
     * Returns the fully qualified name for this Property that uniquely identifies this
     * Property within OpenSpace. It consists of the <code>identifier</code> preceded by
     * all levels of PropertyOwner%s separated with <code>.</code>; for example:
     * <code>owner1.owner2.identifier</code>.
     * \return The fully qualified identifier for this Property
     */
    std::string fullyQualifiedIdentifier() const;

    /**
     * Returns the PropertyOwner of this Property or <code>nullptr</code>, if it does not
     * have an owner.
     *\ return The Property of this Property
     */
    PropertyOwner* owner() const;

    /**
     * Assigned the Property to a new PropertyOwner. This method does not inform the
     * PropertyOwner of this action.
     * \param owner The new PropertyOwner for this Property
     */
    void setPropertyOwner(PropertyOwner* owner);
    
    /**
     * Returns the human-readable GUI name for this Property that has been set in the
     * constructor. This method returns the same value as accessing the metaData object
     * and requesting the <code>std::string</code> stored for the <code>guiName</code>
     * key.
     * \return The human-readable GUI name for this Property
     */
    std::string guiName() const;

    /**
     * Returns the description for this Property that contains all necessary information
     * required for creating a GUI representation. The format of the description is a
     * valid Lua table, i.e., it is surrounded by a pair of <code>{</code> and
     * <code>}</code> with key,value pairs between. Each value can either be a number, a
     * string, a bool, or another table. The general values set by this base function
     * are: <code>Identifier</code>, <code>Name</code>, <code>Type</code>. All other
     * values are specific to the type and are added in a specific subclass, which require
     * the subclass to call this method first.
     * \return The descriptive text for the Property that can be used for constructing a
     * GUI representation
     */
    virtual std::string description() const;

    /**
     * Sets the identifier of the group that this Property belongs to. Property groups can
     * be used, for example, by GUI application to visually group different properties,
     * but it has no impact on the Property itself. The default value for the groupID is
     * <code>""</code>.
     * \param groupId The group id that this property should belong to
     */
    void setGroupIdentifier(std::string groupId);

    /**
     * Returns the group idenfier that this Property belongs to, or <code>""</code> if it
     * belongs to no group.
     * \return The group identifier that this Property belongs to
     */
    std::string groupIdentifier() const;

    /**
     * Sets a hint about the visibility of the Property. Each application accessing the
     * properties is free to ignore this hint. It is stored in the metaData Dictionary
     * with the key: <code>Visibility</code>.
     * \param visibility The new visibility of the Property
     */
    void setVisibility(Visibility visibility);

    /**
     * Returns this Property%'s visibility setting
     * \return This Property%'s visibility setting
     */
    Visibility visibility() const;

    /**
     * This method determines if this Property should be read-only in external
     * applications. This setting is only a hint and does not need to be followed by GUI
     * applications and does not have any effect on the Property::set,
     * Property::setLuaValue, or Property::setStringValue methods. The value is stored in
     * the metaData Dictionary with the key: <code>isReadOnly</code>. The default value is
     * <code>false</code>.
     * \param state <code>true</code> if the Property should be read only,
     * <code>false</code> otherwise
     */
    void setReadOnly(bool state);

    /**
     * This method allows the developer to give hints to the GUI about different
     * representations for the GUI. The same Property (for example Vec4Property) can be
     * used in different ways, each requiring a different input method. These values are
     * stored in the metaData object using the <code>views.</code> prefix in front of the
     * <code>option</code> parameter. See Property::ViewOptions for a default list of
     * possible options. As these are only hints, the GUI is free to ignore any suggestion
     * by the developer.
     * \param option The view option that should be modified
     * \param value Determines if the view option should be active (<code>true</code>) or
     * deactivated (<code>false</code>)
     */
    void setViewOption(std::string option, bool value = true);

    /**
     * Default view options that can be used in the Property::setViewOption method. The
     * values are: Property::ViewOptions::Color = <code>color</code>,
     * Property::ViewOptions::LightPosition = <code>lightPosition</code>,
     * Property::ViewOptions::PowerScaledScalar = <code>powerScaledScalar</code>, and
     * Property::ViewOptions::PowerScaledCoordinate = <code>powerScaledCoordinate</code>.
     */
    struct ViewOptions {
        static const char* Color;
        static const char* LightPosition;
        static const char* PowerScaledScalar;
        static const char* PowerScaledCoordinate;
    };

    /**
     * Returns the metaData that contains all information for external applications to
     * correctly display information about the Property. No information that is stored in
     * this Dictionary is necessary for the programmatic use of the Property.
     * \return The Dictionary containing all meta data information about this Property
     */
    const ghoul::Dictionary& metaData() const;

    /**
     * Returns a list of all tags that have been assigned to the Property. Useful for
     * trying to find a match for a desired batch operation on Properties.
     * \return Pointer to vector of string tags that were assigned to the Property
     */
    const std::vector<std::string>* getTags(void) const;

    /**
     * Adds a tag to the Property's list of assigned tags. Tags are useful for creating
     * groups of Properties that can be used in batch operations.
     * \param tag The string that is to be assigned to the Property
     */
    void addTag(std::string tag);

protected:
    static const char* IdentifierKey;
    static const char* NameKey;
    static const char* TypeKey;
    static const char* MetaDataKey;

    /**
     * Creates the information that is general to every Property and adds the
     * <code>Identifier</code>, <code>Name</code>, <code>Type</code>, and
     * <code>MetaData</code> keys and their values. The meta data is handles by the
     * generateMetaDataDescription method, which has to be overloaded if a concrete base
     * class wants to add meta data that is not curated by the Property class
     * \return The base description common to all Property classes
     */
    std::string generateBaseDescription() const;

    /**
     * Creates the information for the <code>MetaData</code> key-part of the Lua
     * description for the Property. The result can be included as one key-value pair in
     * the description text generated by subclasses. Only the metadata curated by the
     * Property class is used in this method.
     * \return The metadata information text for the property
     */
    virtual std::string generateMetaDataDescription() const;

    /**
     * Creates the information that is specific to each subclass of Property%s. If a
     * subclass needs to add additional information into the description, it has to
     * override this method and return the string containing all of the additional
     * information. The base implementation of the #description method will return the Lua
     * script:
     * <code>return { generateBaseDescription(), generateMetaDataDescription(),</code>
     * <code>generateAdditionalDescription()}</code>, which #generateMetaDataDescription
     * and this method being the override points to customize the behavior.
     * \return The information specific to each subclass of Property
     */ 
    virtual std::string generateAdditionalDescription() const;

    /**
     * This method must be called by all subclasses whenever the encapsulated value has
     * changed and a potential listener has to be informed.
     */
    void notifyListener();

    /// The PropetyOwner this Property belongs to, or <code>nullptr</code>
    PropertyOwner* _owner; 

    /// The identifier for this Property
    std::string _identifier;

    /// The Dictionary containing all meta data necessary for external applications
    ghoul::Dictionary _metaData;

    /// The callback function that will be invoked whenever the encapsulated value changes
    std::function<void()> _onChangeCallback;

    /// Collection of string tag(s) assigned to this property
    std::vector<std::string> _tags;
};

} // namespace properties
} // namespace openspace

#endif // __OPENSPACE_CORE___PROPERTY___H__
