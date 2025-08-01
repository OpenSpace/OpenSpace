/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/util/json_helper.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/easing.h>
#include <ghoul/lua/lua_types.h>
#include <functional>
#include <string>
#include <string_view>

struct lua_State;

namespace openspace::properties {

class PropertyOwner;

/**
 * A property encapsulates a value which should be user-changeable. A property almost
 * always belongs to a PropertyOwner who has taken ownership (setPropertyOwner) of the
 * Property. Per PropertyOwner, the `identifier` needs to be unique and can be used as a
 * URI. This class is an abstract base class and each subclass (most notable
 * TemplateProperty) needs to implement the methods Property::className, Property::get,
 * Property::set, Property::type(), Property::getLuaValue, Property::setLuaValue,
 * Property::getStringValue, and Property::typeLua to make full use of the infrastructure.
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
 *
 * \see TemplateProperty
 * \see PropertyOwner
 */
class Property {
public:
    BooleanType(NeedsConfirmation);

    /**
     * The visibility classes for Property%s. The classes are strictly ordered as
     * Hidden > Developer > AdvancedUser > User > NoviceUser > Always
     */
    enum class Visibility {
        /// Never visible
        Hidden = 5,
        /// Visible in Developer mode
        Developer = 4,
        /// Visible in Advanced User mode
        AdvancedUser = 3,
        /// Visible in User mode
        User = 2,
        /// Visible in Novice User mode
        NoviceUser = 1,
        /// Visible for all types, no matter what
        Always = 0,

        /// The default visibility for properties
        Default = Always
    };

    /**
     * This structure is passed to the constructor of a Property and contains the unique
     * identifier, a GUI name and descriptive text that are both user facing.
     */
    struct PropertyInfo {
        /**
         * GCC requires an explicit constructor here, as it does not handle the default
         * argument for the struct initialization.
         */
        constexpr PropertyInfo(const char* ident, const char* gui, const char* desc,
                              NeedsConfirmation needsConfirmation = NeedsConfirmation::No)
            : identifier(ident)
            , guiName(gui)
            , description(desc)
            , needsConfirmation(needsConfirmation)
        {}

        constexpr PropertyInfo(const char* ident, const char* gui, const char* desc,
                               Visibility vis,
                              NeedsConfirmation needsConfirmation = NeedsConfirmation::No)
            : identifier(ident)
            , guiName(gui)
            , description(desc)
            , visibility(vis)
            , needsConfirmation(needsConfirmation)
        {}

        /// The unique identifier that is part of the fully qualified URI of this Property
        const char* identifier;
        /// The name that is displayed in the user interface
        const char* guiName;
        /// The user facing description of this Property
        const char* description;
        /// Determines the visibility of this Property in the user interface
        Visibility visibility = Visibility::Default;
        /// Determines if the Property require confirmation upon value change
        NeedsConfirmation needsConfirmation = NeedsConfirmation::No;
    };

    /// An OnChangeHandle is returned by the onChange method to uniquely identify an
    /// onChange callback
    using OnChangeHandle = uint32_t;

    /// An OnChangeHandle is returned by the onChange method to uniquely identify an
    /// onDelete callback
    using OnDeleteHandle = uint32_t;

    /// An OnMetaDataChangeHandle is returned by the onMetaDataChange method to uniquely
    /// identify an onMetaDataChange callback
    using OnMetaDataChangeHandle = uint32_t;

    /// This OnChangeHandle can be used to remove all onChange callbacks from this
    /// Property
    constexpr static OnChangeHandle OnChangeHandleAll =
        std::numeric_limits<OnChangeHandle>::max();

    /// This OnMetaDataChangeHandle can be used to remove all onMetaDataChange callbacks
    /// from this Property
    constexpr static OnMetaDataChangeHandle OnMetaDataChangeHandleAll =
        std::numeric_limits<OnMetaDataChangeHandle >::max();

    /**
     * The constructor for the property. The \p info (see PropertyInfo) contains
     * necessary information for this Property. PropertyInfo::identifier needs to be
     * unique for each PropertyOwner. The PropertyInfo::guiName will be stored in the
     * metaData to be accessed by the GUI elements using the PropertyInfo::guiName key.
     * The default visibility settings is Visibility::Always, whereas the default
     * read-only state is `false`.
     *
     * \param info The PropertyInfo structure that contains all the required static
     *        information for initializing this Property.
     *
     * \pre \p info.identifier must not be empty
     * \pre \p info.guiName must not be empty
     */
    explicit Property(PropertyInfo info);

    /**
     * The destructor taking care of deallocating all unused memory. This method will not
     * remove the Property from the PropertyOwner.
     */
    virtual ~Property();

    /**
     * This method returns the class name of the Property. The method is used by the
     * TemplateFactory to create new instances of Propertys. The returned value is almost
     * always identical to the C++ class name of the derived class.
     *
     * \return The class name of the Property
     */
    virtual std::string_view className() const = 0;

    /**
     * This method returns the type that is requested by this Property for the set method.
     * The default implementation returns the type of `void`.
     *
     * \return The type that is requested by this Property's Property::set method
     */
    virtual const std::type_info& type() const;

    /**
     * This method encodes the encapsulated value of this Property at the top of the Lua
     * stack. The specific details of this serialization is up to the property developer
     * as long as the rest of the stack is unchanged. The implementation has to be
     * synchronized with the Property::setLuaValue method. The default implementation is a
     * no-op.
     *
     * \param state The Lua state to which the value will be encoded
     * \return `true` if the encoding succeeded, `false` otherwise
     */
    virtual void getLuaValue(lua_State* state) const = 0;

    /**
     * This method sets the value encapsulated by this Property by deserializing the value
     * on top of the passed Lua stack. The specific details of the deserialization are up
     * to the Property developer, but they must only depend on the top element of the
     * stack and must leave all other elements unchanged. The implementation has to be
     * synchronized with the Property::getLuaValue method. The default implementation is a
     * no-op.
     *
     * \param state The Lua state from which the value will be decoded
     */
    virtual void setLuaValue(lua_State* state) = 0;

    /**
     * Returns the Lua type that will be put onto the stack in the Property::getLua method
     * and which will be consumed by the Property::setLuaValue method. The returned value
     * can be a combination of any value contained in the `LuaTypes`.
     *
     * \return The Lua type that will be consumed or produced by the Property::getLuaValue
     *         and Property::setLuaValue methods.
     */
    virtual ghoul::lua::LuaTypes typeLua() const = 0;

    /**
     * This method encodes the encapsulated \p value of this Property as a `std::string`.
     * The specific details of this serialization is up to the property developer. The
     * default implementation is a no-op.
     *
     * \return The string representation of the stored property value
     */
    virtual std::string stringValue() const;

    /**
     * This method registers a \p callback function that will be called every time if
     * either Property:set or Property::setLuaValue was called with a value that is
     * different from the previously stored value. The callback can be removed by calling
     * the removeOnChange method with the OnChangeHandle that was returned here.
     *
     * \param callback The callback function that is called when the encapsulated type has
     *        been successfully changed by either the Property::set or
     *        Property::setLuaValue methods.
     * \return An OnChangeHandle that can be used in subsequent calls to remove a callback
     *
     * \pre The \p callback must not be empty
     */
    OnChangeHandle onChange(std::function<void()> callback);

    /**
     * This method registers a \p callback function that will be called when the property
     * is destructed.
     *
     * \return An OnDeleteHandle that can be used in subsequent calls to remove a callback
     *
     * \pre The \p callback must not be empty
     */
    OnDeleteHandle onDelete(std::function<void()> callback);

    /**
     * This method registers a \p callback function that will be called every time the
     * notifyMetaDataChangeListener function was called. The callback can be removed by
     * calling the removeOnMetaDataChange method with the OnMetaDataChangeHandle that was
     * returned here.
     *
     * \param callback The callback function that is called when the meta data of the
     *        property has been changed.
     * \return An OnMetaDataChangeHandle that can be used in subsequent calls to remove a
     *         callback
     *
     * \pre The \p callback must not be empty
     */
    OnMetaDataChangeHandle onMetaDataChange(std::function<void()> callback);

    /**
     * This method deregisters a callback that was previously registered with the onChange
     * method. If OnChangeHandleAll is passed to this function, all registered callbacks
     * are removed.
     *
     * \param handle An OnChangeHandle that was returned from a previous call to onChange
     *        by this property or OnChangeHandleAll if all callbacks should be removed.
     *
     * \pre \p handle must refer to a callback that has been previously registred
     * \pre \p handle must refer to a callback that has not been removed previously
     */
    void removeOnChange(OnChangeHandle handle);

    /**
     * This method deregisters a callback that was previously registered with the onDelete
     * method.
     *
     * \param handle An OnDeleteHandle that was returned from a previous call to onDelete
     *        by this property.
     *
     * \pre \p handle must refer to a callback that has been previously registred
     * \pre \p handle must refer to a callback that has not been removed previously
     */
    void removeOnDelete(OnDeleteHandle handle);

    /**
     * This method deregisters a callback that was previously registered with the
     * onMetaDataChange method. If OnMetaDataChangeHandleAll is passed to this function,
     * all registered callbacks are removed.
     *
     * \param handle An OnMetaDataChangeHandle that was returned from a previous call to
     * onMetaDataChange by this property or OnMetaDataChangeHandleAll if all callbacks
     * should be removed.
     *
     * \pre \p handle must refer to a callback that has been previously registred
     * \pre \p handle must refer to a callback that has not been removed previously
     */
    void removeOnMetaDataChange(OnMetaDataChangeHandle handle);

    /**
     * This method returns the unique identifier of this Property.
     *
     * \return The unique identifier of this Property
     */
    const std::string& identifier() const;

    /**
     * Returns the URI for this Property that uniquely identifies this Property within
     * OpenSpace. It consists of the identifier preceded by all levels of PropertyOwner%s
     * separated with `.`; for example: `owner1.owner2.identifier`. If the URI is invalid
     * (the Property hasn't been added to the property tree yet), it returns an empty
     * string.
     *
     * \return The fully qualified identifier for this Property
     */
    std::string_view uri() const;

    /**
     * Returns the PropertyOwner of this Property or `nullptr`, if it does not have an
     * owner.
     *
     * \return The PropertyOwner of this Property
     */
    const PropertyOwner* owner() const;

    /**
     * Assigned the Property to a new PropertyOwner. This method does not inform the
     * PropertyOwner of this action.
     *
     * \param owner The new PropertyOwner for this Property
     */
    void setPropertyOwner(PropertyOwner* owner);

    /**
     * Returns the human-readable GUI name for this Property that has been set in the
     * constructor. This method returns the same value as accessing the metaData object
     * and requesting the `std::string` stored for the `guiName` key.
     *
     * \return The human-readable GUI name for this Property
     */
    const std::string& guiName() const;

    /**
     * This function returns a user-facing description of the Property which can be
     * displayed in the user interface to inform the user what this Property does and how
     * it affects the rendering.
     *
     * \return The description of this Property
     */
    const std::string& description() const;

    /**
     * Sets the identifier of the group that this Property belongs to. Property groups can
     * be used, for example, by GUI application to visually group different properties,
     * but it has no impact on the Property itself. The default value for the \p groupID
     * is `""`.
     *
     * \param groupId The group id that this property should belong to
     */
    void setGroupIdentifier(std::string groupId);

    /**
     * Returns the group idenfier that this Property belongs to, or `""` if it belongs to
     * no group.
     *
     * \return The group identifier that this Property belongs to
     */
    std::string groupIdentifier() const;

    /**
     * Sets a hint about the visibility of the Property. Each application accessing the
     * properties is free to ignore this hint.
     *
     * \param visibility The new visibility of the Property
     */
    void setVisibility(Visibility visibility);

    /**
     * Returns this Property%'s visibility setting.
     *
     * \return This Property%'s visibility setting
     */
    Visibility visibility() const;

    /**
     * This method determines if this Property should be read-only in external
     * applications. This setting is only a hint and does not need to be followed by GUI
     * applications and does not have any effect on the Property::set or
     * Property::setLuaValue methods. The default value is `false`.
     *
     * \param state `true` if the Property should be read only, `false` otherwise
     */
    void setReadOnly(bool state);

    /**
     * Returns whether this property is read-only. This setting is only a hint and does
     * not need to be followed by GUI applications and does not have any effect on the
     * Property::set or Property::setLuaValue methods.
     */
    bool isReadOnly() const;

    /**
     * This method determines if this Property requires confirmation upon every change of
     * the value. This setting is only a hint and does not need to be followed by GUI
     * applications and does not have any effect on the Property::set or
     * Property::setLuaValue methods.
     *
     * \param needsConfirmation `true` if confirmation dialogs should be shown, `false`
     *        otherwise.
     */
    void setNeedsConfirmation(bool needsConfirmation);

    /**
     * Default view options that can be used in the Property::setViewOption method. The
     * values are:
     *   - Property::ViewOptions::Color = `Color` (Intended for Vec3 and Vec4),
     *   - Property::ViewOptions::MinMaxRange = `MinMaxRange` (Intended for Vec2)
     */
    struct ViewOptions {
        static const char* Color;
        static const char* MinMaxRange;
    };

    /**
     * This method allows the developer to give hints to the GUI about different
     * representations for the GUI. The same Property (for example Vec4Property) can be
     * used in different ways, each requiring a different input method. These values are
     * stored in the metaData object under `ViewOptions`. See Property::ViewOptions for a
     * default list of possible options. As these are only hints, the GUI is free to
     * ignore any suggestion by the developer.
     *
     * \param option The view option that should be modified
     * \param value Determines if the view option should be active (`true`) or
     *        deactivated (`false`)
     */
    void setViewOption(std::string option, bool value = true);

    /**
     * This method returns the state of a \p option hint. See Property::ViewOptions for a
     * default list of possible options. As these are only hints, the GUI is free to
     * ignore any suggestion by the developer.
     *
     * \param option The view option that should be retrieved
     * \param defaultValue The value that is returned if the \p option was not set
     * \return The view option's value
     */
    bool viewOption(const std::string& option, bool defaultValue = false) const;

    /**
     * Get a valid JSON formatted representation of the Property's value.
     *
     * \return The value in a JSON compatible format
     */
    virtual std::string jsonValue() const;

    /// Interpolation methods
    virtual void setLuaInterpolationTarget(lua_State* state);

    virtual void interpolateValue(float t,
        ghoul::EasingFunc<float> easingFunction = nullptr);

    /**
     * Creates the information that is general to every Property and adds the
     * `description`, `guiName`, `group`, `isReadOnly`, `needsConfirmation` `type`,
     * and `visibility` keys and their values.
     *
     * \return The base description common to all Property classes
     */
    nlohmann::json generateJsonDescription() const;

    /**
     * Creates the information that is specific to each subclass of Property%s. If a
     * subclass needs to add additional information into the description, it has to
     * override this method and return the string containing all of the additional
     * information.
     *
     * \return The information specific to each subclass of Property
     */
    virtual nlohmann::json generateAdditionalJsonDescription() const;

    /**
     * Returns whether or not the property value has changed.
     *
     * \return true if the property has changed
     */
    bool hasChanged() const;

    /**
     * Reset the valChanged flag to an unchanged state, as if value has not been changed.
     */
    void resetToUnchanged();

    /**
     * This function must be called whenever this property's URI changes. Examples of this
     * are if the PropertyOwner to which this Property belongs changes identifier or is
     * reparented in any way.
     */
    void updateUriCache();

protected:
    /**
     * This method must be called by all subclasses whenever the encapsulated value has
     * changed and potential listeners need to be informed.
     */
    void notifyChangeListeners();

    /**
     * This method must be called by all subclasses whenever the meta data has
     * changed and potential listeners need to be informed.
     */
    void notifyMetaDataChangeListeners();

    /// The PropetyOwner this Property belongs to, or `nullptr`
    PropertyOwner* _owner = nullptr;

    /// The identifier for this Property
    std::string _identifier;

    /// The GUI user-facing name of this Property
    std::string _guiName;

    /// The user-facing description of this Property
    std::string _description;

    /// The meta data necessary for external applications
    struct {
        std::optional<std::string> group;
        Visibility visibility = Visibility::Default;
        std::optional<bool> readOnly;
        bool needsConfirmation;
        std::unordered_map<std::string, bool> viewOptions;
    } _metaData;

    /// The callback functions that will be invoked whenever the value changes
    std::vector<std::pair<OnChangeHandle, std::function<void()>>> _onChangeCallbacks;

    /// The callback functions that will be invoked whenever the value changes
    std::vector<std::pair<OnDeleteHandle, std::function<void()>>> _onDeleteCallbacks;

    /// A cached version of the full URI of this property, which includes the identifiers
    /// of all owners
    std::string _uriCache;
    bool _isUriCacheDirty = true;

    /// The callback functions that will be invoked whenever the meta data changes
    std::vector<std::pair<OnMetaDataChangeHandle, std::function<void()>>>
        _onMetaDataChangeCallbacks;

    /// Flag indicating that this property value has been changed after initialization
    bool _isValueDirty = false;

private:
    OnChangeHandle _currentHandleValue = 0;

#ifdef _DEBUG
    // These identifiers can be used for debugging. Each Property is assigned one unique
    // identifier.
    static uint64_t Identifier;
    uint64_t _id;
#endif
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___PROPERTY___H__
