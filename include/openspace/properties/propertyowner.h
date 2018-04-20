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

#ifndef __OPENSPACE_CORE___PROPERTYOWNER___H__
#define __OPENSPACE_CORE___PROPERTYOWNER___H__

#include <openspace/documentation/documentationgenerator.h>

#include <map>
#include <string>
#include <vector>

namespace openspace::properties {

class Property;

/**
 * A PropertyOwner can own Propertys or other PropertyOwner and provide access to both in
 * a unified way. The <code>identifier</code>s and <code>name</code>s of Propertys and
 * sub-owners must be unique to this PropertyOwner. A Property cannot have the same name
 * as a PropertyOwner owned by this PropertyOwner.
 * Propertys can be added using the Property::addProperty methods and be removed by the
 * Property::removeProperty method. The same holds true for sub-owners
 * (Property::addPropertySubOwner, Property::removePropertySubOwner). These methods will
 * inform the passed object about the new ownership automatically.
 * Stored properties can be accessed using the Property::properties method or the
 * Property::property method, providing an URI for the location of the property. If the
 * URI contains separators (<code>.</code>), the first name before the separator will be
 * used as a subOwner's name and the search will proceed recursively.
 */
class PropertyOwner : public DocumentationGenerator {
public:
    /// The separator that is used while accessing the properties and/or sub-owners
    static const char URISeparator = '.';

    struct PropertyOwnerInfo {
        std::string identifier;
        std::string guiName = "";
        std::string description = "";
    };

    /**
     * The constructor of PropertyOwner.
     *
     * \param info the PropertyOwnerInfo struct that contains the
     * #PropertyOwnerInfo::identifier, #PropertyOwnerInfo::guiName, and
     * #PropertyOwnerInfo::description of this PropertyOwner.
     *
     * \pre The \p info 's #PropertyOwnerInfo::identifier must not contain any whitespaces
     * \pre The \p info 's #PropertyOwnerInfo::identifier must not contain any
     *      <code>.</code>
     */

    PropertyOwner(PropertyOwnerInfo info);

    /**
     * The destructor will remove all Propertys and PropertyOwners it owns along with
     * itself.
     */
    virtual ~PropertyOwner();

    /**
     * Sets the identifier for this PropertyOwner. If the PropertyOwner does not have an
     * owner itself, the identifier must be globally unique. If the PropertyOwner has an
     * owner, the identifier must be unique to the owner (including the owner's
     * properties). No uniqueness check will be preformed here, but rather in the
     * PropertyOwner::addProperty and PropertyOwner::addPropertySubOwner methods).
     *
     * \param identifier The identifier of this PropertyOwner. It must not contain any
     *        <code>.</code>s or whitespaces
     *
     * \pre \p identifier must not contain any whitespaces
     * \pre \p identifier must not contain any <code>.</code>
     */
    void setIdentifier(std::string identifier);

    /**
     * Returns the identifier of this PropertyOwner.
     * \return The identifier of this PropertyOwner
     */
    std::string identifier() const;

    /**
     * Sets the user-facing name of this PropertyOwner. This name does not have to be
     * unique, but it is recommended to be.
     *
     * \param guiName The new user-facing name for this PropertyOwner
     */
    void setGuiName(std::string guiName);

    /**
     * Returns the current user-facing name for this PropertyOwner.
     *
     * \return The current user-facing name for this PropertyOwner
     */
    const std::string& guiName() const;

    void setDescription(std::string description);

    std::string description() const;

    /**
     * Returns a list of all Propertys directly owned by this PropertyOwner. This list not
     * include Propertys owned by other sub-owners.
     *
     * \return A list of all Propertys directly owned by this PropertyOwner
     */
    std::vector<Property*> properties() const;

    /**
     * Returns a list of all Propertys directly or indirectly owned by this PropertyOwner.
     *
     * \return A list of all Propertys directly or indirectly owned by this PropertyOwner
     */
    std::vector<Property*> propertiesRecursive() const;

    /**
     * Retrieves a Property identified by <code>URI</code> from this PropertyOwner. If
     * <code>id</code> does not contain a <code>.</code> the identifier must refer to a
     * Property directly owned by this PropertyOwner. If the identifier contains one or
     * more <code>.</code>, the first part of the name will be recursively extracted and
     * used as a name for a sub-owner and only the last part of the identifier is
     * referring to a Property owned by PropertyOwner named by the second-but-last name.
     * \param URI The identifier of the Property that should be extracted
     * \return If the Property cannot be found, <code>nullptr</code> is returned,
     * otherwise the pointer to the Property is returned
     */
    Property* property(const std::string& URI) const;

    /**
     * This method checks if a Property with the provided <code>URI</code> exists in this
     * PropertyOwner (or any sub-owner). If the identifier contains one or more
     * <code>.</code>, the first part of the name will be recursively extracted and is
     * used as a name for a sub-owner and only the last part of the identifier is
     * referring to a Property owned by PropertyOwner named by the second-but-last name.
     * \return <code>true</code> if the <code>URI</code> refers to a Property;
     * <code>false</code> otherwise.
     */
    bool hasProperty(const std::string& URI) const;

    void setPropertyOwner(PropertyOwner* owner) { _owner = owner; }
    PropertyOwner* owner() const { return _owner; }

    /**
     * Returns a list of all sub-owners this PropertyOwner has. Each name of a sub-owner
     * has to be unique with respect to other sub-owners as well as Property's owned by
     * this PropertyOwner.
     * \return A list of all sub-owners this PropertyOwner has
     */
    std::vector<PropertyOwner*> propertySubOwners() const;

    /**
     * This method returns the direct sub-owner of this PropertyOwner with the provided
     * <code>name</code>. This means that <code>name</code> cannot contain any
     * <code>.</code> as this character is not allowed in PropertyOwner names. If the
     * <code>name</code> does not name a valid sub-owner of this PropertyOwner, a
     * <code>nullptr</code> will be returned.
     * \param name The name of the sub-owner that should be returned
     * \return The PropertyOwner with the given <code>name</code>, or <code>nullptr</code>
     */
    PropertyOwner* propertySubOwner(const std::string& name) const;

    /**
     * Returns <code>true</code> if this PropertyOwner owns a sub-owner with the provided
     * <code>name</code>; returns <code>false</code> otherwise.
     * \param name The name of the sub-owner that should be looked up
     * \return <code>true</code> if this PropertyOwner owns a sub-owner with the provided
     * <code>name</code>; returns <code>false</code> otherwise
     */
    bool hasPropertySubOwner(const std::string& name) const;

    /**
     * This method converts a provided <code>groupID</code>, used by the Propertys, into a
     * human-readable <code>name</code> which can be used by some external application.
     * \param groupID The group identifier whose human-readable name should be set
     * \param name The human-readable name for the group identifier
     */
    void setPropertyGroupName(std::string groupID, std::string name);

    /**
     * Returns the human-readable name for the <code>groupID</code> for the Propertys of
     * this PropertyOwner.
     * \param groupID The group identifier whose human-readable name should be returned
     * \return The human readable name for the Propertys identified by
     * <code>groupID</code>
     */
    std::string propertyGroupName(const std::string& groupID) const;

    /**
     * Assigns the Property <code>prop</code> to this PropertyOwner. This method will
     * check if the name of the Property is unique amongst Propertys and sub-owners in
     * this PropertyOwner. This method will also inform the Property about the change in
     * ownership by calling the Property::setPropertyOwner method.
     * \param prop The Property whose ownership is changed.
     */
    void addProperty(Property* prop);

    /// \see Property::addProperty(Property*)
    void addProperty(Property& prop);

    /**
     * Adds the provided PropertyOwner to the list of sub-owners for this PropertyOwner.
     * This means that the name of the <code>owner</code> has to be unique amonst the
     * direct Property's as well as other PropertyOwner's that this PropertyOwner owns.
     * This uniqueness will be tested in this method.
     * \param owner The owner that should be assigned to this PropertyOwner
     */
    void addPropertySubOwner(PropertyOwner* owner);

    /// \see PropertyOwner::addPropertySubOwner(PropertyOwner*)
    void addPropertySubOwner(PropertyOwner& owner);

    /**
     * Removes the Property from this PropertyOwner. Notifies the Property about this
     * change by calling the Property::setPropertyOwner method with a <code>nullptr</code>
     * as parameter.
     * \param prop The Property that should be removed from this PropertyOwner
     */
    void removeProperty(Property* prop);

    /// \see PropertyOwner::removeProperty(Property*)
    void removeProperty(Property& prop);

    /**
     * Removes the sub-owner from this PropertyOwner.
     * \param owner The PropertyOwner that should be removed
     */
    void removePropertySubOwner(PropertyOwner* owner);

    /// \see PropertyOwner::removePropertySubOwner(PropertyOwner*)
    void removePropertySubOwner(PropertyOwner& owner);

    /**
     * Returns a list of all tags that have been assigned to the Property. Useful for
     * trying to find a match for a desired batch operation on Properties.
     * \return Pointer to vector of string tags that were assigned to the Property
     */
    std::vector<std::string> tags() const;

    /**
     * Adds a tag to the PropertyOwner's list of assigned tags. Tags are useful for
     * creating oups of Properties that can be used in batch operations or to mark up
     * PropertyOwners for other usages (such signalling to GUI applications).
     * \param tag The string that is to be assigned to the Property
     */
    void addTag(std::string tag);

    /**
     * Removes a tag from this PropertyOwner. No error is reported if the tag does not
     * exist
     * @param tag The tag is that is to be removed from this PropertyOwner
     */
    void removeTag(const std::string& tag);


protected:
    /// The unique identifier of this PropertyOwner
    std::string _identifier;
    /// The user-facing GUI name for this PropertyOwner
    std::string _guiName;
    /// The description for this PropertyOwner
    std::string _description;

private:
    std::string generateJson() const override;

    /// The owner of this PropertyOwner
    PropertyOwner* _owner;
    /// A list of all registered Property's
    std::vector<Property*> _properties;
    /// A list of all sub-owners
    std::vector<PropertyOwner*> _subOwners;
    /// The associations between group identifiers of Property's and human-readable names
    std::map<std::string, std::string> _groupNames;
    /// Collection of string tag(s) assigned to this property
    std::vector<std::string> _tags;
};

}  // namespace openspace::properties

#endif // __OPENSPACE_CORE___PROPERTYOWNER___H__
