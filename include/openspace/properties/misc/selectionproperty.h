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

#ifndef __OPENSPACE_CORE___SELECTIONPROPERTY___H__
#define __OPENSPACE_CORE___SELECTIONPROPERTY___H__

#include <openspace/properties/templateproperty.h>

#include <set>
#include <vector>

namespace openspace::properties {

class SelectionProperty : public TemplateProperty<std::set<std::string>> {

public:
    SelectionProperty(Property::PropertyInfo info);

    std::string_view className() const override final;
    ghoul::lua::LuaTypes typeLua() const override final;

    /**
     * This method sets the stored value to the provided value `val`. If the value is
     * different, the listeners are notified. It also removes any invalid keys in the
     * input set. A key is invalid if it does not correspond to an existing option in the
     * SelectionProperty.
     *
     * \param val The new value for this SelectionProperty
     */
    void setValue(std::set<std::string> val) override final;

    /**
     * Checks if an option given by the provided `key` exists.
     *
     * \param key The key that should be checked for existence
     * \return `true` if the option exists; `false` otherwise
     */
    bool hasOption(const std::string& key) const;

    /**
     * Checks if an option given by the provided `key` is selected.
     *
     * \param key The key that should be checked
     * \return `true` if the option is selected; `false` otherwise
     */
    bool isSelected(const std::string& key) const;

    /**
     * Checks if the SelectionProperty has any selected values, that is, if its value is
     * empty.
     *
     * \return `true` if there are selected options; `false` otherwise
     */
    bool hasSelected() const;

    /**
     * Returns all available options for this SelectionProperty. Should be sorted
     * alphabetically.
     *
     * \return A list of all available options
     */
    const std::vector<std::string>& options() const;

    /**
     * This method sets all available options at once, removing any potential duplicates.
     * If a selection exists, it is updated to that any selected option that does not
     * match the new set of options is removed.
     *
     * \param keys The keys for the options to be set
     */
    void setOptions(const std::vector<std::string>& keys);

    /**
     * Adds an individual option, if it did not already exist.
     *
     * \param key The key for the option to be added
     */
    void addOption(const std::string& key);

    /**
     * This method clears the selection list, that is the value of this SelectionProperty.
     */
    void clearSelection();

    /**
     * This method clears the options list. As the selection must match the available
     * options, the selection list is cleared as well.
     */
    void clearOptions();

    void getLuaValue(lua_State* state) const override final;

    std::string stringValue() const override;
    using TemplateProperty<std::set<std::string>>::operator std::set<std::string>;
    using TemplateProperty<std::set<std::string>>::operator=;

private:
    void sortOptions();
    bool removeInvalidKeys(std::set<std::string>& keys) const;

    nlohmann::json generateAdditionalJsonDescription() const override final;

private:
    std::set<std::string> toValue(lua_State* state) const override final;

    // A list of all available options that can be selected
    std::vector<std::string> _options;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___SELECTIONPROPERTY___H__
