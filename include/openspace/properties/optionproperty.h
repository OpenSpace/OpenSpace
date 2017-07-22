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

#ifndef __OPENSPACE_CORE___OPTIONPROPERTY___H__
#define __OPENSPACE_CORE___OPTIONPROPERTY___H__

#include <openspace/properties/scalar/intproperty.h>

#include <vector>

namespace openspace::properties {

/**
 * The OptionProperty is a property that provides a number of predefined (using the 
 * addOption method) options consisting of a <code>description</code> and a
 * <code>value</code>. The available options can be queried using the options method.
 * Only values representing valid options can be used to set this property, or an error
 * will be logged
 */
class OptionProperty : public IntProperty {
public:
    using Property::PropertyInfo;

    /**
     * The struct storing a single option consisting of an integer <code>value</code> and
     * a <code>string</code> description.
     */
    struct Option {
        int value;
        std::string description;
    };

    enum class DisplayType {
        Radio,
        Dropdown
    };

    /**
     * The constructor delegating the <code>identifier</code> and the <code>guiName</code>
     * to its super class.
     * \param info The PropertyInfo structure that contains all the required static
     * information for initializing this Property.
     * \pre \p info.identifier must not be empty
     * \pre \p info.guiName must not be empty
     */
    OptionProperty(PropertyInfo info);

    /**
    * The constructor delegating the <code>identifier</code> and the <code>guiName</code>
    * to its super class.
    * \param info The PropertyInfo structure that contains all the required static
    * information for initializing this Property.
    * \param displayType Optional DisplayType for GUI (default RADIO)
    * \pre \p info.identifier must not be empty
    * \pre \p info.guiName must not be empty
    */
    OptionProperty(PropertyInfo info, DisplayType displayType);

    /**
     * Returns the name of the class for reflection purposes.
     * \return The name of this class for reflection purposes
     */
    std::string className() const override;
    using IntProperty::operator=;

    /**
    * Returns the type for GUI display.
    * \return OptionType for display purposes
    */
    DisplayType displayType() const;

    /**
     * Adds the passed option to the list of available options. The <code>value</code> of
     * the <code>option</code> must not have been registered previously, or a warning will
     * be logged.
     * \param value The option that will be added to the list of available options
     * \param desc The description of the value that will be added
     */
    void addOption(int value, std::string desc);

    /**
    * Adds multiple options to the OptionProperty. Each value in the vector consists of
    * an integer value and a string description.
    * \param options Pairs of <option, description> that are added to the OptionProperty
    */
    void addOptions(std::vector<std::pair<int, std::string>> options);

    /**
     * Returns the list of available options.
     * \return The list of available options
     */
    const std::vector<Option>& options() const;

    /**
     * The overritten TemplateProperty::setValue method that checks if the provided value
     * represents a valid Option
     * \param value The value of the Option that should be set
     */
    void setValue(int value) override;

    /**
    * Get the description of the option that matches <code>value</code>
    * \param value The value of the option 
    */
    std::string getDescriptionByValue(int value);

private:
    static const std::string OptionsKey;
    std::string generateAdditionalDescription() const;

    /// The list of options which have been registered with this OptionProperty
    std::vector<Option> _options;
    DisplayType _displayType;
};

} // namespace openspace::properties

#endif // __OPENSPACE_CORE___OPTIONPROPERTY___H__
