/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __OPTIONPROPERTY_H__
#define __OPTIONPROPERTY_H__

#include <openspace/properties/scalarproperty.h>

#include <vector>

namespace openspace {
namespace properties {

/**
 * The OptionProperty is a property that provides a number of predefined (using the 
 * addOption method) options consisting of a <code>description</code> and a
 * <code>value</code>. The available options can be queried using the options method.
 * Only values representing valid options can be used to set this property, or an error
 * will be logged
 */
class OptionProperty : public IntProperty {
public:
    /**
     * The struct storing a single option consisting of an integer <code>value</code> and
     * a <code>string</code> description.
     */
    struct Option {
        int value;
        std::string description;
    };

    /**
     * The constructor delegating the <code>identifier</code> and the <code>guiName</code>
     * to its super class.
     * \param identifier A unique identifier for this property
     * \param guiName The GUI name that should be used to represent this property
     */
    OptionProperty(std::string identifier, std::string guiName);

    /**
     * Returns the name of the class for reflection purposes.
     * \return The name of this class for reflection purposes
     */
    std::string className() const override;
    using IntProperty::operator=;

    /**
     * Adds the passed option to the list of available options. The <code>value</code> of
     * the <code>option</code> must not have been registered previously, or a warning will
     * be logged.
     * \param value The option that will be added to the list of available options
     * \param desc The description of the value that will be added
     */
    void addOption(int value, std::string desc);

    /**
    * Appends options with vectors of values and descriptions
    * \param values A std::vector<int> of values for the options
    * \param descs A std::vector<string> of descriptions for each value
    */
    void addOptions(std::vector<int> values, std::vector<std::string> descs);

    /**
     * Returns the list of available options.
     * /return The list of available options
     */
    const std::vector<Option>& options() const;

    /**
     * The overritten TemplateProperty::setValue method that checks if the provided value
     * represents a valid Option
     * \param value The value of the Option that should be set
     */
    void setValue(int value) override;

private:
    static const std::string OptionsKey;
    std::string generateAdditionalDescription() const;

    /// The list of options which have been registered with this OptionProperty
    std::vector<Option> _options;
};

} // namespace properties
} // namespace openspace

#endif // __STRINGPROPERTY_H__