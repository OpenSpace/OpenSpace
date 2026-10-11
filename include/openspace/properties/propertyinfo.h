/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___PROPERTYINFO___H__
#define __OPENSPACE_CORE___PROPERTYINFO___H__

#include <openspace/misc/boolean.h>
#include <string_view>

namespace openspace {

/**
 * This structure is passed to the constructor of a Property and contains the unique
 * identifier, a GUI name and descriptive text that are both user facing.
 */
struct PropertyInfo {
    BooleanType(NeedsConfirmation);
    BooleanType(IsEnablable);
    BooleanType(IsEnabled);

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

    /// The unique identifier that is part of the fully qualified URI of this Property
    std::string_view identifier;
    /// The name that is displayed in the user interface
    std::string_view guiName;
    /// The user facing description of this Property
    std::string_view description;
    /// Determines the visibility of this Property in the user interface
    Visibility visibility = Visibility::Default;
    /// Determines if the Property is enablable (linked to a boolean)
    IsEnablable isEnablable = IsEnablable::No;
    /// Determines if the Property require confirmation upon value change
    NeedsConfirmation needsConfirmation = NeedsConfirmation::No;

    // Default values
    struct {
        // Whether this property by default is enabled. This value is only used if the
        // property's `isEnablable` value is set to `Yes`
        IsEnabled isEnabled = IsEnabled::No;
    } defaults;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PROPERTYINFO___H__
