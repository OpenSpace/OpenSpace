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
#include <concepts>
#include <string_view>

namespace openspace {

BooleanType(IsEnablable);
BooleanType(IsEnabled);
BooleanType(NeedsConfirmation);
BooleanType(IsReadOnly);

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
    Always = 0
};

/**
 * The default values for properties that do not have default values of their own.
 */
struct BasicDefaults {
    /// Whether this property by default is enabled. This value is only used if the
    /// property's `isEnablable` value is set to `Yes`
    IsEnabled isEnabled = IsEnabled::No;
};

/**
 * The default values for properties that store a single value of type `T`, like
 * std::string.
 */
template <typename T>
struct ValueDefaults {
    /// Whether this property by default is enabled. This value is only used if the
    /// property's `isEnablable` value is set to `Yes`
    IsEnabled isEnabled = IsEnabled::No;

    /// The default value the property should after after creation
    T value = T();
};

/**
 * Provides the minimum value, maximum value, and step size that a numerical property of
 * type `T` uses when they are not specified in its NumericalDefaults. Every numerical
 * property type specializes this struct next to the class definition of the property.
 */
template <typename T>
struct NumericalLimits;

/**
 * The default values for properties that store a numerical value of type `T`, such as
 * the float or glm::vec3.
 */
template <typename T>
struct NumericalDefaults {
    /// Whether this property by default is enabled. This value is only used if the
    /// property's `isEnablable` value is set to `Yes`
    IsEnabled isEnabled = IsEnabled::No;

    /// The value that the property has after it was created
    T value = T(0);

    /// The smallest and largest value that the property can have
    struct {
        T min = NumericalLimits<T>::Min;
        T max = NumericalLimits<T>::Max;
    } minMax;

    /// The step size that user interfaces use when changing the value of the property
    T stepSize = NumericalLimits<T>::Step;

    /// The exponent that user interfaces use for sliders of the property. An exponent of
    /// 1.f results in a linear slider
    float exponent = 1.f;
};

/**
 * This structure is passed to the constructor of a Property and contains the unique
 * identifier, a GUI name and descriptive text that are both user facing. The `Defaults`
 * type contains the default values of the property and depends on the type of the
 * property. Each property type provides an alias for its matching instantiation, for
 * example FloatPropertyInfo or BoolPropertyInfo, which should be used instead of this
 * struct directly. Properties that do not have any default values use PropertyInfo.
 */
template <typename Defaults = BasicDefaults>
struct BasicPropertyInfo {
    /// The unique identifier that is part of the fully qualified URI of this Property
    std::string_view identifier;

    /// The name that is displayed in the user interface
    std::string_view name;

    /// The user facing description of this Property
    std::string_view description;

    /// Determines the visibility of this Property in the user interface
    Visibility visibility = Visibility::Always;

    /// Determines if the Property is enablable (linked to a boolean)
    IsEnablable isEnablable = IsEnablable::No;

    /// Determines if the Property require confirmation upon value change
    NeedsConfirmation needsConfirmation = NeedsConfirmation::No;

    /// Determines if the Property is readonly
    IsReadOnly isReadOnly = IsReadOnly::No;

    /// The default values of the Property. The exact shape of this struct depends on the
    /// specific property implementation
    Defaults defaults = {};

    /**
     * Returns a copy of this BasicPropertyInfo without the type-specific default values.
     * This is used to pass the information to the Property base class, which does not
     * know about the type of the value.
     */
    constexpr operator BasicPropertyInfo<BasicDefaults>() const
        requires (!std::same_as<Defaults, BasicDefaults>)
    {
        return {
            .identifier = identifier,
            .name = name,
            .description = description,
            .visibility = visibility,
            .isEnablable = isEnablable,
            .needsConfirmation = needsConfirmation,
            .isReadOnly = isReadOnly,
            .defaults = { .isEnabled = defaults.isEnabled }
        };
    }
};

using PropertyInfo = BasicPropertyInfo<BasicDefaults>;

} // namespace openspace

#endif // __OPENSPACE_CORE___PROPERTYINFO___H__
