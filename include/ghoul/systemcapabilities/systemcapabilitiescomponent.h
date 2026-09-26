/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___SYSTEMCAPABILITIESCOMPONENT___H__
#define __GHOUL___SYSTEMCAPABILITIESCOMPONENT___H__

#include <ghoul/misc/assert.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/exception.h>
#include <string>
#include <string_view>
#include <vector>

// X11 #defines 'None' in its X.h header file. We don't want it here
#undef None

#ifdef WIN32
struct IWbemLocator;
struct IWbemServices;
#endif // WIN32

namespace ghoul::systemcapabilities {

/**
 * This class is the base class of all components that can detect a specific set of
 * features. Each subclass should focus on a specific module of features (like
 * GeneralCapabilitiesComponent or OpenGLCapabilitiesComponent). The action flow for a
 * SystemCapabilitiesComponent subclass is as follows: It gets created with the
 * constructor and should provide a descriptive name to this SystemCapabilitiesComponent's
 * constructor, then, the #initialize function will be called, #detectCapabilities follows
 * and finally a call to #capabilities, which should return an `std::vector` of
 * `std::pair`'s with `{`description `,`value `}`. Each SystemCapabilitiesComponent%'s
 * #deinitialize function might be called with a call to #initialize directly following
 * and it should rescan all the values and capabilities on the following #initialize
 * function again to check for changes.
 */
class SystemCapabilitiesComponent {
public:
    BooleanType(InitializeWMI);

    /**
     * The verbosity that is used in the #capabilities method.
     */
    enum class Verbosity {
        /// No verbosity at all, discarding all information strings
        None = 0,
        /// The minimal verbosity presenting the absolute minimum information
        Minimal,
        /// The default verbosity
        Default,
        /// Show all the available information
        Full
    };

    /**
     * Each piece of capability information is represented by an information and a value.
     */
    struct CapabilityInformation {
        /// The user-readable description of this capability
        std::string description;

        /// The value of the capability
        std::string value;

        /// The SystemCapabilitiesComponent%'s recommendation for Verbosity
        Verbosity verbosity;
    };

    /**
     * The base constructor will initialize the Windows Management Instrumentation
     * (if the application is running on Windows). If the application is compiled not on
     * Windows, no error will occur. If WMI should be used and an error occurs, an error
     * will be logged. This method needs to be called from each derived subclass, even if
     * WMI is not used.
     *
     * \param initializeWMI If this parameter is `true`, the Windows Management
     *        Instrumentation will be initialized
     */
    SystemCapabilitiesComponent(InitializeWMI initializeWMI = InitializeWMI::Yes);

    /**
     * The virtual destructor that will deinitialize all necessary values.
     */
    virtual ~SystemCapabilitiesComponent();

    /**
     * This method will need to detect all the capabilities or values the subclass is
     * responsible for. It is useful for the subclass to cache the values in member
     * variables and make them available though accessor functions as well as subsequent
     * calls to #capabilities.
     */
    virtual void detectCapabilities() = 0;

    /**
     * This method will clear all the capabilities of the subclass. Sensible default
     * values should be used that indicate clearly that it is an invalid value.
     */
    virtual void clearCapabilities() = 0;

    /**
     * This method returns pairs CapabilityInformation%s, describing the features and
     * capabilities the subclass is responsible for. As a best-practice, the subclass
     * should allow individual query for each of the elements as well.
     *
     * \return All CapabilityInformation%s that this SystemCapabilitiesComponent is
     *         responsible for
     */
    virtual std::vector<CapabilityInformation> capabilities() const = 0;

    /**
     * The implementation in the subclass should returns a descriptive name of the
     * component.
     *
     * \return A descriptive name of the component
     */
    virtual std::string_view name() const = 0;

protected:
#ifdef WIN32
    /**
     * This method initializes the Windows Management Instrumentation.
     *
     * \throw WMIError If there was an error initializing the Windows Management
     *        Instrumentation
     * \pre The Windows Management Instrumentation must not have been initialized before
     */
    static void initializeWMI();

    /**
     * This method deinitializes the Windows Management Instrumentation.
     *
     * \pre The Windows Management Instrumentation must have been initialized before
     */
    static void deinitializeWMI();

    /**
     * Returns if the Windows Management Instrumentation has been initialized before. If
     * the WMI is not enabled, or the application is compiled on a non-Windows
     * architecture, this method is a no-op.
     *
     * \return `true` if the WMI has been initialized
     */
    static bool isWMIInitialized();

    /**
     * Helper function that queries the Windows Management Instrumentation for the
     * \p attribute within the \p wmiClass and it expects the attribute to be of type
     * `std::string`.
     *
     * \param wmiClass The class in which the required attribute resides
     * \param attribute The attribute which we are interested in
     * \param value A reference to the value, where the attribute will be stored in (if
     *        the attribute could be found and it is of type `string`)
     *
     * \throw WMIError If there was an error querying the Windows Management
     *        Instrumentation service or there was no query result available
     * \pre \p wmiClass must not be empty
     * \pre \p attribute must not be empty
     * \pre The Windows Management Instrumentation must have been initialized
     */
    void queryWMI(const std::string& wmiClass, const std::string& attribute,
        std::string& value);

    /**
     * Helper function that queries the Windows Management Instrumentation for the
     * \p attribute within the \p wmiClass and it expects the attribute to be of type
     * `int`.
     *
     * \param wmiClass The class in which the required attribute resides
     * \param attribute The attribute which we are interested in
     * \param value A reference to the value, where the attribute will be stored in (if
     *        the attribute could be found and it is of type `int`)
     *
     * \throw WMIError If there was an error querying the Windows Management
     *        Instrumentation service or there was no query result available
     * \pre \p wmiClass must not be empty
     * \pre \p attribute must not be empty
     * \pre The Windows Management Instrumentation must have been initialized
     */
    void queryWMI(const std::string& wmiClass, const std::string& attribute,
        int& value);

    /**
     * Helper function that queries the Windows Management Instrumentation for the
     * \p attribute within the \p wmiClass and it expects the attribute to be of type
     * `unsigned int`.
     *
     * \param wmiClass The class in which the required attribute resides
     * \param attribute The attribute which we are interested in
     * \param value A reference to the value, where the attribute will be stored in (if
     *        the attribute could be found and it is of type `unsigned int`)
     *
     * \throw WMIError If there was an error querying the Windows Management
     *        Instrumentation service or there was no query result available
     * \pre \p wmiClass must not be empty
     * \pre \p attribute must not be empty
     * \pre The Windows Management Instrumentation must have been initialized
     */
    void queryWMI(const std::string& wmiClass, const std::string& attribute,
        unsigned int& value);

    /**
     * Helper function that queries the Windows Management Instrumentation for the
     * \p attribute within the \p wmiClass and it expects the attribute to be of type
     * `unsigned long long`.
     *
     * \param wmiClass The class in which the required attribute resides
     * \param attribute The attribute which we are interested in
     * \param value A reference to the value, where the attribute will be stored in (if
     *        the attribute could be found and it is of type `unsigned long long`)
     *
     * \throw WMIError If there was an error querying the Windows Management
     *        Instrumentation service or there was no query result available
     * \pre \p wmiClass must not be empty
     * \pre \p attribute must not be empty
     * \pre The Windows Management Instrumentation must have been initialized
     */
    void queryWMI(const std::string& wmiClass, const std::string& attribute,
        unsigned long long& value);

    /**
     * The locator object that was used in the `CoCreateInstance` call in the
     * #initializeWMI call.
     */
    static IWbemLocator* _iwbemLocator;

    /**
     * The service object that was used in the `_iwbemLocator->ConnectServer`
     * call in the #initializeWMI call.
     */
    static IWbemServices* _iwbemServices;
#endif // WIN32
};

} // namespace ghoul::systemcapabilities

namespace ghoul {
    template <>
    inline std::string to_string(
                      const systemcapabilities::SystemCapabilitiesComponent::Verbosity& v)
    {
        using Verbosity = systemcapabilities::SystemCapabilitiesComponent::Verbosity;
        switch (v) {
            case Verbosity::None:    return "None";
            case Verbosity::Minimal: return "Minimal";
            case Verbosity::Default: return "Default";
            case Verbosity::Full:    return "Full";
            default:                 throw MissingCaseException();
        }
    }

    template <>
    constexpr systemcapabilities::SystemCapabilitiesComponent::Verbosity from_string(
                                                                 std::string_view string)
    {
        using Verbosity = systemcapabilities::SystemCapabilitiesComponent::Verbosity;

        if (string == "None") { return Verbosity::None; }
        else if (string == "Minimal") { return Verbosity::Minimal; }
        else if (string == "Default") { return Verbosity::Default; }
        else if (string == "Full") { return Verbosity::Full; }
        throw RuntimeError(std::format("Unknown verbosity '{}'", string));
    }
} // namespace ghoul

#endif // __GHOUL___SYSTEMCAPABILITIESCOMPONENT___H__
