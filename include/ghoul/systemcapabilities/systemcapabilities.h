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

#ifndef __GHOUL___SYSTEMCAPABILITIES___H__
#define __GHOUL___SYSTEMCAPABILITIES___H__

#include <ghoul/systemcapabilities/systemcapabilitiescomponent.h>

#include <ghoul/misc/exception.h>
#include <memory>
#include <vector>

namespace ghoul::systemcapabilities {

/**
 * The SystemCapabilities class allows access to the functionality the system provides.
 * The class is composed of SystemCapabilitiesComponent%s with each component checking for
 * a specific kind of capabilities (for example OpenGLCapabilitiesComponent or
 * GeneralCapabilitiesComponent) and provides access to the components with the templated
 * #component method. The values are not guaranteed to be constant over the lifetime of
 * the application, but most values can be considered to be static and cache-able. New
 * components can be added using the #addComponent and retrieved using the #component
 * method. Each type of component can only be added once to the SystemCapabilities. The
 * second insertion will log a warning if `GHL_DEBUG` is defined.
 */
class SystemCapabilities {
public:
    /**
     * The error that is returned when a requested SystemCapabilitiesComponent could not
     * be found.
     */
    struct CapabilitiesComponentNotFoundError : public RuntimeError {
        explicit CapabilitiesComponentNotFoundError();
    };

    /**
     * Returns a reference to the global SystemCapabilities object.
     *
     * \return A reference to the global SystemCapabilities object
     *
     * \pre The static SystemCapabilities must have been initialized
     */
    static SystemCapabilities& ref();

    /**
     * Returns the initialization state of the SystemCapabilities object.
     *
     * \return The initialization state of the SystemCapabilities object
     */
    static bool isInitialized();

    /**
     * Calling this method will trigger the
     * #ghoul::systemcapabilities::SystemCapabilitiesComponent::detectCapabilities of all
     * registered components (#addComponent). If the capabilities have been detected
     * previously and a new component is added, a following call will redetect the
     * capabilities of all components, thus, multiple calls to this function will perform
     * a full redetection.
     */
    void detectCapabilities();

    /**
     * Logs all of the detected capabilities of the log, group by the individual
     * SystemCapabilitiesComponent%s. The verbosity of the log is controlled by the
     * \p verbosity. This method will, in turn, call the
     * #ghoul::systemcapabilities::SystemCapabilitiesComponent::capabilities of all the
     * registered SystemCapabilitiesComponent%s.
     *
     * \param verbosity The verbosity of the resulting log entries
     */
    void logCapabilities(SystemCapabilitiesComponent::Verbosity verbosity =
        SystemCapabilitiesComponent::Verbosity::Default) const;

    /**
     * Adds the passed `component` to this SystemCapabilities and assumes
     * ownership of this object. This method will not automatically detect the
     * capabilities in this component; this has to be done using the
     * #ghoul::systemcapabilities::SystemCapabilitiesComponent::detectCapabilities
     * method. A specific subclass of SystemCapabilitiesComponent can only be added once
     * to the SystemCapabilities.
     *
     * \param component The component that will be added to this SystemCapabilities
     *        object
     *
     * \pre \p component must not be `nullptr`
     * \pre \p A component of the same type must not have been added before
     */
    void addComponent(std::unique_ptr<SystemCapabilitiesComponent> component);

    /**
     * Returns the component of type `T`.
     *
     * \tparam T The subclass of SystemCapabilitiesComponent that should be retrieved
     * \return The SystemCapabilitiesComponent that should be retrieved or `nullptr` if no
     *         such type exists
     *
     * \throw CapabilitiesComponentNotFoundError If no component of type T could be found
     */
    template <typename T>
    T& component();

private:
    /**
     * Clears the capabilities of all components.
     */
    void clearCapabilities();

    /// The list of all components of this SystemCapabilities
    std::vector<std::unique_ptr<SystemCapabilitiesComponent>> _components;

    /// The static singleton member
    static SystemCapabilities _systemCapabilities;
    static bool _isInitialized;
};

} // namespace ghoul::systemcapabilities

#define SysCap (ghoul::systemcapabilities::SystemCapabilities::ref())

#include "systemcapabilities.inl"

#endif // __GHOUL___SYSTEMCAPABILITIES___H__
