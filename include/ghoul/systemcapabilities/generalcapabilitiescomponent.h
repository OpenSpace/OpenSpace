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

#ifndef __GHOUL___GENERALCAPABILITIESCOMPONENT___H__
#define __GHOUL___GENERALCAPABILITIESCOMPONENT___H__

#include <ghoul/systemcapabilities/systemcapabilitiescomponent.h>

#include <ghoul/systemcapabilities/systemcapabilities.h>

namespace ghoul::systemcapabilities {

// @TODO: Implement CPU detection
// @TODO: Implement feature detection

/**
 * This subclass of SystemCapabilitiesComponent detects CPU-related capabilities, like CPU
 * information, main memory availability and other local, general hardware features. At
 * the current time, only the Operating System and the main memory are implemented.
 */
class GeneralCapabilitiesComponent : public SystemCapabilitiesComponent {
public:
    /**
     * Main exception that is thrown if an error occured in the detection of general
     * capabilities.
     */
    struct GeneralCapabilitiesComponentError : public RuntimeError {
        explicit GeneralCapabilitiesComponentError(std::string msg);
    };

    /**
     * Exception that is thrown if there was an error detecting the operating system.
     */
    struct OperatingSystemError final : public GeneralCapabilitiesComponentError {
        explicit OperatingSystemError(std::string desc, std::string errorMsg);

        /// The general description of the error
        const std::string description;

        /// Additional information about the error message
        const std::string errorMessage;
    };

    /**
     * Exception that is thrown if there was an error detecting the main memory.
     */
    struct MainMemoryError final : public GeneralCapabilitiesComponentError {
        explicit MainMemoryError(std::string msg);
    };

    /**
     * This enum stores the possible operating systems that can be detected.
     */
    enum class OperatingSystem {
        Windows10or11,
        WindowsServer2016,
        WindowsVista,
        WindowsServer2008,
        Windows7,
        WindowsServer2008R2,
        Windows8,
        WindowsServer2012,
        Windows81,
        WindowsServer2012R2,
        WindowsServer2003R2,
        WindowsStorageServer2003,
        WindowsXPProfx64,
        WindowsServer2003,
        WindowsXPHome,
        WindowsXPProf,
        Windows2000Prof,
        Windows2000DatacenterServer,
        Windows2000AdvancedServer,
        Windows2000Server,
        Linux, // @TODO we need more variety here
        MacOS, // @TODO we need more variety here
        Unknown
    };

    /**
     * Returns the list of all capabilities that were detected by this
     * SystemCapabilitiesComponent.
     *
     * \return The list of all detected capabilities
     */
    std::vector<CapabilityInformation> capabilities() const override;

    /**
     * Returns the operating system version.
     *
     * \return The operating system version
     */
    OperatingSystem operatingSystem() const;

    /**
     * Returns the operating system as a parsed string. It should contain the manufacturer
     * and the version.
     *
     * \return The operating system
     */
    std::string operatingSystemString() const;

    /**
     * Returns the full operating system. The exact format of the returned string is
     * implementation and operating system-dependent but it should contain the
     * manufacturer and the version.
     *
     * \return The operating system as a parsed string
     */
    const std::string& fullOperatingSystem() const;

    /**
     * Returns the amount of available, installed main memory (RAM) on the system in MB.
     * This value is retrieved using the Windows Management Instrumentation (if
     * available). The default value is `-1`.
     *
     * \return The amount of available main memory
     */
    unsigned int installedMainMemory() const;

    /**
     * Returns the number of cores.
     *
     * \return The number of cores
     */
    unsigned int cores() const;

    /**
     * Returns the cache line size.
     *
     * \return The cache line size
     */
    unsigned int cacheLineSize() const;

    /**
     * Returns the L2 associativity.
     *
     * \return The L2 associativity
     */
    unsigned int L2Associativity() const;

    /**
     * Returns the cache size.
     *
     * \return The cache size
     */
    unsigned int cacheSize() const;

    /**
     * Returns all supported exteions as comma separated string.
     *
     * \return The extension
     */
    const std::string& extensions() const;

    /**
     * Returns the `CPU` string.
     *
     * \return The `CPU` string
     */
    std::string_view name() const override;

protected:
    /**
     * Method that detects all of the capabilities.
     *
     * \throw OperatingSystemError If the detection of the operating system failed
     * \throw MainMemoryError If the detection of the main memory failed
     */
    void detectCapabilities() override;
    void clearCapabilities() override;

    /**
     * Detects the operating system.
     *
     * \throw OperatingSystemError If the detection of the operating system failed
     */
    void detectOS();

    /**
     * Detects the amount of the computer's main memory.
     *
     * \throw MainMemoryError If the detection of the main memory failed
     * \throw WMIError If there was an error accessing the Windows Management
     */
    void detectMemory();

    /**
     * Detects detailed information about the CPU on this computer.
     */
    void detectCPU();

    /// Information about the operating system
    OperatingSystem _operatingSystem = OperatingSystem::Unknown;
    std::string _operatingSystemExtra;
    std::string _fullOperatingSystem;

    /// The amount of RAM that is installed
    unsigned int _installedMainMemory = 0;

    /// Information about the CPU
    std::string _cpu;

    /// Number of CPU cores
    unsigned int _cores = 0;

    /// The size of a cache line
    unsigned int _cacheLineSize = 0;

    /// The associativity of the L2 cache
    unsigned int _L2Associativity = 0;

    /// The size of the cache
    unsigned int _cacheSize = 0;

    /// Available CPU extensions
    std::string _extensions;
};

} // namespace ghoul::systemcapabilities

namespace ghoul {

template <>
std::string to_string(
    const systemcapabilities::GeneralCapabilitiesComponent::OperatingSystem& value);
} // namespace ghoul

#define CpuCap (                                                                         \
    ghoul::systemcapabilities::SystemCapabilities::ref().component<                      \
        ghoul::systemcapabilities::GeneralCapabilitiesComponent                          \
    >())

#endif // __GHOUL___GENERALCAPABILITIESCOMPONENT___H__
