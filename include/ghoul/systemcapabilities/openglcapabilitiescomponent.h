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

#ifndef __GHOUL___OPENGLCAPABILITIESCOMPONENT___H__
#define __GHOUL___OPENGLCAPABILITIESCOMPONENT___H__

#include <ghoul/systemcapabilities/systemcapabilitiescomponent.h>

#include <ghoul/systemcapabilities/systemcapabilities.h>
#include <ghoul/systemcapabilities/version.h>

namespace ghoul::systemcapabilities {

/**
 * This subclass of SystemCapabilitiesComponent detects graphics and OpenGL-related
 * capabilities, like the OpenGL version, supported extensions, or the driver version.
 */
class OpenGLCapabilitiesComponent : public SystemCapabilitiesComponent {
public:
    /**
     * The main exception that is thrown if there is an error detecting an OpenGL
     * capability.
     */
    struct OpenGLCapabilitiesComponentError : public RuntimeError {
        explicit OpenGLCapabilitiesComponentError(std::string msg);
    };

    struct GPUVendorError : public OpenGLCapabilitiesComponentError {
        explicit GPUVendorError(std::string msg);
    };

    /**
     * This enum stores the possible vendors of graphics cards that can be detected.
     */
    enum class Vendor {
        Nvidia, ///< Nvidia
        AmdATI, ///< AMD/ATI
        Intel, ///< Intel
        Other ///< vendor could not be detected
    };

    std::vector<CapabilityInformation> capabilities() const override;

    /**
     * Returns the maximum OpenGL version that is supported on this platform. This means
     * that all the lower version will be supported as well.
     *
     * \return The maximum OpenGL version
     */
    Version openGLVersion() const;

    /**
     * Returns the value of a call to `glGetString(GL_VENDOR)`. This will give
     * detailed information about the vendor of the main graphics card. This string can be
     * used if the automatic Vendor detection failed.
     *
     * \return The value of a call to `glGetString(GL_VENDOR)`
     */
    const std::string& glslCompiler() const;

    /**
     * Returns the vendor of the main graphics card.
     *
     * \return The vendor of the main graphics card
     */
    Vendor gpuVendor() const;

    /**
     * Returns the vendor of the main graphics card converted into a string.
     *
     * \return The vendor of the main graphics card converted into a string
     */
    std::string_view gpuVendorString() const;

    /**
     * Returns all available extensions as a list of names.
     *
     * \return All available extensions as a list of names
     */
    const std::vector<std::string>& extensions() const;

    /**
     * Checks is a specific `extension` is supported or not
     *
     * \return `true` if the `extension` is supported; `false` otherwise
     */
    bool isExtensionSupported(std::string_view extension) const;

    /**
     * Returns the maximum number of texture units that are available on the main
     * graphics card.
     *
     * \return The maximum number of texture units
     */
    int maxTextureUnits() const;

    /**
     * Returns the largest dimension for a 2D texture on this graphics card.
     *
     * \return The largest dimension for a 2D texture on this graphics card
     */
    int max2DTextureSize() const;

    /**
     * Returns the largest dimension for a 3D texture on this graphics card.
     *
     * \return The largest dimension for a 3D texture on this graphics card
     */
    int max3DTextureSize() const;

    /**
     * Returns the maximum number of atomic counter buffer bindings that are available on
     * the main graphics card.
     *
     * \return The maximum number of bindings
     */
    int maxAtomicCounterBufferBindings() const;

    /**
     * Returns the maximum number of shader storage bindings that are available on the
     * main graphics card.
     *
     * \return The maximum number of bindings
     */
    int maxShaderStorageBufferBindings() const;

    /**
     * Returns the maximum number of uniform buffer bindings that are available on the
     * main graphics card.
     *
     * \return The maximum number of bindings
     */
    int maxUniformBufferBindings() const;

    /**
     * Returns the `OpenGL` string.
     *
     * \return The `OpenGL` string
     */
    std::string_view name() const override;

protected:
    /**
     * Method that detects all of the capabilities.
     *
     * \throw GPUVendorError If the detection of the GPU vendor failed
     * \throw WMIError If the Windows Management Instrumentation was requested and access
     *        to it failed
     */
    void detectCapabilities() override;
    void clearCapabilities() override;

    /**
     * Detect the maximum supported GLSL Version.
     */
    void detectGLSLVersion();

    /**
     * Detect the vendor of the main GPU.
     *
     * \throw GPUVendorError if there was an error detecting the GPU vendor
     */
    void detectGPUVendor();

    /**
     * Get the vendor string from OpenGL.
     */
    void detectGLRenderer();

    /**
     * Detect all available extensions.
     */
    void detectExtensions();

    /**
     * Use WMI (on Windows) to retrieve information about the installed driver.
     *
     * \throw WMIError If there was an error accessing the Windows Management
     *        Instrumentation
     */
    void detectDriverInformation();

    /// OpenGL Version
    Version _glVersion = { .major = 0, .minor = 0, .release = 0 };
    /// GPU vendor
    std::string _glslCompiler;
    /// GPU vendor
    Vendor _vendor = Vendor::Other;
    /// GL_RENDERER
    std::string _glRenderer;
    /// Supported GLSL extensions
    std::vector<std::string> _extensions;
    /// GLEW Version
    Version _glewVersion = { .major = 0, .minor = 0, .release = 0 };

    /// The maximum supported texture size can have
    int _maxTextureSize = -1;
    /// The maximum supported texture size for 3D textures
    int _maxTextureSize3D = -1;
    /// The maximum number of texture units
    int _nTextureUnits = -1;
    /// The maximum number of atomic counter buffer bindings
    int _nAtomicCounterBufferBindings = -1;
    /// The maximum number of shader storage buffer bindings
    int _nShaderStorageBufferBindings = -1;
    /// The maximum number of uniform buffer bindings
    int _nUniformBufferBindings = -1;
    /// The maximum number of color attachments for an FBO
    int _maxFramebufferColorAttachments = -1;

    // Only used in WMI, but declared nevertheless to prevent a mismatch in compiler flags
    // between the cpp file and an application including this header
    /// Stores the version of the installed driver
    std::string _driverVersion;
    /// Stores the date of the installed driver
    std::string _driverDate;
    /// How many MB of memory is installed on the main GPU
    unsigned int _adapterRAM = 0;
    /// The name of the main GPU
    std::string _adapterName;
};

} // namespace ghoul::systemcapabilities

#define OpenGLCap (                                                                      \
    ghoul::systemcapabilities::SystemCapabilities::ref().component<                      \
        ghoul::systemcapabilities::OpenGLCapabilitiesComponent                           \
    >())

#endif // __GHOUL___OPENGLCAPABILITIESCOMPONENT___H__
