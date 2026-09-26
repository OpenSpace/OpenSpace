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

#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringconversion.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <algorithm>
#include <sstream>
#include <utility>

#ifdef WIN32
#include <Windows.h>
#pragma comment(lib, "User32.lib")
#pragma comment(lib, "Kernel32.lib")
typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);
typedef BOOL (WINAPI *PGPI)(DWORD, DWORD, DWORD, DWORD, DWORD);
#else // ^^^^ WIN32 | !WIN32 vvvv
#include <sys/utsname.h>
#endif // WIN32

namespace {
    constexpr std::string_view _loggerCat = "OpenGLCapabilities";
} // namespace

namespace ghoul::systemcapabilities {

OpenGLCapabilitiesComponent::OpenGLCapabilitiesComponentError::
OpenGLCapabilitiesComponentError(std::string msg)
    : RuntimeError(std::move(msg), "OpenGLCapabilitiesComponent")
{}

OpenGLCapabilitiesComponent::GPUVendorError::GPUVendorError(std::string msg)
    : OpenGLCapabilitiesComponentError(std::move(msg))
{}

void OpenGLCapabilitiesComponent::detectCapabilities() {
    clearCapabilities();

    detectGLSLVersion();
    detectGPUVendor();
    detectGLRenderer();
    detectExtensions();
    try {
        // This function might fail if the process has insufficient priviledges to access
        // the WMI on Windows
        detectDriverInformation();
    }
    catch (const std::runtime_error& e) {
        LWARNINGC("OpenGLCapabilitiesComponent", e.what());
    }

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &_maxTextureSize);
    glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE, &_maxTextureSize3D);

    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, &_nTextureUnits);
    glGetIntegerv(GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS, &_nAtomicCounterBufferBindings);
    glGetIntegerv(GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS, &_nShaderStorageBufferBindings);
    glGetIntegerv(GL_MAX_UNIFORM_BUFFER_BINDINGS, &_nUniformBufferBindings);

    glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, &_maxFramebufferColorAttachments);
}

void OpenGLCapabilitiesComponent::detectGLSLVersion() {
    glGetIntegerv(GL_MAJOR_VERSION, &(_glVersion.major));
    glGetIntegerv(GL_MINOR_VERSION, &(_glVersion.minor));
}

void OpenGLCapabilitiesComponent::detectGPUVendor() {
    _glslCompiler = reinterpret_cast<const char*>(glGetString(GL_VENDOR));

    if (_glslCompiler.contains("NVIDIA")) {
        _vendor = Vendor::Nvidia;
    }
    else if (_glslCompiler.contains("ATI") || _glslCompiler.contains("AMD")) {
        _vendor = Vendor::AmdATI;
    }
    else if (_glslCompiler.contains("INTEL") || _glslCompiler.contains("Intel")) {
        _vendor = Vendor::Intel;
    }
    else {
        LINFO(std::format(
            "Vendor of graphics card is not in the enum 'Vendor'. Vendor information: {}",
            _glslCompiler
        ));
        _vendor = Vendor::Other;
    }
}

void OpenGLCapabilitiesComponent::detectGLRenderer() {
    _glRenderer = reinterpret_cast<const char*>(glGetString(GL_RENDERER));
}

void OpenGLCapabilitiesComponent::detectExtensions() {
    GLint nExtensions = 0;
    glGetIntegerv(GL_NUM_EXTENSIONS, &nExtensions);
    for (GLint i = 0; i < nExtensions; i++) {
        const GLubyte* ext = glGetStringi(GL_EXTENSIONS, i);
        if (ext) {
            const std::string extension = reinterpret_cast<const char*>(ext);
            _extensions.push_back(extension);
        }
    }
}

void OpenGLCapabilitiesComponent::detectDriverInformation() {
#ifdef WIN32
    queryWMI("Win32_VideoController", "DriverVersion", _driverVersion);

    std::string date;
    queryWMI("Win32_VideoController", "DriverDate", date);

    _driverDate = std::format(
        "{}-{}-{}", date.substr(0, 4), date.substr(4, 2), date.substr(6, 2)
    );

    queryWMI("Win32_VideoController", "AdapterRAM", _adapterRAM);
    // `adapterRAM` is in bytes
    _adapterRAM = (_adapterRAM / 1024) / 1024;

    queryWMI("Win32_VideoController", "Name", _adapterName);
#endif // WIN32
}

void OpenGLCapabilitiesComponent::clearCapabilities() {
    _glVersion = { .major = 0, .minor = 0, .release = 0 };
    _glslCompiler.clear();
    _vendor = Vendor::Other;
    _glRenderer.clear();
    _extensions.clear();
    _glewVersion = { .major = 0, .minor = 0, .release = 0 };

    _maxTextureSize = -1;
    _maxTextureSize3D = -1;
    _nTextureUnits = -1;

#ifdef WIN32
    _driverVersion.clear();
    _driverDate.clear();
    _adapterRAM = 0;
    _adapterName.clear();
#endif // WIN32
}

std::vector<SystemCapabilitiesComponent::CapabilityInformation>
OpenGLCapabilitiesComponent::capabilities() const
{
    std::vector<SystemCapabilitiesComponent::CapabilityInformation> res;
    res.push_back({ "OpenGL Version", to_string(_glVersion), Verbosity::Minimal });
    res.push_back({ "OpenGL Compiler", _glslCompiler, Verbosity::Minimal });
    res.push_back({ "OpenGL Renderer", _glRenderer, Verbosity::Minimal });
    res.push_back({ "GPU Vendor", std::string(gpuVendorString()), Verbosity::Minimal });
    res.push_back({"GLEW Version", to_string(_glewVersion),Verbosity::Minimal});
#ifdef WIN32
    res.push_back({ "GPU Name", _adapterName, Verbosity::Minimal });
    res.push_back({ "GPU Driver Version", _driverVersion, Verbosity::Minimal });
    res.push_back({ "GPU Driver Date", _driverDate, Verbosity::Minimal });
    res.push_back({ "GPU RAM", to_string(_adapterRAM) + " MB",Verbosity::Minimal });
#endif // WIN32

    res.push_back({ "Max Texture Size", to_string(_maxTextureSize), Verbosity::Default });
    res.push_back({
        "Max 3D Texture Size", to_string(_maxTextureSize3D), Verbosity::Default }
    );
    res.push_back({
        "Num of Texture Units", to_string(_nTextureUnits), Verbosity::Default }
    );
    res.push_back({
        "FBO Color Attachments",
        to_string(_maxFramebufferColorAttachments),
        Verbosity::Default
    });

    std::stringstream s;
    if (!_extensions.empty()) {
        for (size_t i = 0; i < _extensions.size() - 1; i++) {
            s << _extensions[i] << ", ";
        }
        s << _extensions[_extensions.size() - 1] << "\n";
    }
    res.push_back({ "Extensions", s.str(), Verbosity::Full });
    return res;
}

Version OpenGLCapabilitiesComponent::openGLVersion() const {
    return _glVersion;
}

const std::string& OpenGLCapabilitiesComponent::glslCompiler() const {
    return _glslCompiler;
}

OpenGLCapabilitiesComponent::Vendor OpenGLCapabilitiesComponent::gpuVendor() const {
    return _vendor;
}

const std::vector<std::string>& OpenGLCapabilitiesComponent::extensions() const {
    return _extensions;
}

bool OpenGLCapabilitiesComponent::isExtensionSupported(std::string_view extension) const {
    const auto result = std::find(_extensions.cbegin(), _extensions.cend(), extension);
    return (result != _extensions.cend());
}

int OpenGLCapabilitiesComponent::maxTextureUnits() const {
    return _nTextureUnits;
}

int OpenGLCapabilitiesComponent::max2DTextureSize() const {
    return _maxTextureSize;
}

int OpenGLCapabilitiesComponent::max3DTextureSize() const {
    return _maxTextureSize3D;
}

int OpenGLCapabilitiesComponent::maxAtomicCounterBufferBindings() const {
    return _nAtomicCounterBufferBindings;
}

int OpenGLCapabilitiesComponent::maxShaderStorageBufferBindings() const {
    return _nShaderStorageBufferBindings;
}

int OpenGLCapabilitiesComponent::maxUniformBufferBindings() const {
    return _nUniformBufferBindings;
}

std::string_view OpenGLCapabilitiesComponent::gpuVendorString() const {
    switch (_vendor) {
        case Vendor::Nvidia: return "Nvidia";
        case Vendor::AmdATI: return "AMD/ATI";
        case Vendor::Intel:  return "Intel";
        default:             return "other";
    }
}

std::string_view OpenGLCapabilitiesComponent::name() const {
    return "OpenGL";
}

} // namespace ghoul::systemcapabilities
