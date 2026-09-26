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
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR .OTHER LIABILITY, WHETHER IN AN ACTION OF *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <ghoul/opengl/texture.h>

#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <cstring>

namespace {
    using namespace ghoul;
    using namespace ghoul::opengl;

    struct FormatError : public RuntimeError {
        explicit FormatError(Texture::Format format, GLenum dataType)
            : RuntimeError(
                std::format(
                    "Unhandled combination '{}' / '{}'",
                    static_cast<int>(format), static_cast<int>(dataType)
                ),
                "FormatError"
            )
        {}
    };

    /**
     * Returns the internal format for the provided format initialization struct. If the
     * internal format is explicitly specified, it is returned. Otherwise it is deduced
     * from the format and dataType parameters.
     */
    constexpr GLenum toInternalFormat(const Texture::FormatInit& frmt) {
        if (frmt.internalFormat.has_value()) {
            return *frmt.internalFormat;
        }

        switch (frmt.format) {
            case Texture::Format::Red:
                switch (frmt.dataType) {
                    case GL_UNSIGNED_BYTE:  return GL_R8;
                    case GL_BYTE:           return GL_R8;
                    case GL_UNSIGNED_SHORT: return GL_R16;
                    case GL_SHORT:          return GL_R16;
                    case GL_FLOAT:          return GL_R32F;
                    default:                throw FormatError(frmt.format, frmt.dataType);
                }
            case Texture::Format::RG:
                switch (frmt.dataType) {
                    case GL_UNSIGNED_BYTE:  return GL_RG8;
                    case GL_BYTE:           return GL_RG8;
                    case GL_UNSIGNED_SHORT: return GL_RG16;
                    case GL_SHORT:          return GL_RG16;
                    case GL_FLOAT:          return GL_RG32F;
                    default:                throw FormatError(frmt.format, frmt.dataType);
                }
            case Texture::Format::RGB:
                switch (frmt.dataType) {
                    case GL_UNSIGNED_BYTE:  return GL_RGB8;
                    case GL_BYTE:           return GL_RGB8;
                    case GL_UNSIGNED_SHORT: return GL_RGB16;
                    case GL_SHORT:          return GL_RGB16;
                    case GL_FLOAT:          return GL_RGB32F;
                    default:                throw FormatError(frmt.format, frmt.dataType);
                }
            case Texture::Format::BGR:
                switch (frmt.dataType) {
                    case GL_UNSIGNED_BYTE:  return GL_RGB8;
                    case GL_BYTE:           return GL_RGB8;
                    case GL_UNSIGNED_SHORT: return GL_RGB16;
                    case GL_SHORT:          return GL_RGB16;
                    case GL_FLOAT:          return GL_RGB32F;
                    default:                throw FormatError(frmt.format, frmt.dataType);
                }
            case Texture::Format::RGBA:
                switch (frmt.dataType) {
                    case GL_UNSIGNED_BYTE:  return GL_RGBA8;
                    case GL_BYTE:           return GL_RGBA8;
                    case GL_UNSIGNED_SHORT: return GL_RGBA16;
                    case GL_SHORT:          return GL_RGBA16;
                    case GL_FLOAT:          return GL_RGBA32F;
                    default:                throw FormatError(frmt.format, frmt.dataType);
                }
            case Texture::Format::BGRA:
                switch (frmt.dataType) {
                    case GL_UNSIGNED_BYTE:  return GL_RGBA8;
                    case GL_BYTE:           return GL_RGBA8;
                    case GL_UNSIGNED_SHORT: return GL_RGBA16;
                    case GL_SHORT:          return GL_RGBA16;
                    case GL_FLOAT:          return GL_RGBA32F;
                    default:                throw FormatError(frmt.format, frmt.dataType);
                }
            case Texture::Format::DepthComponent:
                switch (frmt.dataType) {
                    case GL_UNSIGNED_SHORT: return GL_DEPTH_COMPONENT16;
                    case GL_SHORT:          return GL_DEPTH_COMPONENT16;
                    case GL_UNSIGNED_INT:   return GL_DEPTH_COMPONENT32;
                    case GL_INT:            return GL_DEPTH_COMPONENT32;
                    case GL_FLOAT:          return GL_DEPTH_COMPONENT32F;
                    default:                throw FormatError(frmt.format, frmt.dataType);
                }
            default:
                throw MissingCaseException();
        }
    }

    constexpr Texture::WrappingModes toWrappingModes(std::variant<
                                                         Texture::WrappingMode,
                                                         Texture::WrappingModes
                                                     > wrappingMode)
    {
        if (std::holds_alternative<Texture::WrappingMode>(wrappingMode)) {
            return {
                std::get<Texture::WrappingMode>(wrappingMode),
                std::get<Texture::WrappingMode>(wrappingMode),
                std::get<Texture::WrappingMode>(wrappingMode)
            };
        }
        else {
            return std::get<Texture::WrappingModes>(wrappingMode);
        }
    }


    std::optional<std::array<GLenum, 4>> parseSwizzleMask(
                                                      const Texture::SamplerInit& sampler,
                                                        const Texture::FormatInit& format)
    {
        if (sampler.swizzleMask.has_value()) {
            return *sampler.swizzleMask;
        }

        if (sampler.autoSwizzleGrayscale && format.format == Texture::Format::Red) {
            return std::array<GLenum, 4>{ GL_RED, GL_RED, GL_RED, GL_ONE };
        }

        return std::nullopt;
    }

    constexpr int nChannels(Texture::Format format) {
        switch (format) {
            case Texture::Format::Red:            return 1;
            case Texture::Format::DepthComponent: return 1;
            case Texture::Format::RG:             return 2;
            case Texture::Format::RGB:            return 3;
            case Texture::Format::BGR:            return 3;
            case Texture::Format::RGBA:           return 4;
            case Texture::Format::BGRA:           return 4;
        }
        return 0;
    }

    constexpr GLubyte bytesPerPixel(Texture::Format format, GLenum dataType) {
        const int n = nChannels(format);

        switch (dataType) {
            case GL_UNSIGNED_BYTE:               return static_cast<GLubyte>(1 * n);
            case GL_BYTE:                        return static_cast<GLubyte>(1 * n);
            case GL_UNSIGNED_BYTE_3_3_2:         return static_cast<GLubyte>(1 * n);
            case GL_UNSIGNED_BYTE_2_3_3_REV:     return static_cast<GLubyte>(1 * n);
            case GL_UNSIGNED_SHORT:              return static_cast<GLubyte>(2 * n);
            case GL_SHORT:                       return static_cast<GLubyte>(2 * n);
            case GL_UNSIGNED_SHORT_5_6_5:        return static_cast<GLubyte>(2 * n);
            case GL_UNSIGNED_SHORT_5_6_5_REV:    return static_cast<GLubyte>(2 * n);
            case GL_UNSIGNED_SHORT_4_4_4_4:      return static_cast<GLubyte>(2 * n);
            case GL_UNSIGNED_SHORT_4_4_4_4_REV:  return static_cast<GLubyte>(2 * n);
            case GL_UNSIGNED_SHORT_5_5_5_1:      return static_cast<GLubyte>(2 * n);
            case GL_UNSIGNED_SHORT_1_5_5_5_REV:  return static_cast<GLubyte>(2 * n);
            case GL_UNSIGNED_INT:                return static_cast<GLubyte>(4 * n);
            case GL_INT:                         return static_cast<GLubyte>(4 * n);
            case GL_FLOAT:                       return static_cast<GLubyte>(4 * n);
            case GL_UNSIGNED_INT_8_8_8_8:        return static_cast<GLubyte>(4 * n);
            case GL_UNSIGNED_INT_8_8_8_8_REV:    return static_cast<GLubyte>(4 * n);
            case GL_UNSIGNED_INT_10_10_10_2:     return static_cast<GLubyte>(4 * n);
            case GL_UNSIGNED_INT_2_10_10_10_REV: return static_cast<GLubyte>(4 * n);
            default:                             throw MissingCaseException();
        }
    }
} // namespace

namespace ghoul::opengl {

Texture::Texture(FormatInit format, SamplerInit sampler, const std::byte* data,
                 int pixelAlignment, KeepMemory keepMemory)
    : _type(std::move(format.type))
    , _dimensions(std::move(format.dimensions))
    , _format(std::move(format.format))
    , _internalFormat(toInternalFormat(format))
    , _dataType(std::move(format.dataType))
    , _filter(std::move(sampler.filter))
    , _wrapping(toWrappingModes(sampler.wrapping))
    , _borderColor(std::move(sampler.borderColor))
    , _swizzleMask(parseSwizzleMask(sampler, format))
    , _mipMapLevel(sampler.mipMapLevel.value_or(8))
    , _pixelAlignment(std::move(pixelAlignment))
{
    ghoul_assert(_dimensions.x >= 1, "X dimension must be positive");
    ghoul_assert(_dimensions.y >= 1, "Y dimension must be positive");
    ghoul_assert(_dimensions.z >= 1, "Z dimension must be positive");

    initialize(data);

    if (data && keepMemory) {
        const int nBytes = expectedPixelDataSize();
        _pixels.resize(nBytes);
        std::memcpy(_pixels.data(), data, nBytes);
    }
}

Texture::~Texture() {
    glDeleteTextures(1, &_id);
}

void Texture::initialize(const std::byte* data) {
    ZoneScoped;

    //
    // Create OpenGL name
    //
    glCreateTextures(_type, 1, &_id);


    //
    // Set filtering parameters
    //
    switch (_filter) {
        case FilterMode::Nearest:
            glTextureParameteri(_id, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
            glTextureParameteri(_id, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
            break;
        case FilterMode::Linear:
            glTextureParameteri(_id, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            glTextureParameteri(_id, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            break;
        case FilterMode::LinearMipMap:
            glTextureParameteri(_id, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
            glTextureParameteri(_id, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            glTextureParameteri(_id, GL_TEXTURE_MAX_LEVEL, _mipMapLevel - 1);
            break;
        case FilterMode::AnisotropicMipMap:
            glTextureParameteri(_id, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
            glTextureParameteri(_id, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            glTextureParameteri(_id, GL_TEXTURE_MAX_LEVEL, _mipMapLevel - 1);
            if (std::equal_to<>()(_anisotropyLevel, -1.f)) {
                GLfloat maxTextureAnisotropy = 1.0;
                glGetFloatv(
                    GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT,
                    &maxTextureAnisotropy
                );
                _anisotropyLevel = maxTextureAnisotropy;
            }
            glTextureParameterf(_id, GL_TEXTURE_MAX_ANISOTROPY_EXT, _anisotropyLevel);
            glTextureParameteri(_id, GL_TEXTURE_BASE_LEVEL, 0);
            break;
    }


    //
    // Set the wrapping mode
    //
    switch (_type) {
        case GL_TEXTURE_1D:
            glTextureParameteri(_id, GL_TEXTURE_WRAP_S, static_cast<GLint>(_wrapping.s));
            break;
        case GL_TEXTURE_2D:
            glTextureParameteri(_id, GL_TEXTURE_WRAP_S, static_cast<GLint>(_wrapping.s));
            glTextureParameteri(_id, GL_TEXTURE_WRAP_T, static_cast<GLint>(_wrapping.t));
            break;
        case GL_TEXTURE_3D:
            glTextureParameteri(_id, GL_TEXTURE_WRAP_S, static_cast<GLint>(_wrapping.s));
            glTextureParameteri(_id, GL_TEXTURE_WRAP_T, static_cast<GLint>(_wrapping.t));
            glTextureParameteri(_id, GL_TEXTURE_WRAP_R, static_cast<GLint>(_wrapping.r));
            break;
        default:
            throw MissingCaseException();
    }


    //
    // Apply the swizzle mask
    //
    if (_swizzleMask.has_value()) {
        glTextureParameteriv(_id, GL_TEXTURE_SWIZZLE_RGBA, _swizzleMask->data());
    }


    //
    // Set the border color
    //
    if (_borderColor.has_value()) {
        glTextureParameterfv(_id, GL_TEXTURE_BORDER_COLOR, glm::value_ptr(*_borderColor));
    }


    //
    // Create data storage
    //
    const bool useMipMaps =
        (_filter == FilterMode::LinearMipMap) ||
        (_filter == FilterMode::AnisotropicMipMap);
    const GLsizei nMipMapLevels = [this, useMipMaps]() {
        if (!useMipMaps || _dimensions.x == 1) {
            return 1;
        }

        // For each dimension calculate the maximally allowed mipmap level and then take
        // the one that is lowest as the final result
        const int x = static_cast<int>(std::floor(std::log2(_dimensions.x)));
        const int y = static_cast<int>(std::floor(std::log2(_dimensions.y)));
        const int z = static_cast<int>(std::floor(std::log2(_dimensions.z)));

        if (_dimensions.z > 1) {
            return std::min(std::min(std::min(x, y), z), _mipMapLevel);
        }
        if (_dimensions.y > 1) {
            return std::min(std::min(x, y), _mipMapLevel);
        }
        return std::min(x, _mipMapLevel);
    }();
    _usesMipMapping = useMipMaps && nMipMapLevels > 1;

    switch (_type) {
        case GL_TEXTURE_1D:
            glTextureStorage1D(_id, nMipMapLevels, _internalFormat, _dimensions.x);
            break;
        case GL_TEXTURE_2D:
            glTextureStorage2D(
                _id,
                nMipMapLevels,
                _internalFormat,
                _dimensions.x,
                _dimensions.y
            );
            break;
        case GL_TEXTURE_3D:
            glTextureStorage3D(
                _id,
                nMipMapLevels,
                _internalFormat,
                _dimensions.x,
                _dimensions.y,
                _dimensions.z
            );
            break;
        default:
            throw MissingCaseException();
    }


    //
    // Upload data
    //
    if (data) {
        uploadTexture(data);

        if (_usesMipMapping) {
            glGenerateTextureMipmap(_id);
        }
    }
}

void Texture::uploadTexture(const std::byte* data) const {
    ghoul_assert(data, "Data must be provided");

    glBindTexture(_type, _id);
    glPixelStorei(GL_UNPACK_ALIGNMENT, _pixelAlignment);

    switch (_type) {
        case GL_TEXTURE_1D:
            glTextureSubImage1D(
                _id,
                0,
                0,
                GLsizei(_dimensions.x),
                GLenum(_format),
                _dataType,
                data
            );
            break;
        case GL_TEXTURE_2D:
            glTextureSubImage2D(
                _id,
                0,
                0,
                0,
                GLsizei(_dimensions.x),
                GLsizei(_dimensions.y),
                GLenum(_format),
                _dataType,
                data
            );
            break;
        case GL_TEXTURE_3D:
            glTextureSubImage3D(
                _id,
                0,
                0,
                0,
                0,
                GLsizei(_dimensions.x),
                GLsizei(_dimensions.y),
                GLsizei(_dimensions.z),
                GLenum(_format),
                _dataType,
                data
            );
            break;
        default:
            throw MissingCaseException();
    }

    if (_usesMipMapping) {
        glGenerateTextureMipmap(_id);
    }
}

void Texture::resize(glm::uvec3 dimensions) {
    if (dimensions == _dimensions) {
        return;
    }

    _dimensions = std::move(dimensions);

    glDeleteTextures(1, &_id);
    initialize(nullptr);
}

Texture::operator GLuint() const {
    return _id;
}

void Texture::setName(std::string name) {
    _name = std::move(name);
    glObjectLabel(GL_TEXTURE, _id, static_cast<GLsizei>(_name.size()), _name.data());
}

const std::string& Texture::name() const {
    return _name;
}

glm::uvec3 Texture::dimensions() const {
    return _dimensions;
}

int Texture::numberOfChannels() const {
    return nChannels(_format);
}

GLenum Texture::type() const {
    return _type;
}

Texture::Format Texture::format() const {
    return _format;
}

GLenum Texture::internalFormat() const {
    return _internalFormat;
}

GLenum Texture::dataType() const {
    return _dataType;
}

std::vector<std::byte> Texture::pixelData() const {
    // Set the pixel alignment for reading
    glPixelStorei(GL_PACK_ALIGNMENT, _pixelAlignment);

    const int nBytes = expectedPixelDataSize();
    std::vector<std::byte> res = std::vector<std::byte>(nBytes);
    glGetTextureImage(_id, 0, GLenum(_format), _dataType, nBytes, res.data());
    return res;
}

Texture::Format Texture::formatFromNumChannels(int nChannels) {
    switch (nChannels) {
        case 1: return Texture::Format::Red;
        case 2: return Texture::Format::RG;
        case 3: return Texture::Format::RGB;
        case 4: return Texture::Format::RGBA;
        default:
            throw ghoul::RuntimeError(
                std::format("Unsupported channel count: {}", nChannels)
            );
    }
}

void Texture::downloadTexture() {
    if (!_pixels.empty()) {
        return;
    }

    // Set the pixel alignment for reading
    glPixelStorei(GL_PACK_ALIGNMENT, _pixelAlignment);

    const int nBytes = expectedPixelDataSize();
    _pixels.resize(nBytes);
    glGetTextureImage(_id, 0, GLenum(_format), _dataType, nBytes, _pixels.data());
}

const std::vector<std::byte> Texture::cachedPixelData() const {
    return _pixels;
}

void Texture::clearDownloadedTexture() {
    _pixels.clear();
}

int Texture::expectedPixelDataSize() const {
    return glm::compMul(_dimensions) * bytesPerPixel(_format, _dataType);
}

void Texture::setPixelData(const std::byte* pixels, int pixelAlignment,
                           KeepMemory keepMemory)
{
    _pixelAlignment = pixelAlignment;
    uploadTexture(pixels);

    if (keepMemory) {
        const int nBytes = expectedPixelDataSize();
        _pixels.resize(nBytes);
        std::memcpy(_pixels.data(), pixels, nBytes);
    }
}

template <class T>
const T& Texture::texel(const glm::uvec3& pos) const {
    ghoul_assert(
        sizeof(T) == bytesPerPixel(_format, _dataType),
        "Size of T must be equal to texel size"
    );
    ghoul_assert(
        pos.x < _dimensions.x,
        "x must be smaller than the width of the Texture"
    );
    ghoul_assert(
        pos.y < _dimensions.y,
        "y must be smaller than the height of the Texture"
    );
    ghoul_assert(
        pos.z < _dimensions.z,
        "z must be smaller than the depth of the Texture"
    );
    return (reinterpret_cast<const T*>(_pixels.data())[
        (pos.z * _dimensions.x * _dimensions.y) + (pos.y * _dimensions.x) + pos.x
    ]);
}

glm::vec4 Texture::texelAsFloat(const glm::uvec3& pos) const {
    ghoul_assert(
        (pos.z * _dimensions.x * _dimensions.y) + (pos.y * _dimensions.x) + pos.x
        < glm::compMul(_dimensions),
        "x, y, and z must be inside the texture dimensions"
    );
    ghoul_assert(!_pixels.empty(), "No texture was downloaded before call");

    switch (_format) {
        case Format::Red:
            switch (_dataType) {
                case GL_UNSIGNED_BYTE:
                {
                    const uint8_t t = texel<uint8_t>(pos);
                    const float tt =
                        static_cast<float>(t) / std::numeric_limits<uint8_t>::max();
                    return { tt, 0.f, 0.f, 1.f };
                }
                case GL_BYTE:
                {
                    const int8_t t = texel<int8_t>(pos);
                    const float tt =
                        static_cast<float>(t) / std::numeric_limits<int8_t>::max();
                    return { tt, 0.f, 0.f, 1.f };
                }
                case GL_UNSIGNED_SHORT:
                {
                    const uint16_t t = texel<uint16_t>(pos);
                    const float tt =
                        static_cast<float>(t) / std::numeric_limits<uint16_t>::max();
                    return { tt, 0.f, 0.f, 1.f };
                }
                case GL_SHORT:
                {
                    const int16_t t = texel<int16_t>(pos);
                    const float tt =
                        static_cast<float>(t) / std::numeric_limits<int16_t>::max();
                    return { tt, 0.f, 0.f, 1.f };
                }
                case GL_UNSIGNED_INT:
                {
                    const uint32_t t = texel<uint32_t>(pos);
                    constexpr uint32_t max = std::numeric_limits<uint32_t>::max();
                    const float tt = static_cast<float>(t) / static_cast<float>(max);
                    return { tt, 0.f, 0.f, 1.f };
                }
                case GL_INT:
                {
                    const int32_t t = texel<int32_t>(pos);
                    constexpr int32_t max = std::numeric_limits<int32_t>::max();
                    const float tt = static_cast<float>(t) / static_cast<float>(max);
                    return { tt, 0.f, 0.f, 1.f };
                }
                case GL_FLOAT:
                {
                    const float t = texel<float>(pos);
                    return { t, 0.f, 0.f, 1.f };
                }
                default:
                    ghoul_assert(false, "Missing case label");
                    throw MissingCaseException();
            }
        case Format::RG:
            switch (_dataType) {
                case GL_UNSIGNED_BYTE:
                {
                    const glm::tvec2<uint8_t>& t = texel<glm::tvec2<uint8_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<uint8_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<uint8_t>::max();
                    return { tr, tg, 0.f, 1.f };
                }
                case GL_BYTE:
                {
                    const glm::tvec2<int8_t>& t = texel<glm::tvec2<int8_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<int8_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<int8_t>::max();
                    return { tr, tg, 0.f, 1.f };
                }
                case GL_UNSIGNED_SHORT:
                {
                    const glm::tvec2<uint16_t>& t = texel<glm::tvec2<uint16_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<uint16_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<uint16_t>::max();
                    return { tr, tg, 0.f, 1.f };
                }
                case GL_SHORT:
                {
                    const glm::tvec2<int16_t>& t = texel<glm::tvec2<int16_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<int16_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<int16_t>::max();
                    return { tr, tg, 0.f, 1.f };
                }
                case GL_UNSIGNED_INT:
                {
                    const glm::tvec2<uint32_t>& t = texel<glm::tvec2<uint32_t>>(pos);
                    constexpr uint32_t max = std::numeric_limits<uint32_t>::max();
                    const float tr = static_cast<float>(t.r) / static_cast<float>(max);
                    const float tg = static_cast<float>(t.g) / static_cast<float>(max);
                    return { tr, tg, 0.f, 1.f };
                }
                case GL_INT:
                {
                    const glm::tvec2<int32_t>& t = texel<glm::tvec2<int32_t>>(pos);
                    constexpr int32_t max = std::numeric_limits<int32_t>::max();
                    const float tr = static_cast<float>(t.r) / static_cast<float>(max);
                    const float tg = static_cast<float>(t.g) / static_cast<float>(max);
                    return { tr, tg, 0.f, 1.f };
                }
                case GL_FLOAT:
                {
                    const glm::vec2& t = texel<glm::vec2>(pos);
                    return { t.r, t.g, 0.f, 1.f };
                }
                default:
                    ghoul_assert(false, "Missing case label");
                    throw MissingCaseException();
            }
        case Format::RGB:
            [[fallthrough]];
        case Format::BGR:
            switch (_dataType) {
                case GL_UNSIGNED_BYTE:
                {
                    const glm::tvec3<uint8_t>& t = texel<glm::tvec3<uint8_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<uint8_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<uint8_t>::max();
                    const float tb =
                        static_cast<float>(t.b) / std::numeric_limits<uint8_t>::max();
                    return { tr, tg, tb, 1.f };
                }
                case GL_BYTE:
                {
                    const glm::tvec3<int8_t>& t = texel<glm::tvec3<int8_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<int8_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<int8_t>::max();
                    const float tb =
                        static_cast<float>(t.b) / std::numeric_limits<int8_t>::max();
                    return { tr, tg, tb, 1.f };
                }
                case GL_UNSIGNED_SHORT:
                {
                    const glm::tvec3<uint16_t>& t = texel<glm::tvec3<uint16_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<uint16_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<uint16_t>::max();
                    const float tb =
                        static_cast<float>(t.b) / std::numeric_limits<uint16_t>::max();
                    return { tr, tg, tb, 1.f };
                }
                case GL_SHORT:
                {
                    const glm::tvec3<int16_t>& t = texel<glm::tvec3<int16_t>>(pos);
                    const float tr =
                        static_cast<float>(t.r) / std::numeric_limits<int16_t>::max();
                    const float tg =
                        static_cast<float>(t.g) / std::numeric_limits<int16_t>::max();
                    const float tb =
                        static_cast<float>(t.b) / std::numeric_limits<int16_t>::max();
                    return { tr, tg, tb, 1.f };
                }
                case GL_UNSIGNED_INT:
                {
                    const glm::tvec3<uint32_t>& t = texel<glm::tvec3<uint32_t>>(pos);
                    constexpr uint32_t max = std::numeric_limits<uint32_t>::max();
                    const float tr = static_cast<float>(t.r) / static_cast<float>(max);
                    const float tg = static_cast<float>(t.g) / static_cast<float>(max);
                    const float tb = static_cast<float>(t.b) / static_cast<float>(max);
                    return { tr, tg, tb, 1.f };
                }
                case GL_INT:
                {
                    const glm::tvec3<int32_t>& t = texel<glm::tvec3<int32_t>>(pos);
                    constexpr int32_t max = std::numeric_limits<int32_t>::max();
                    const float tr = static_cast<float>(t.r) / static_cast<float>(max);
                    const float tg = static_cast<float>(t.g) / static_cast<float>(max);
                    const float tb = static_cast<float>(t.b) / static_cast<float>(max);
                    return { tr, tg, tb, 1.f };
                }
                case GL_FLOAT:
                {
                    const glm::vec3& t = texel<glm::vec3>(pos);
                    return { t.r, t.g, t.b, 1.f };
                }
                default:
                    ghoul_assert(false, "Missing case label");
                    throw MissingCaseException();
            }
        case Format::RGBA:
            [[fallthrough]];
        case Format::BGRA:
            switch (_dataType) {
                case GL_UNSIGNED_BYTE:
                {
                    const glm::tvec4<uint8_t>& t = texel<glm::tvec4<uint8_t>>(pos);
                    return {
                        static_cast<float>(t.r) / std::numeric_limits<uint8_t>::max(),
                        static_cast<float>(t.g) / std::numeric_limits<uint8_t>::max(),
                        static_cast<float>(t.b) / std::numeric_limits<uint8_t>::max(),
                        static_cast<float>(t.a) / std::numeric_limits<uint8_t>::max()
                    };
                }
                case GL_BYTE:
                {
                    const glm::tvec4<int8_t>& t = texel<glm::tvec4<int8_t>>(pos);
                    return {
                        static_cast<float>(t.r) / std::numeric_limits<int8_t>::max(),
                        static_cast<float>(t.g) / std::numeric_limits<int8_t>::max(),
                        static_cast<float>(t.b) / std::numeric_limits<int8_t>::max(),
                        static_cast<float>(t.a) / std::numeric_limits<int8_t>::max()
                    };
                }
                case GL_UNSIGNED_SHORT:
                {
                    const glm::tvec4<uint16_t>& t = texel<glm::tvec4<uint16_t>>(pos);
                    return {
                        static_cast<float>(t.r) / std::numeric_limits<uint16_t>::max(),
                        static_cast<float>(t.g) / std::numeric_limits<uint16_t>::max(),
                        static_cast<float>(t.b) / std::numeric_limits<uint16_t>::max(),
                        static_cast<float>(t.a) / std::numeric_limits<uint16_t>::max()
                    };
                }
                case GL_SHORT:
                {
                    const glm::tvec4<int16_t>& t = texel<glm::tvec4<int16_t>>(pos);
                    return {
                        static_cast<float>(t.r) / std::numeric_limits<int16_t>::max(),
                        static_cast<float>(t.g) / std::numeric_limits<int16_t>::max(),
                        static_cast<float>(t.b) / std::numeric_limits<int16_t>::max(),
                        static_cast<float>(t.a) / std::numeric_limits<int16_t>::max()
                    };
                }
                case GL_UNSIGNED_INT:
                {
                    const glm::tvec4<uint32_t>& t = texel<glm::tvec4<uint32_t>>(pos);
                    constexpr uint32_t max = std::numeric_limits<uint32_t>::max();
                    return {
                        static_cast<float>(t.r) / static_cast<float>(max),
                        static_cast<float>(t.g) / static_cast<float>(max),
                        static_cast<float>(t.b) / static_cast<float>(max),
                        static_cast<float>(t.a) / static_cast<float>(max)
                    };
                }
                case GL_INT:
                {
                    const glm::tvec4<int32_t>& t = texel<glm::tvec4<int32_t>>(pos);
                    constexpr int32_t max = std::numeric_limits<int32_t>::max();
                    return {
                        static_cast<float>(t.r) / static_cast<float>(max),
                        static_cast<float>(t.g) / static_cast<float>(max),
                        static_cast<float>(t.b) / static_cast<float>(max),
                        static_cast<float>(t.a) / static_cast<float>(max)
                    };
                }
                case GL_FLOAT:
                    return texel<glm::vec4>(pos);
                default:
                    throw MissingCaseException();
            }
        case Format::DepthComponent:
            return glm::vec4(0.f);
        default:
            throw MissingCaseException();
    }
}

template const uint8_t& Texture::texel(const glm::uvec3& pos) const;
template const uint16_t& Texture::texel(const glm::uvec3& pos) const;
template const uint32_t& Texture::texel(const glm::uvec3& pos) const;
template const int8_t& Texture::texel(const glm::uvec3& pos) const;
template const int16_t& Texture::texel(const glm::uvec3& pos) const;
template const int32_t& Texture::texel(const glm::uvec3& pos) const;
template const float& Texture::texel(const glm::uvec3& pos) const;

template const glm::tvec2<uint8_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec2<uint16_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec2<uint32_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec2<int8_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec2<int16_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec2<int32_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec2<float>& Texture::texel(const glm::uvec3& pos) const;

template const glm::tvec3<uint8_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec3<uint16_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec3<uint32_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec3<int8_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec3<int16_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec3<int32_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec3<float>& Texture::texel(const glm::uvec3& pos) const;

template const glm::tvec4<uint8_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec4<uint16_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec4<uint32_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec4<int8_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec4<int16_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec4<int32_t>& Texture::texel(const glm::uvec3& pos) const;
template const glm::tvec4<float>& Texture::texel(const glm::uvec3& pos) const;

} // namespace ghoul::opengl
