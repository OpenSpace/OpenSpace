/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>

#include <modules/globebrowsing/tile/tile.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/fmt.h>

#ifdef GLOBEBROWSING_USE_GDAL
#ifdef _MSC_VER
#pragma warning (push)
 // CPL throws warning about missing DLL interface
#pragma warning (disable : 4251)
#endif // _MSC_VER

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#endif // GLOBEBROWSING_USE_GDAL

namespace {
    constexpr const char* _loggerCat = "TileDataType";
} // namespace

namespace openspace::globebrowsing::tiledatatype {

#ifdef GLOBEBROWSING_USE_GDAL

float interpretFloat(GDALDataType gdalType, const char* src) {
    switch (gdalType) {
        case GDT_Byte:
            return static_cast<float>(*reinterpret_cast<const GLubyte*>(src));
        case GDT_UInt16:
            return static_cast<float>(*reinterpret_cast<const GLushort*>(src));
        case GDT_Int16:
            return static_cast<float>(*reinterpret_cast<const GLshort*>(src));
        case GDT_UInt32:
            return static_cast<float>(*reinterpret_cast<const GLuint*>(src));
        case GDT_Int32:
            return static_cast<float>(*reinterpret_cast<const GLint*>(src));
        case GDT_Float32:
            return static_cast<float>(*reinterpret_cast<const GLfloat*>(src));
        case GDT_Float64:
            return static_cast<float>(*reinterpret_cast<const GLdouble*>(src));
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
        }
}

size_t numberOfBytes(GDALDataType gdalType) {
    switch (gdalType) {
        case GDT_Byte:
            return sizeof(GLubyte);
        case GDT_UInt16:
            return sizeof(GLushort);
        case GDT_Int16:
            return sizeof(GLshort);
        case GDT_UInt32:
            return sizeof(GLuint);
        case GDT_Int32:
            return sizeof(GLint);
        case GDT_Float32:
            return sizeof(GLfloat);
        case GDT_Float64:
            return sizeof(GLdouble);
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
    }
}

size_t getMaximumValue(GDALDataType gdalType) {
    switch (gdalType) {
        case GDT_Byte:
            return 1ULL << 8ULL;
        case GDT_UInt16:
            return 1ULL << 16ULL;
        case GDT_Int16:
            return 1ULL << 15ULL;
        case GDT_UInt32:
            return size_t(1) << 32ULL;
        case GDT_Int32:
            return 1ULL << 31ULL;
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
    }
}

TextureFormat getTextureFormat(int rasterCount, GDALDataType gdalType) {
    TextureFormat format;

    switch (rasterCount) {
        case 1: // Red
            format.ghoulFormat = ghoul::opengl::Texture::Format::Red;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_R8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_R16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_R16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_R32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_R32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_R32F;
                    break;
                // No representation of 64 bit float?
                //case GDT_Float64:
                //    format.glFormat = GL_RED;
                //    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        case 2:
            format.ghoulFormat = ghoul::opengl::Texture::Format::RG;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_RG8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_RG16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_RG16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_RG32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_RG32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_RG32F;
                    break;
                case GDT_Float64:
                    format.glFormat = GL_RED;
                    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        case 3:
            format.ghoulFormat = ghoul::opengl::Texture::Format::RGB;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_RGB8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_RGB16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_RGB16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_RGB32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_RGB32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_RGB32F;
                    break;
                // No representation of 64 bit float?
                //case GDT_Float64:
                //    format.glFormat = GL_RED;
                //    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        case 4:
            format.ghoulFormat = ghoul::opengl::Texture::Format::RGBA;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_RGBA8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_RGBA16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_RGB16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_RGBA32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_RGBA32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_RGBA32F;
                    break;
                // No representation of 64 bit float?
                //case GDT_Float64:
                //    format.glFormat = GL_RED;
                //    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        default:
            LERROR(fmt::format(
                "Unknown number of channels for OpenGL texture: {}",
                rasterCount
            ));
            throw ghoul::MissingCaseException();
    }
    return format;
}

TextureFormat getTextureFormatOptimized(int rasterCount, GDALDataType gdalType) {
    TextureFormat format;

    switch (rasterCount) {
        case 1: // Red
            format.ghoulFormat = ghoul::opengl::Texture::Format::Red;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_R8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_R16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_R16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_R32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_R32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_R32F;
                    break;
                // No representation of 64 bit float?
                //case GDT_Float64:
                //    format.glFormat = GL_RED;
                //    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        case 2:
            format.ghoulFormat = ghoul::opengl::Texture::Format::RG;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_RG8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_RG16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_RG16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_RG32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_RG32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_RG32F;
                    break;
                case GDT_Float64:
                    format.glFormat = GL_RED;
                    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        case 3:
            format.ghoulFormat = ghoul::opengl::Texture::Format::BGR;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_RGB8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_RGB16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_RGB16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_RGB32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_RGB32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_RGB32F;
                    break;
                // No representation of 64 bit float?
                //case GDT_Float64:
                //    format.glFormat = GL_RED;
                //    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        case 4:
            format.ghoulFormat = ghoul::opengl::Texture::Format::BGRA;
            switch (gdalType) {
                case GDT_Byte:
                    format.glFormat = GL_RGBA8;
                    break;
                case GDT_UInt16:
                    format.glFormat = GL_RGBA16UI;
                    break;
                case GDT_Int16:
                    format.glFormat = GL_RGB16_SNORM;
                    break;
                case GDT_UInt32:
                    format.glFormat = GL_RGBA32UI;
                    break;
                case GDT_Int32:
                    format.glFormat = GL_RGBA32I;
                    break;
                case GDT_Float32:
                    format.glFormat = GL_RGBA32F;
                    break;
                // No representation of 64 bit float?
                //case GDT_Float64:
                //    format.glFormat = GL_RED;
                //    break;
                default:
                    LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
                    throw ghoul::MissingCaseException();
            }
            break;
        default:
            LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
            throw ghoul::MissingCaseException();
    }
    return format;
}

GLenum getOpenGLDataType(GDALDataType gdalType) {
    switch (gdalType) {
        case GDT_Byte:
            return GL_UNSIGNED_BYTE;
        case GDT_UInt16:
            return GL_UNSIGNED_SHORT;
        case GDT_Int16:
            return GL_SHORT;
        case GDT_UInt32:
            return GL_UNSIGNED_INT;
        case GDT_Int32:
            return GL_INT;
        case GDT_Float32:
            return GL_FLOAT;
        case GDT_Float64:
            return GL_DOUBLE;
        default:
            LERROR(fmt::format("GDAL data type unknown to OpenGL: {}", gdalType));
            throw ghoul::MissingCaseException();
    }
}

GDALDataType getGdalDataType(GLenum glType) {
    switch (glType) {
        case GL_UNSIGNED_BYTE:
            return GDT_Byte;
        case GL_UNSIGNED_SHORT:
            return GDT_UInt16;
        case GL_SHORT:
            return GDT_Int16;
        case GL_UNSIGNED_INT:
            return GDT_UInt32;
        case GL_INT:
            return GDT_Int32;
        case GL_FLOAT:
            return GDT_Float32;
        case GL_DOUBLE:
            return GDT_Float64;
        default:
            LERROR(fmt::format(
                "OpenGL data type unknown to GDAL: {}", static_cast<int>(glType)
            ));
            throw ghoul::MissingCaseException();
    }
}

#endif // GLOBEBROWSING_USE_GDAL

size_t numberOfRasters(ghoul::opengl::Texture::Format format) {
    switch (format) {
        case ghoul::opengl::Texture::Format::Red:
            return 1;
        case ghoul::opengl::Texture::Format::RG:
            return 2;
        case ghoul::opengl::Texture::Format::RGB:
        case ghoul::opengl::Texture::Format::BGR:
            return 3;
        case ghoul::opengl::Texture::Format::RGBA:
        case ghoul::opengl::Texture::Format::BGRA:
            return 4;
        default: {
            ghoul_assert(false, "Unknown format");
            throw ghoul::MissingCaseException();
        }
    }
}

size_t numberOfBytes(GLenum glType) {
    switch (glType) {
        case GL_UNSIGNED_BYTE:  return sizeof(GLubyte);
        case GL_BYTE:           return sizeof(GLbyte);
        case GL_UNSIGNED_SHORT: return sizeof(GLushort);
        case GL_SHORT:          return sizeof(GLshort);
        case GL_UNSIGNED_INT:   return sizeof(GLuint);
        case GL_INT:            return sizeof(GLint);
        case GL_HALF_FLOAT:     return sizeof(GLhalf);
        case GL_FLOAT:          return sizeof(GLfloat);
        case GL_DOUBLE:         return sizeof(GLdouble);
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
    }
}

size_t getMaximumValue(GLenum glType) {
    switch (glType) {
        case GL_UNSIGNED_BYTE:  return 1ULL << 8ULL;
        case GL_UNSIGNED_SHORT: return 1ULL << 16ULL;
        case GL_SHORT:          return 1ULL << 15ULL;
        case GL_UNSIGNED_INT:   return 1ULL << 32ULL;
        case GL_INT:            return 1ULL << 31ULL;
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
    }
}

float interpretFloat(GLenum glType, const char* src) {
    switch (glType) {
        case GL_UNSIGNED_BYTE:
            return static_cast<float>(*reinterpret_cast<const GLubyte*>(src));
        case GL_UNSIGNED_SHORT:
            return static_cast<float>(*reinterpret_cast<const GLushort*>(src));
        case GL_SHORT:
            return static_cast<float>(*reinterpret_cast<const GLshort*>(src));
        case GL_UNSIGNED_INT:
            return static_cast<float>(*reinterpret_cast<const GLuint*>(src));
        case GL_INT:
            return static_cast<float>(*reinterpret_cast<const GLint*>(src));
        case GL_HALF_FLOAT:
            return static_cast<float>(*reinterpret_cast<const GLhalf*>(src));
        case GL_FLOAT:
            return static_cast<float>(*reinterpret_cast<const GLfloat*>(src));
        case GL_DOUBLE:
            return static_cast<float>(*reinterpret_cast<const GLdouble*>(src));
        default:
            ghoul_assert(false, "Unknown data type");
            throw ghoul::MissingCaseException();
    }
}

GLenum glTextureFormat(GLenum glType, ghoul::opengl::Texture::Format format) {
    switch (format) {
        case ghoul::opengl::Texture::Format::Red:
            switch (glType) {
                case GL_BYTE:
                    return GL_R8;
                case GL_UNSIGNED_BYTE:
                    return GL_R8;
                case GL_INT:
                    return GL_R32I;
                case GL_UNSIGNED_INT:
                    return GL_R32UI;
                case GL_FLOAT:
                    return GL_R32F;
                case GL_HALF_FLOAT:
                    return GL_R16F;
                default:
                    ghoul_assert(false, "glType data type unknown");
                    throw ghoul::MissingCaseException();
            }
        case ghoul::opengl::Texture::Format::RG:
            switch (glType) {
                case GL_BYTE:
                    return GL_RG8;
                case GL_UNSIGNED_BYTE:
                    return GL_RG8;
                case GL_INT:
                    return GL_RG32I;
                case GL_UNSIGNED_INT:
                    return GL_RG32UI;
                case GL_FLOAT:
                    return GL_RG32F;
                case GL_HALF_FLOAT:
                    return GL_RG16F;
                default:
                    ghoul_assert(false, "glType data type unknown");
                    throw ghoul::MissingCaseException();
            }
        case ghoul::opengl::Texture::Format::RGB:
            switch (glType) {
                case GL_BYTE:
                    return GL_RGB8;
                case GL_UNSIGNED_BYTE:
                    return GL_RGB8;
                case GL_INT:
                    return GL_RGB32I;
                case GL_UNSIGNED_INT:
                    return GL_RGB32UI;
                case GL_FLOAT:
                    return GL_RGB32F;
                case GL_HALF_FLOAT:
                    return GL_RGB16F;
                default:
                    ghoul_assert(false, "glType data type unknown");
                    throw ghoul::MissingCaseException();
            }
        case ghoul::opengl::Texture::Format::RGBA:
            switch (glType) {
                case GL_BYTE:
                    return GL_RGBA8;
                case GL_UNSIGNED_BYTE:
                    return GL_RGBA8;
                case GL_INT:
                    return GL_RGBA32I;
                case GL_UNSIGNED_INT:
                    return GL_RGBA32UI;
                case GL_FLOAT:
                    return GL_RGBA32F;
                case GL_HALF_FLOAT:
                    return GL_RGBA16F;
                default:
                    ghoul_assert(false, "glType data type unknown");
                    throw ghoul::MissingCaseException();
            }
        case ghoul::opengl::Texture::Format::BGR:
            switch (glType) {
                case GL_BYTE:
                    return GL_RGB8;
                case GL_UNSIGNED_BYTE:
                    return GL_RGB8;
                case GL_INT:
                    return GL_RGB32I;
                case GL_UNSIGNED_INT:
                    return GL_RGB32UI;
                case GL_FLOAT:
                    return GL_RGB32F;
                case GL_HALF_FLOAT:
                    return GL_RGB16F;
                default:
                    ghoul_assert(false, "glType data type unknown");
                    throw ghoul::MissingCaseException();
            }
        case ghoul::opengl::Texture::Format::BGRA:
            switch (glType) {
                case GL_BYTE:
                    return GL_RGBA8;
                case GL_UNSIGNED_BYTE:
                    return GL_RGBA8;
                case GL_INT:
                    return GL_RGBA32I;
                case GL_UNSIGNED_INT:
                    return GL_RGBA32UI;
                case GL_FLOAT:
                    return GL_RGBA32F;
                case GL_HALF_FLOAT:
                    return GL_RGBA16F;
                default:
                    ghoul_assert(false, "glType data type unknown");
                    throw ghoul::MissingCaseException();
            }
        default:
            ghoul_assert(false, "Unknown format for OpenGL texture");
            throw ghoul::MissingCaseException();
    }
}

} // namespace openspace::globebrowsing::tiledatatype
