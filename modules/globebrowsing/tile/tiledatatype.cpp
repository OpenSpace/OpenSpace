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

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/misc/assert.h>


#include <modules/globebrowsing/tile/tiledatatype.h>


#include <modules/globebrowsing/geometry/angle.h>

#include <float.h>

#include <sstream>
#include <algorithm>





namespace {
    const std::string _loggerCat = "TileIOResult";
}



namespace openspace {
    float TileDataType::interpretFloat(GDALDataType gdalType, const char* src) {
        switch (gdalType) {
        case GDT_Byte:      return static_cast<float>(*reinterpret_cast<const GLubyte*>(src));
        case GDT_UInt16:    return static_cast<float>(*reinterpret_cast<const GLushort*>(src));
        case GDT_Int16:     return static_cast<float>(*reinterpret_cast<const GLshort*>(src));
        case GDT_UInt32:    return static_cast<float>(*reinterpret_cast<const GLuint*>(src));
        case GDT_Int32:     return static_cast<float>(*reinterpret_cast<const GLint*>(src));
        case GDT_Float32:   return static_cast<float>(*reinterpret_cast<const GLfloat*>(src));
        case GDT_Float64:   return static_cast<float>(*reinterpret_cast<const GLdouble*>(src));
        default:
            LERROR("Unknown data type");
            ghoul_assert(false, "Unknown data type");
            return -1.0;
        }
    }



    size_t TileDataType::numberOfBytes(GDALDataType gdalType) {
        switch (gdalType) {
        case GDT_Byte: return sizeof(GLubyte);
        case GDT_UInt16: return sizeof(GLushort);
        case GDT_Int16: return sizeof(GLshort);
        case GDT_UInt32: return sizeof(GLuint);
        case GDT_Int32: return sizeof(GLint);
        case GDT_Float32: return sizeof(GLfloat);
        case GDT_Float64: return sizeof(GLdouble);
        default:
            LERROR("Unknown data type");
            ghoul_assert(false, "Unknown data type");
            return -1;
        }
    }

    size_t TileDataType::getMaximumValue(GDALDataType gdalType) {
        switch (gdalType) {
        case GDT_Byte: return 1 << 8;
        case GDT_UInt16: return 1 << 16;
        case GDT_Int16: return 1 << 15;
        case GDT_UInt32: return 1 << 32;
        case GDT_Int32: return 1 << 31;
        default:
            LERROR("Unknown data type");
            return -1;
        }
    }


    TextureFormat TileDataType::getTextureFormat(
        int rasterCount, GDALDataType gdalType)
    {
        TextureFormat format;

        switch (rasterCount) {
        case 1: // Red
            format.ghoulFormat = Texture::Format::Red;
            switch (gdalType) {
            case GDT_Byte:      format.glFormat = GL_R8; break;
            case GDT_UInt16:    format.glFormat = GL_R16UI; break;
            case GDT_Int16:     format.glFormat = GL_R16_SNORM; break;
            case GDT_UInt32:    format.glFormat = GL_R32UI; break;
            case GDT_Int32:     format.glFormat = GL_R32I; break;
            case GDT_Float32:   format.glFormat = GL_R32F; break;
                //case GDT_Float64:   format.glFormat = GL_RED; break; // No representation of 64 bit float?
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 2:
            format.ghoulFormat = Texture::Format::RG;
            switch (gdalType) {
            case GDT_Byte: format.glFormat = GL_RG8; break;
            case GDT_UInt16: format.glFormat = GL_RG16UI; break;
            case GDT_Int16: format.glFormat = GL_RG16_SNORM; break;
            case GDT_UInt32: format.glFormat = GL_RG32UI; break;
            case GDT_Int32: format.glFormat = GL_RG32I; break;
            case GDT_Float32: format.glFormat = GL_RG32F; break;
            case GDT_Float64: format.glFormat = GL_RED; break; // No representation of 64 bit float?
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 3:
            format.ghoulFormat = Texture::Format::RGB;
            switch (gdalType) {
            case GDT_Byte: format.glFormat = GL_RGB8; break;
            case GDT_UInt16: format.glFormat = GL_RGB16UI; break;
            case GDT_Int16: format.glFormat = GL_RGB16_SNORM; break;
            case GDT_UInt32: format.glFormat = GL_RGB32UI; break;
            case GDT_Int32: format.glFormat = GL_RGB32I; break;
            case GDT_Float32: format.glFormat = GL_RGB32F; break;
                // case GDT_Float64: format.glFormat = GL_RED; break;// No representation of 64 bit float? 
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        case 4:
            format.ghoulFormat = Texture::Format::RGBA;
            switch (gdalType) {
            case GDT_Byte: format.glFormat = GL_RGBA8; break;
            case GDT_UInt16: format.glFormat = GL_RGBA16UI; break;
            case GDT_Int16: format.glFormat = GL_RGB16_SNORM; break;
            case GDT_UInt32: format.glFormat = GL_RGBA32UI; break;
            case GDT_Int32: format.glFormat = GL_RGBA32I; break;
            case GDT_Float32: format.glFormat = GL_RGBA32F; break;
            case GDT_Float64: format.glFormat = GL_RED; break; // No representation of 64 bit float?
            default: LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            }
            break;
        default:
            LERROR("Unknown number of channels for OpenGL texture: " << rasterCount);
            break;
        }
        return format;
    }






    GLuint TileDataType::getOpenGLDataType(GDALDataType gdalType) {
        switch (gdalType) {
        case GDT_Byte: return GL_UNSIGNED_BYTE;
        case GDT_UInt16: return GL_UNSIGNED_SHORT;
        case GDT_Int16: return GL_SHORT;
        case GDT_UInt32: return GL_UNSIGNED_INT;
        case GDT_Int32: return GL_INT;
        case GDT_Float32: return GL_FLOAT;
        case GDT_Float64: return GL_DOUBLE;
        default:
            LERROR("GDAL data type unknown to OpenGL: " << gdalType);
            return GL_UNSIGNED_BYTE;
        }
    }

    GDALDataType TileDataType::getGdalDataType(GLuint glType) {
        switch (glType) {
        case GL_UNSIGNED_BYTE: return GDT_Byte;
        case GL_UNSIGNED_SHORT: return GDT_UInt16;
        case GL_SHORT: return GDT_Int16;
        case GL_UNSIGNED_INT: return GDT_UInt32;
        case GL_INT: return GDT_Int32;
        case GL_FLOAT: return GDT_Float32;
        case GL_DOUBLE: return GDT_Float64;
        default:
            LERROR("OpenGL data type unknown to GDAL: " << glType);
            return GDT_Unknown;
        }
    }

}  // namespace openspace
