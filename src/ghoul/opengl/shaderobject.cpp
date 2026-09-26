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

#include <ghoul/opengl/shaderobject.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <fstream>
#include <utility>

namespace ghoul::opengl {

ShaderObject::ShaderObjectError::ShaderObjectError(std::string msg)
    : RuntimeError(std::move(msg), "ShaderObject")
{}

ShaderObject::ShaderCompileError::ShaderCompileError(std::string error, std::string ident,
                                                     std::string name)
    : ShaderObjectError(
        name.empty() ?
        std::format("Error compiling shader object: {}\n{}", error, ident) :
        std::format("Error compiling shader object [{}]: {}\n{}", name, error, ident)
    )
    , compileError(std::move(error))
    , fileIdentifiers(std::move(ident))
    , shaderName(std::move(name))
{}

ShaderObject::ShaderObject(ShaderType shaderType, const std::filesystem::path& filename,
                           std::string name, Dictionary dictionary)
    : _id(glCreateShader(static_cast<GLenum>(shaderType)))
    , _type(shaderType)
    , _shaderName(std::move(name))
    , _loggerCat(
        _shaderName.empty() ?
            "ShaderObject" :
            std::format("ShaderObject('{}')", _shaderName)
    )
    , _preprocessor(filename, std::move(dictionary))
{
    if (_id == 0) {
        throw ShaderObjectError("glCreateShader returned 0");
    }

    if (!_shaderName.empty()) {
        glObjectLabel(
            GL_SHADER,
            _id,
            GLsizei(_shaderName.length() + 1),
            _shaderName.c_str()
        );
    }

    const bool hasFilename = !filename.empty();
    if (hasFilename) {
        rebuildFromFile();
    }
}

ShaderObject::ShaderObject(ShaderObject&& rhs) noexcept
    : _id(rhs._id)
    , _type(rhs._type)
    , _shaderName(rhs._shaderName)
    , _loggerCat(rhs._loggerCat)
    , _onChangeCallback(std::move(rhs._onChangeCallback))
    , _preprocessor(std::move(rhs._preprocessor))
{
    setShaderObjectCallback(rhs._onChangeCallback);
}

ShaderObject::~ShaderObject() {
    glDeleteShader(_id);
}

ShaderObject::operator GLuint() const {
    return _id;
}

const std::string& ShaderObject::name() const {
    return _shaderName;
}

void ShaderObject::setShaderObjectCallback(ShaderObjectCallback changeCallback) {
    _onChangeCallback = std::move(changeCallback);
    // The ShaderPreprocessor will take care to call the callback whenever the underlying
    // file changes, an included file changes, or the dictionary changes
    _preprocessor.setCallback(_onChangeCallback);
}

std::filesystem::path ShaderObject::filename() const {
    return _preprocessor.filename();
}

void ShaderObject::setDictionary(Dictionary dictionary) {
    _preprocessor.setDictionary(std::move(dictionary));
}

Dictionary ShaderObject::dictionary() const {
    return _preprocessor.dictionary();
}

void ShaderObject::rebuildFromFile() {
    std::string contents = _preprocessor.process();

    // If in debug mode, output the source to file
#ifdef GHL_DEBUG
    std::filesystem::path generatedFilename;

    std::filesystem::path base;
    if (_shaderName.empty()) {
        base = filename().stem();
    } else {
        base = _shaderName;
    }

    if (FileSys.cacheManager()) {
        // We use the .baseName() version because otherwise we get a new file every time
        // we reload the shader
        generatedFilename = FileSys.cacheManager()->cachedFilename(base, "");
    }
    else {
        // Either the cachemanager wasn't initialized or the filename could not be fetched
        generatedFilename += ".GhoulGenerated.glsl";
    }

    std::ofstream os;
    os.exceptions(std::ofstream::failbit | std::ofstream::badbit);
    os.open(generatedFilename);
    os << contents;
#endif // GHL_DEBUG

    const char* contentPtr = contents.c_str();
    glShaderSource(_id, 1, &contentPtr, nullptr);
}

void ShaderObject::deleteShader() const {
    glDeleteShader(_id);
}

void ShaderObject::compile() {
    glCompileShader(_id);

    GLint compilationStatus = 0;
    glGetShaderiv(_id, GL_COMPILE_STATUS, &compilationStatus);
    if (static_cast<GLboolean>(compilationStatus) == GL_FALSE) {
        GLint logLength = 0;
        glGetShaderiv(_id, GL_INFO_LOG_LENGTH, &logLength);

        if (logLength == 0) {
            throw ShaderCompileError(
                "Unknown error",
                _preprocessor.includedFiles(),
                name()
            );
        }

        std::vector<GLchar> log(logLength);
        glGetShaderInfoLog(_id, logLength, nullptr, log.data());
        const std::string logMessage = std::string(log.data());
        throw ShaderCompileError(logMessage, _preprocessor.includedFiles(), name());
    }
}

std::string_view ShaderObject::typeAsString() const {
    return ShaderObject::stringForShaderType(_type);
}

std::string_view ShaderObject::stringForShaderType(ShaderType type) {
    switch (type) {
        case ShaderType::Vertex:                return "Vertex shader";
        case ShaderType::TesselationControl:    return "Tesselation Control shader";
        case ShaderType::TesselationEvaluation: return "Tesselation Evaluation shader";
        case ShaderType::Geometry:              return "Geometry shader";
        case ShaderType::Fragment:              return "Fragment shader";
        case ShaderType::Compute:               return "Compute shader";
        default:                                throw MissingCaseException();
    }
}

} // namespace ghoul::opengl
