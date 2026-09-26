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

#include <ghoul/opengl/programobject.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <glm/gtc/type_ptr.hpp>
#include <algorithm>
#include <utility>

namespace ghoul::opengl {

ProgramObject::ProgramObjectError::ProgramObjectError(std::string msg)
    : RuntimeError(std::move(msg), "ProgramObject")
{}

ProgramObject::ProgramObjectLinkingError::ProgramObjectLinkingError(std::string msg,
                                                                    std::string name)
    : ProgramObjectError(
        name.empty() ?
        std::format("Error linking program object: {}", msg) :
        std::format("Error linking program object [{}]: {}", name, msg)
      )
    , linkerError(std::move(msg))
    , programName(std::move(name))
{}

ProgramObject::ProgramObject()
    : _id(glCreateProgram())
{
    if (_id == 0) {
        throw ProgramObjectError("glCreateProgram returned 0");
    }
}

ProgramObject::ProgramObject(std::string name)
    : _id(glCreateProgram())
    , _programName(std::move(name))
    , _loggerCat(std::format("ProgramObject('{}')", _programName))
{
    if (_id == 0) {
        throw ProgramObjectError("glCreateProgram returned 0");
    }
    glObjectLabel(
        GL_PROGRAM,
        _id,
        static_cast<GLsizei>(_programName.length() + 1),
        _programName.c_str()
    );
}

ProgramObject::~ProgramObject() {
    glDeleteProgram(_id);
}

ProgramObject::operator GLuint() const {
    return _id;
}

const std::string& ProgramObject::name() const{
    return _programName;
}

void ProgramObject::setDictionary(const Dictionary& dictionary) {
    for (const std::shared_ptr<ShaderObject>& shaderObject : _shaderObjects) {
        shaderObject->setDictionary(dictionary);
    }
}

Dictionary ProgramObject::dictionary() const {
    if (_shaderObjects.empty()) {
        throw ProgramObjectError("No shader object attached");
    }
    return _shaderObjects[0]->dictionary();
}

void ProgramObject::setProgramObjectCallback(ProgramObjectCallback changeCallback) {
    const ShaderObject::ShaderObjectCallback c = [this, changeCallback](){
        _programIsDirty = true;
        changeCallback(this);
    };
    for (const std::shared_ptr<ShaderObject>& shaderObject : _shaderObjects) {
        shaderObject->setShaderObjectCallback(c);
    }
}

void ProgramObject::attachObject(std::shared_ptr<ShaderObject> shaderObject) {
    ghoul_assert(shaderObject, "ShaderObject must not be nullptr");
    auto it = std::find(_shaderObjects.cbegin(), _shaderObjects.cend(), shaderObject);
    ghoul_assert(it == _shaderObjects.cend(), "ShaderObject was already registered");

    shaderObject->setShaderObjectCallback([this]() { _programIsDirty = true; });

    glAttachShader(_id, *shaderObject);
    _shaderObjects.push_back(std::move(shaderObject));
}

void ProgramObject::detachObject(const std::shared_ptr<ShaderObject>& shaderObject) {
    ghoul_assert(shaderObject, "ShaderObject must not be nullptr");
    auto it = std::find(_shaderObjects.begin(), _shaderObjects.end(), shaderObject);
    ghoul_assert(it != _shaderObjects.end(), "ShaderObject must have been registered");

    glDetachShader(_id, *shaderObject);
    _shaderObjects.erase(it);
}

void ProgramObject::compileShaderObjects() {
    for (const std::shared_ptr<ShaderObject>& obj : _shaderObjects) {
        obj->compile();
    }
}

void ProgramObject::linkProgramObject() {
    glLinkProgram(_id);

    GLint linkStatus = 0;
    glGetProgramiv(_id, GL_LINK_STATUS, &linkStatus);
    if (static_cast<GLboolean>(linkStatus) == GL_FALSE) {
        GLint logLength = 0;
        glGetProgramiv(_id, GL_INFO_LOG_LENGTH, &logLength);

        if (logLength == 0) {
            throw ProgramObjectLinkingError("Unknown error", name());
        }

        std::vector<GLchar> rawLog(logLength);
        glGetProgramInfoLog(_id, logLength, nullptr, rawLog.data());
        const std::string log = std::string(rawLog.data());
        throw ProgramObjectLinkingError(log, name());
    }

    GLint logLength = 0;
    glGetProgramiv(_id, GL_INFO_LOG_LENGTH, &logLength);
    if (logLength > 0) {
        std::vector<GLchar> rawLog(logLength);
        glGetProgramInfoLog(_id, logLength, nullptr, rawLog.data());
        const std::string log = std::string(rawLog.data());
        if (!log.empty()) {
            LWARNING(log);
        }
    }

    _programIsDirty = false;
}

void ProgramObject::rebuildFromFile() {
    for (std::shared_ptr<ShaderObject>& shader : _shaderObjects) {
        shader->rebuildFromFile();
    }

    compileShaderObjects();
    linkProgramObject();

    LINFO("Successfully rebuilt ProgramObject");
}

void ProgramObject::validate() const {
    glValidateProgram(_id);

    GLint success = 0;
    glGetProgramiv(_id, GL_VALIDATE_STATUS, &success);

    GLint logLength = 0;
    glGetProgramiv(_id, GL_INFO_LOG_LENGTH, &logLength);

    if (success) {
        if (logLength > 0) {
            std::vector<GLchar> rawLog(logLength);
            glGetProgramInfoLog(_id, logLength, nullptr, rawLog.data());
            const std::string log = std::string(rawLog.data());
            LINFO(log);
        }
    }
    else {
        if (logLength > 0) {
            std::vector<GLchar> rawLog(logLength);
            glGetProgramInfoLog(_id, logLength, nullptr, rawLog.data());
            const std::string log = std::string(rawLog.data());
            LERROR(log);
        }
        else {
            LERROR("Unknown error validating program");
        }
    }
}

bool ProgramObject::isDirty() const {
    return _programIsDirty;
}

void ProgramObject::activate() const {
    glUseProgram(_id);
}

void ProgramObject::deactivate() const {
    glUseProgram(0);
}

std::unique_ptr<ProgramObject> ProgramObject::Build(const std::string& name,
                                            const std::filesystem::path& vertexShaderPath,
                                          const std::filesystem::path& fragmentShaderPath,
                                                             const Dictionary& dictionary)
{
    ghoul_assert(!vertexShaderPath.empty(), "VertexShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(vertexShaderPath),
        "VertexShaderPath file must exist"
    );
    ghoul_assert(!fragmentShaderPath.empty(), "FragmentShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(fragmentShaderPath),
        "FragmentShaderPath file must exist"
    );

    auto program = std::make_unique<ProgramObject>(name);
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Vertex,
        vertexShaderPath,
        name + " Vertex",
        dictionary
    ));
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Fragment,
        fragmentShaderPath,
        name + " Fragment",
        dictionary
    ));

    program->compileShaderObjects();
    program->linkProgramObject();
    return program;
}

std::unique_ptr<ProgramObject> ProgramObject::Build(const std::string& name,
                                            const std::filesystem::path& vertexShaderPath,
                                          const std::filesystem::path& fragmentShaderPath,
                                          const std::filesystem::path& geometryShaderPath,
                                                             const Dictionary& dictionary)
{
    ghoul_assert(!vertexShaderPath.empty(), "VertexShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(vertexShaderPath),
        "VertexShaderPath file must exist"
    );
    ghoul_assert(!fragmentShaderPath.empty(), "FragmentShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(fragmentShaderPath),
        "FragmentShaderPath file must exist"
    );
    ghoul_assert(!geometryShaderPath.empty(), "GeometryShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(geometryShaderPath),
        "GeometryShaderPath file must exist"
    );

    auto program = std::make_unique<ProgramObject>(name);
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Vertex,
        vertexShaderPath,
        name + " Vertex",
        dictionary
    ));
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Geometry,
        geometryShaderPath,
        name + " Geometry",
        dictionary
    ));
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Fragment,
        fragmentShaderPath,
        name + " Fragment",
        dictionary
    ));

    program->compileShaderObjects();
    program->linkProgramObject();
    return program;
}

std::unique_ptr<ProgramObject> ProgramObject::Build(const std::string& name,
                                            const std::filesystem::path& vertexShaderPath,
                                          const std::filesystem::path& fragmentShaderPath,
                                          const std::filesystem::path& geometryShaderPath,
                            const std::filesystem::path& tessellationEvaluationShaderPath,
                               const std::filesystem::path& tessellationControlShaderPath,
                                                             const Dictionary& dictionary)
{
    ghoul_assert(!vertexShaderPath.empty(), "VertexShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(vertexShaderPath),
        "VertexShaderPath file must exist"
    );
    ghoul_assert(!fragmentShaderPath.empty(), "FragmentShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(fragmentShaderPath),
        "FragmentShaderPath file must exist"
    );
    ghoul_assert(!geometryShaderPath.empty(), "GeometryShaderPath must not be empty");
    ghoul_assert(
        std::filesystem::is_regular_file(geometryShaderPath),
        "GeometryShaderPath file must exist"
    );
    ghoul_assert(
        !tessellationEvaluationShaderPath.empty(),
        "Tessellation evaluation shader must not be empty"
    );
    ghoul_assert(
        std::filesystem::is_regular_file(tessellationEvaluationShaderPath),
        "Tessellation evaluation shader file must exist"
    );
    ghoul_assert(
        !tessellationControlShaderPath.empty(),
        "Tessellation control shader must not be empty"
    );
    ghoul_assert(
        std::filesystem::is_regular_file(tessellationControlShaderPath),
        "Tessellation control shader file must exist"
    );

    auto program = std::make_unique<ProgramObject>(name);
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Vertex,
        vertexShaderPath,
        name + " Vertex",
        dictionary
    ));
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Geometry,
        geometryShaderPath,
        name + " Geometry",
        dictionary
    ));
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::TesselationEvaluation,
        tessellationEvaluationShaderPath,
        name + " Tessellation Evaluation",
        dictionary
    ));
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::TesselationControl,
        tessellationControlShaderPath,
        name + " Tessellation Control",
        dictionary
    ));
    program->attachObject(std::make_unique<ShaderObject>(
        ShaderObject::ShaderType::Fragment,
        fragmentShaderPath,
        name + " Fragment",
        dictionary
    ));

    program->compileShaderObjects();
    program->linkProgramObject();
    return program;
}

void ProgramObject::setIgnoreUniformLocationError(IgnoreError ignoreError) {
    _ignoreUniformLocationError = ignoreError;
}

bool ProgramObject::ignoreUniformLocationError() const {
    return _ignoreUniformLocationError;
}

GLint ProgramObject::uniformLocation(const std::string& name) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = glGetUniformLocation(_id, name.c_str());
    if (!_ignoreUniformLocationError && location == -1) {
        LWARNING(std::format("Failed to locate uniform location for '{}'", name));
    }
    return location;
}

void ProgramObject::setUniform(const std::string& name, GLuint value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, GLuint v1, GLuint v2) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2);
    }
}

void ProgramObject::setUniform(const std::string& name, GLuint v1, GLuint v2,
                               GLuint v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3);
    }
}

void ProgramObject::setUniform(const std::string& name, GLuint v1, GLuint v2, GLuint v3,
                               GLuint v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::uvec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::uvec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::uvec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<GLuint>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::uvec2>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::uvec3>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::uvec4>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name, GLint value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, GLint v1, GLint v2) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2);
    }
}

void ProgramObject::setUniform(const std::string& name, GLint v1, GLint v2,
                               GLint v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3);
    }
}

void ProgramObject::setUniform(const std::string& name, GLint v1, GLint v2, GLint v3,
                               GLint v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::ivec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::ivec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::ivec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<GLint>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::ivec2>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::ivec3>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::ivec4>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name, GLfloat value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, GLfloat v1, GLfloat v2) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2);
    }
}

void ProgramObject::setUniform(const std::string& name, GLfloat v1, GLfloat v2,
                               GLfloat v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3);
    }
}

void ProgramObject::setUniform(const std::string& name, GLfloat v1, GLfloat v2,
                               GLfloat v3, GLfloat v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::vec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::vec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::vec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<GLfloat>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::vec2>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::vec3>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::vec4>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name, GLdouble value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, GLdouble v1, GLdouble v2) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2);
    }
}

void ProgramObject::setUniform(const std::string& name, GLdouble v1, GLdouble v2,
                               GLdouble v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3);
    }
}

void ProgramObject::setUniform(const std::string& name, GLdouble v1, GLdouble v2,
                               GLdouble v3, GLdouble v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dvec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dvec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dvec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<GLdouble>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::dvec2>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::dvec3>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name,
                               const std::vector<glm::dvec4>& values) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, values);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat2x2& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat2x3& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat2x4& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat3x2& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat3x3& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat3x4& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat4x2& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat4x3& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::mat4x4& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat2x2& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat2x3& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat2x4& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat3x2& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat3x3& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat3x4& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat4x2& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat4x3& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(const std::string& name, const glm::dmat4x4& value,
                               Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = uniformLocation(name);
    if (location != -1) {
        setUniform(location, value, transpose);
    }
}

void ProgramObject::setUniform(GLint location, GLuint value) const {
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform1ui(_id, location, value);
}

void ProgramObject::setUniform(GLint location, GLuint v1, GLuint v2) const {
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform2ui(_id, location, v1, v2);
}

void ProgramObject::setUniform(GLint location, GLuint v1, GLuint v2, GLuint v3) const {
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform3ui(_id, location, v1, v2, v3);
}

void ProgramObject::setUniform(GLint location, GLuint v1, GLuint v2, GLuint v3,
                               GLuint v4) const
{
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform4ui(_id, location, v1, v2, v3, v4);
}

void ProgramObject::setUniform(GLint location, const glm::uvec2& value) const {
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform2uiv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::uvec3& value) const {
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform3uiv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::uvec4& value) const {
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform4uiv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const std::vector<GLuint>& values) const {
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform1uiv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        values.data()
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::uvec2>& values) const
{
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform2uiv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const unsigned int*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::uvec3>& values) const
{
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform3uiv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const unsigned int*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::uvec4>& values) const
{
    ghoul_assert(location != -1, "Location must not be -1");
    glProgramUniform4uiv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const unsigned int*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location, GLint value) const {
    glProgramUniform1i(_id, location, value);
}

void ProgramObject::setUniform(GLint location, GLint v1, GLint v2) const {
    glProgramUniform2i(_id, location, v1, v2);
}

void ProgramObject::setUniform(GLint location, GLint v1, GLint v2, GLint v3) const {
    glProgramUniform3i(_id, location, v1, v2, v3);
}

void ProgramObject::setUniform(GLint location, GLint v1, GLint v2, GLint v3,
                               GLint v4) const
{
    glProgramUniform4i(_id, location, v1, v2, v3, v4);
}

void ProgramObject::setUniform(GLint location, const glm::ivec2& value) const {
    glProgramUniform2iv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::ivec3& value) const {
    glProgramUniform3iv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::ivec4& value) const {
    glProgramUniform4iv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const std::vector<GLint>& values) const {
    glProgramUniform1iv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        values.data()
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::ivec2>& values) const
{
    glProgramUniform2iv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const int*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::ivec3>& values) const
{
    glProgramUniform3iv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const int*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::ivec4>& values) const
{
    glProgramUniform4iv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const int*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location, GLfloat value) const {
    glProgramUniform1f(_id, location, value);
}

void ProgramObject::setUniform(GLint location, GLfloat v1, GLfloat v2) const {
    glProgramUniform2f(_id, location, v1, v2);
}

void ProgramObject::setUniform(GLint location, GLfloat v1, GLfloat v2, GLfloat v3) const {
    glProgramUniform3f(_id, location, v1, v2, v3);
}

void ProgramObject::setUniform(GLint location, GLfloat v1, GLfloat v2, GLfloat v3,
                               GLfloat v4) const
{
    glProgramUniform4f(_id, location, v1, v2, v3, v4);
}

void ProgramObject::setUniform(GLint location, const glm::vec2& value) const {
    glProgramUniform2fv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::vec3& value) const {
    glProgramUniform3fv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::vec4& value) const {
    glProgramUniform4fv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const std::vector<GLfloat>& values) const {
    glProgramUniform1fv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        values.data()
    );
}

void ProgramObject::setUniform(GLint location, const std::vector<glm::vec2>& values) const
{
    glProgramUniform2fv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const float*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location, const std::vector<glm::vec3>& values) const
{
    glProgramUniform3fv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const float*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location, const std::vector<glm::vec4>& values) const
{
    glProgramUniform4fv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const float*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location, GLdouble value) const {
    glProgramUniform1d(_id, location, value);
}

void ProgramObject::setUniform(GLint location, GLdouble v1, GLdouble v2) const {
    glProgramUniform2d(_id, location, v1, v2);
}

void ProgramObject::setUniform(GLint location, GLdouble v1, GLdouble v2,
                               GLdouble v3) const
{
    glProgramUniform3d(_id, location, v1, v2, v3);
}

void ProgramObject::setUniform(GLint location, GLdouble v1, GLdouble v2, GLdouble v3,
                               GLdouble v4) const
{
    glProgramUniform4d(_id, location, v1, v2, v3, v4);
}

void ProgramObject::setUniform(GLint location, const glm::dvec2& value) const {
    glProgramUniform2dv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::dvec3& value) const {
    glProgramUniform3dv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const glm::dvec4& value) const {
    glProgramUniform4dv(_id, location, 1, glm::value_ptr(value));
}

void ProgramObject::setUniform(GLint location, const std::vector<GLdouble>& values) const
{
    glProgramUniform1dv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        values.data()
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::dvec2>& values) const
{
    glProgramUniform2dv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const double*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::dvec3>& values) const
{
    glProgramUniform3dv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const double*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location,
                               const std::vector<glm::dvec4>& values) const
{
    glProgramUniform4dv(
        _id,
        location,
        static_cast<GLsizei>(values.size()),
        reinterpret_cast<const double*>(values.data())
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat2x2& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix2fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat2x3& value,
                               Transpose transpose) const
{

    glProgramUniformMatrix2x3fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat2x4& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix2x4fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat3x2& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix3x2fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat3x3& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix3fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat3x4& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix3x4fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat4x2& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix4x2fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat4x3& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix4x3fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::mat4x4& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix4fv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat2x2& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix2dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat2x3& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix2x3dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat2x4& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix2x4dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat3x2& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix3x2dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat3x3& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix3dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat3x4& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix3x4dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat4x2& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix4x2dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat4x3& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix4x3dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

void ProgramObject::setUniform(GLint location, const glm::dmat4x4& value,
                               Transpose transpose) const
{
    glProgramUniformMatrix4dv(
        _id,
        location,
        1,
        transpose ? GL_TRUE : GL_FALSE,
        value_ptr(value)
    );
}

bool ProgramObject::setSsboBinding(std::string_view name, GLuint binding) const {
    const GLuint index = glGetProgramResourceIndex(
        _id,
        GL_SHADER_STORAGE_BLOCK,
        name.data()
    );
    if (index == GL_INVALID_INDEX) {
        return false;
    }
    setSsboBinding(index, binding);
    return true;
}

void ProgramObject::setSsboBinding(GLuint index, GLuint binding) const {
    glShaderStorageBlockBinding(_id, index, binding);
}

GLuint ProgramObject::attributeLocation(const std::string& name) const {
    const GLint location = glGetAttribLocation(_id, name.data());
    if (!_ignoreAttributeLocationError && location == -1) {
        LWARNING(std::format("Failed to locate attribute location for '{}'", name));
        return GL_INVALID_INDEX;
    }
    return static_cast<GLuint>(location);
}

void ProgramObject::bindAttributeLocation(const std::string& name, GLuint index) const {
    glBindAttribLocation(_id, index, name.data());
}

bool ProgramObject::ignoreAttributeLocationError() const {
    return _ignoreAttributeLocationError;
}

void ProgramObject::setIgnoreAttributeLocationError(IgnoreError ignoreError) {
    _ignoreAttributeLocationError = ignoreError;
}

void ProgramObject::setAttribute(const std::string& name, bool value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, bool v1, bool v2) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2);
    }
}

void ProgramObject::setAttribute(const std::string& name, bool v1, bool v2,
                                 bool v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3);
    }
}

void ProgramObject::setAttribute(const std::string& name, bool v1, bool v2, bool v3,
                                 bool v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::bvec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::bvec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::bvec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLint value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLint v1, GLint v2) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLint v1, GLint v2,
                                 GLint v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLint v1, GLint v2, GLint v3,
                                 GLint v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::ivec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::ivec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::ivec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLfloat value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLfloat v1, GLfloat v2) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLfloat v1, GLfloat v2,
                                 GLfloat v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLfloat v1, GLfloat v2,
                                 GLfloat v3, GLfloat v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::vec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::vec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::vec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLdouble value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLdouble v1, GLdouble v2) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLdouble v1, GLdouble v2,
                                 GLdouble v3) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3);
    }
}

void ProgramObject::setAttribute(const std::string& name, GLdouble v1, GLdouble v2,
                                 GLdouble v3, GLdouble v4) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, v1, v2, v3, v4);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dvec2& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dvec3& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dvec4& value) const {
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat2x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat2x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat2x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat3x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat3x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat3x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat4x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat4x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::mat4x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat2x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat2x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat2x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat3x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat3x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat3x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat4x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat4x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location == GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(const std::string& name, const glm::dmat4x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint location = attributeLocation(name);
    if (location != GL_INVALID_INDEX) {
        setAttribute(location, value, transpose);
    }
}

void ProgramObject::setAttribute(GLuint location, bool value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI1i(location, value);
}

void ProgramObject::setAttribute(GLuint location, bool v1, bool v2) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI2i(location, v1, v2);
}

void ProgramObject::setAttribute(GLuint location, bool v1, bool v2, bool v3) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI3i(location, v1, v2, v3);
}

void ProgramObject::setAttribute(GLuint location, bool v1, bool v2, bool v3,
                                 bool v4) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI4i(location, v1, v2, v3, v4);
}

void ProgramObject::setAttribute(GLuint location, const glm::bvec2& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI2iv(location, glm::value_ptr(glm::ivec2(value)));
}

void ProgramObject::setAttribute(GLuint location, const glm::bvec3& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI3iv(location, glm::value_ptr(glm::ivec3(value)));
}

void ProgramObject::setAttribute(GLuint location, const glm::bvec4& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI4iv(location, glm::value_ptr(glm::ivec4(value)));
}

void ProgramObject::setAttribute(GLuint location, GLint value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI1i(location, value);
}

void ProgramObject::setAttribute(GLuint location, GLint v1, GLint v2) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI2i(location, v1, v2);
}

void ProgramObject::setAttribute(GLuint location, GLint v1, GLint v2, GLint v3) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI3i(location, v1, v2, v3);
}

void ProgramObject::setAttribute(GLuint location, GLint v1, GLint v2, GLint v3,
                                 GLint v4) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI4i(location, v1, v2, v3, v4);
}

void ProgramObject::setAttribute(GLuint location, const glm::ivec2& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI2iv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, const glm::ivec3& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI3iv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, const glm::ivec4& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribI4iv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, GLfloat value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttrib1f(location, value);
}

void ProgramObject::setAttribute(GLuint location, GLfloat v1, GLfloat v2) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttrib2f(location, v1, v2);
}

void ProgramObject::setAttribute(GLuint location, GLfloat v1, GLfloat v2,
                                 GLfloat v3) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttrib3f(location, v1, v2, v3);
}

void ProgramObject::setAttribute(GLuint location, GLfloat v1, GLfloat v2, GLfloat v3,
                                 GLfloat v4) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttrib4f(location, v1, v2, v3, v4);
}

void ProgramObject::setAttribute(GLuint location, const glm::vec2& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttrib2fv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, const glm::vec3& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttrib3fv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, const glm::vec4& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttrib4fv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, GLdouble value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribL1d(location, value);
}

void ProgramObject::setAttribute(GLuint location, GLdouble v1, GLdouble v2) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribL2d(location, v1, v2);
}

void ProgramObject::setAttribute(GLuint location, GLdouble v1, GLdouble v2,
                                 GLdouble v3) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribL3d(location, v1, v2, v3);
}

void ProgramObject::setAttribute(GLuint location, GLdouble v1, GLdouble v2, GLdouble v3,
                                 GLdouble v4) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribL4d(location, v1, v2, v3, v4);
}

void ProgramObject::setAttribute(GLuint location, const glm::dvec2& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribL2dv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, const glm::dvec3& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribL3dv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, const glm::dvec4& value) const {
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    glVertexAttribL4dv(location, glm::value_ptr(value));
}

void ProgramObject::setAttribute(GLuint location, const glm::mat2x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib2fv(location, glm::value_ptr(value[0]));
        glVertexAttrib2fv(location + 1, glm::value_ptr(value[1]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat2x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib3fv(location, glm::value_ptr(value[0]));
        glVertexAttrib3fv(location + 1, glm::value_ptr(value[1]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat2x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib4fv(location, glm::value_ptr(value[0]));
        glVertexAttrib4fv(location + 1, glm::value_ptr(value[1]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat3x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib2fv(location, glm::value_ptr(value[0]));
        glVertexAttrib2fv(location + 1, glm::value_ptr(value[1]));
        glVertexAttrib2fv(location + 2, glm::value_ptr(value[2]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat3x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib3fv(location, glm::value_ptr(value[0]));
        glVertexAttrib3fv(location + 1, glm::value_ptr(value[1]));
        glVertexAttrib3fv(location + 2, glm::value_ptr(value[2]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat3x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib4fv(location, glm::value_ptr(value[0]));
        glVertexAttrib4fv(location + 1, glm::value_ptr(value[1]));
        glVertexAttrib4fv(location + 2, glm::value_ptr(value[2]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat4x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib2fv(location, glm::value_ptr(value[0]));
        glVertexAttrib2fv(location + 1, glm::value_ptr(value[1]));
        glVertexAttrib2fv(location + 2, glm::value_ptr(value[2]));
        glVertexAttrib2fv(location + 3, glm::value_ptr(value[3]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat4x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib3fv(location, glm::value_ptr(value[0]));
        glVertexAttrib3fv(location + 1, glm::value_ptr(value[1]));
        glVertexAttrib3fv(location + 2, glm::value_ptr(value[2]));
        glVertexAttrib3fv(location + 3, glm::value_ptr(value[3]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::mat4x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttrib4fv(location, glm::value_ptr(value[0]));
        glVertexAttrib4fv(location + 1, glm::value_ptr(value[1]));
        glVertexAttrib4fv(location + 2, glm::value_ptr(value[2]));
        glVertexAttrib4fv(location + 3, glm::value_ptr(value[3]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat2x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL2dv(location, glm::value_ptr(value[0]));
        glVertexAttribL2dv(location + 1, glm::value_ptr(value[1]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat2x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL3dv(location, glm::value_ptr(value[0]));
        glVertexAttribL3dv(location + 1, glm::value_ptr(value[1]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat2x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL4dv(location, glm::value_ptr(value[0]));
        glVertexAttribL4dv(location + 1, glm::value_ptr(value[1]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat3x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL2dv(location, glm::value_ptr(value[0]));
        glVertexAttribL2dv(location + 1, glm::value_ptr(value[1]));
        glVertexAttribL2dv(location + 2, glm::value_ptr(value[2]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat3x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL3dv(location, glm::value_ptr(value[0]));
        glVertexAttribL3dv(location + 1, glm::value_ptr(value[1]));
        glVertexAttribL3dv(location + 2, glm::value_ptr(value[2]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat3x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL4dv(location, glm::value_ptr(value[0]));
        glVertexAttribL4dv(location + 1, glm::value_ptr(value[1]));
        glVertexAttribL4dv(location + 2, glm::value_ptr(value[2]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat4x2& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL2dv(location, glm::value_ptr(value[0]));
        glVertexAttribL2dv(location + 1, glm::value_ptr(value[1]));
        glVertexAttribL2dv(location + 2, glm::value_ptr(value[2]));
        glVertexAttribL2dv(location + 3, glm::value_ptr(value[3]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat4x3& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL3dv(location, glm::value_ptr(value[0]));
        glVertexAttribL3dv(location + 1, glm::value_ptr(value[1]));
        glVertexAttribL3dv(location + 2, glm::value_ptr(value[2]));
        glVertexAttribL3dv(location + 3, glm::value_ptr(value[3]));
    }
}

void ProgramObject::setAttribute(GLuint location, const glm::dmat4x4& value,
                                 Transpose transpose) const
{
    ghoul_assert(location != GL_INVALID_INDEX, "Location must not be GL_INVALID_INDEX");
    if (transpose) {
        setAttribute(location, glm::transpose(value));
    }
    else {
        glVertexAttribL4dv(location, glm::value_ptr(value[0]));
        glVertexAttribL4dv(location + 1, glm::value_ptr(value[1]));
        glVertexAttribL4dv(location + 2, glm::value_ptr(value[2]));
        glVertexAttribL4dv(location + 3, glm::value_ptr(value[3]));
    }
}

void ProgramObject::setIgnoreSubroutineLocationError(IgnoreError ignoreError) {
    _ignoreSubroutineLocationError = ignoreError;
}

bool ProgramObject::ignoreSubroutineLocationError() const {
    return _ignoreSubroutineLocationError;
}

void ProgramObject::setIgnoreSubroutineUniformLocationError(IgnoreError ignoreError) {
    _ignoreSubroutineUniformLocationError = ignoreError;
}

bool ProgramObject::ignoreSubroutineUniformLocationError() const {
    return _ignoreSubroutineUniformLocationError;
}

GLuint ProgramObject::subroutineIndex(ShaderObject::ShaderType shaderType,
                                      const std::string& name)
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLuint index = glGetSubroutineIndex(
        _id,
        static_cast<GLenum>(shaderType),
        name.c_str()
    );
    if (!_ignoreSubroutineLocationError && index == GL_INVALID_INDEX) {
        LWARNING("Failed to locate subroutine index for: " + name);
    }
    return index;
}

GLint ProgramObject::subroutineUniformLocation(ShaderObject::ShaderType shaderType,
                                               const std::string& name) const
{
    ghoul_assert(!name.empty(), "Name must not be empty");

    const GLint location = glGetSubroutineUniformLocation(
        _id,
        static_cast<GLenum>(shaderType),
        name.c_str()
    );
    if (!_ignoreSubroutineUniformLocationError && location == -1) {
        LWARNING("Failed to locate subroutine uniform location for: " + name);
    }
    return location;
}

std::vector<std::string> ProgramObject::activeSubroutineUniformNames(
                                                ShaderObject::ShaderType shaderType) const
{
    GLint maximumUniformNameLength = 0;
    glGetProgramStageiv(
        _id,
        static_cast<GLenum>(shaderType),
        GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH,
        &maximumUniformNameLength
    );

    int countActiveSubroutineUniforms = 0;
    std::vector<char> buffer(maximumUniformNameLength);
    glGetProgramStageiv(
        _id,
        static_cast<GLenum>(shaderType),
        GL_ACTIVE_SUBROUTINE_UNIFORMS,
        &countActiveSubroutineUniforms
    );
    std::vector<std::string> result(countActiveSubroutineUniforms);
    for (GLuint i = 0; i < static_cast<GLuint>(countActiveSubroutineUniforms); i++) {
        glGetActiveSubroutineUniformName(
            _id,
            static_cast<GLenum>(shaderType),
            i,
            maximumUniformNameLength,
            nullptr,
            buffer.data()
        );
        result[i] = std::string(buffer.data());
    }

    return result;
}

std::vector<std::string> ProgramObject::compatibleSubroutineNames(
                                                      ShaderObject::ShaderType shaderType,
                                                   GLuint subroutineUniformLocation) const
{
    ghoul_assert(
        subroutineUniformLocation != GL_INVALID_INDEX,
        "Location must not be GL_INVALID_INDEX"
    );

    GLint maximumUniformNameLength = 0;
    glGetProgramStageiv(
        _id,
        static_cast<GLenum>(shaderType),
        GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH,
        &maximumUniformNameLength
    );

    GLint numCompatibleSubroutines = 0;
    glGetActiveSubroutineUniformiv(
        _id,
        static_cast<GLenum>(shaderType),
        subroutineUniformLocation,
        GL_NUM_COMPATIBLE_SUBROUTINES,
        &numCompatibleSubroutines
    );
    if (numCompatibleSubroutines == 0) {
        return std::vector<std::string>();
    }

    std::vector<std::string> result = std::vector<std::string>(numCompatibleSubroutines);
    std::vector<GLint> indices = std::vector<GLint>(numCompatibleSubroutines);
    std::vector<char> buffer = std::vector<char>(maximumUniformNameLength);
    glGetActiveSubroutineUniformiv(
        _id,
        static_cast<GLenum>(shaderType),
        subroutineUniformLocation,
        GL_COMPATIBLE_SUBROUTINES,
        indices.data()
    );
    for (GLuint i = 0; i < static_cast<GLuint>(numCompatibleSubroutines); i++) {
        glGetActiveSubroutineName(
            _id,
            static_cast<GLenum>(shaderType),
            static_cast<GLuint>(indices[i]),
            maximumUniformNameLength,
            nullptr,
            buffer.data()
        );
        result[i] = std::string(buffer.data());
    }
    return result;
}

std::vector<std::string> ProgramObject::compatibleSubroutineNames(
                                                      ShaderObject::ShaderType shaderType,
                                           const std::string& subroutineUniformName) const
{
    ghoul_assert(!subroutineUniformName.empty(), "Name must not be empty");

    const GLint index = subroutineUniformLocation(shaderType, subroutineUniformName);
    if (index == -1) {
        return std::vector<std::string>();
    }
    else {
        return compatibleSubroutineNames(shaderType, static_cast<GLuint>(index));
    }
}

bool ProgramObject::setUniformSubroutines(ShaderObject::ShaderType shaderType,
                                          const std::vector<GLuint>& indices)
{
    ghoul_assert(!indices.empty(), "Values must not be empty");

#ifdef GHL_DEBUG
    int countActiveSubroutineUniforms = 0;
    glGetProgramStageiv(
        _id,
        static_cast<GLenum>(shaderType),
        GL_ACTIVE_SUBROUTINE_UNIFORMS,
        &countActiveSubroutineUniforms
    );
    if (static_cast<size_t>(countActiveSubroutineUniforms) != indices.size()) {
        LWARNING(std::format(
            "Number of active subroutine uniforms ({}) is different from passed uniform "
            "subroutine indices ({})",
            countActiveSubroutineUniforms, indices.size()
        ));
        return false;
    }
#endif // GHL_DEBUG
    glUniformSubroutinesuiv(
        static_cast<GLenum>(shaderType),
        static_cast<GLsizei>(indices.size()),
        indices.data()
    );
    return true;
}

bool ProgramObject::setUniformSubroutines(ShaderObject::ShaderType shaderType,
                                         const std::map<std::string, std::string>& values)
{
    ghoul_assert(!values.empty(), "Values must not be empty");

#ifdef GHL_DEBUG
    int countActiveSubroutineUniforms = 0;
    glGetProgramStageiv(
        _id,
        static_cast<GLenum>(shaderType),
        GL_ACTIVE_SUBROUTINE_UNIFORMS,
        &countActiveSubroutineUniforms
    );
    if (static_cast<size_t>(countActiveSubroutineUniforms) != values.size()) {
        LWARNING(std::format(
            "Number of active subroutine uniforms ({}) is different from passed uniform "
            "subroutine indices ({})",
            countActiveSubroutineUniforms,
            values.size()
        ));
        return false;
    }
#endif // GHL_DEBUG

    std::vector<GLuint> uniformIndices = std::vector<GLuint>(values.size());
    const std::vector<std::string>& uniformSubroutines = activeSubroutineUniformNames(
        shaderType
    );
    for (size_t i = 0; i < uniformSubroutines.size(); i++) {
        const std::string& uniformSubroutine = uniformSubroutines[i];
        auto subroutine = values.find(uniformSubroutine);
#ifdef GHL_DEBUG
        if (subroutine == values.end()) {
            LWARNING(std::format(
                "Uniform subroutine name '{}' was not present in map", uniformSubroutine
            ));
            return false;
        }
#endif // GHL_DEBUG
        const std::string& nameSubroutine = subroutine->second;
        const GLuint idxSubroutine = subroutineIndex(shaderType, nameSubroutine);
#ifdef GHL_DEBUG
        if (idxSubroutine == GL_INVALID_INDEX) {
            LWARNING(std::format(
                "Subroutine name '{}' was not found in shader object", nameSubroutine
            ));
            return false;
        }
#endif // GHL_DEBUG
        uniformIndices[i] = idxSubroutine;
    }
    glUniformSubroutinesuiv(
        static_cast<GLenum>(shaderType),
        static_cast<GLsizei>(uniformIndices.size()),
        uniformIndices.data()
    );
    return true;
}

void ProgramObject::bindFragDataLocation(const std::string& name, GLuint colorNumber) {
    ghoul_assert(!name.empty(), "Name must not be empty");
    ghoul_assert(
        colorNumber != GL_INVALID_INDEX,
        "Location must not be GL_INVALID_INDEX"
    );

#ifdef GHL_DEBUG
    GLint maxBuffers = 0;
    glGetIntegerv(GL_MAX_DRAW_BUFFERS, &maxBuffers);
    if (colorNumber >= static_cast<GLuint>(maxBuffers)) {
        LWARNING(std::format(
            "ColorNumber '{}' is bigger than the maximum of simultaneous outputs '{}'",
            colorNumber, maxBuffers
        ));
        return;
    }
#endif // GHL_DEBUG

    glBindFragDataLocation(_id, colorNumber, name.c_str());
}

} // namespace ghoul::opengl
