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

#ifndef __GHOUL___SHADEROBJECT___H__
#define __GHOUL___SHADEROBJECT___H__

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/shaderpreprocessor.h>
#include <filesystem>
#include <functional>
#include <string>
#include <string_view>
#include <type_traits>

namespace ghoul::filesystem { class File; }

namespace ghoul::opengl {

/**
 * This class is a wrapper for an OpenGL shader object. It represents a single shader
 * object of the types declared in ShaderObject::ShaderType. All ShaderObject%s must be
 * loaded from a file on disk. The shader will load the contents of the file in the
 * constructor (if a constructor with a filename was chosen) or in the setFilename method.
 * If the file changes after it has been loaded, the ShaderObject will not change unless
 * rebuildFromFile is called, which pulls the changes. Each object can have a name that
 * will be used for a debug label (if available) and for the logging. The type of the
 * ShaderObject is selected in the constructor and cannot be changed afterwards. To use
 * the ShaderObjects, they have to be attached to a ProgramObject first.
 */
class ShaderObject {
public:
    /**
     * Main exception that is thrown by methods of the ShaderObject class.
     */
    struct ShaderObjectError : public RuntimeError {
        explicit ShaderObjectError(std::string msg);
    };

    /**
     * The exception that is thrown if the compilation of a ShaderObject failed.
     */
    struct ShaderCompileError final : public ShaderObjectError {
        /**
         * The constructor constructing a ShaderCompileError containing the cause for the
         * error (\p error) as well as the, optional, \p ident and \p name.
         */
        explicit ShaderCompileError(std::string error, std::string ident,
            std::string name);

        /// The compile error as reported by the GLSL compiler
        const std::string compileError;

        /// File identifiers of included files as reported by ShaderPreprocessor
        const std::string fileIdentifiers;

        /// The name of the ShaderObject that caused the compile error
        const std::string shaderName;
    };

    /**
     * An enum of the different types of shaders that can be used in OpenGL. They can be
     * used interchangeably in native OpenGL calls, too.
     */
    enum class ShaderType : std::underlying_type_t<GLenum> {
        Vertex = static_cast<std::underlying_type_t<GLenum>>(GL_VERTEX_SHADER),
        TesselationControl = static_cast<std::underlying_type_t<GLenum>>(
            GL_TESS_CONTROL_SHADER
        ),
        TesselationEvaluation = static_cast<std::underlying_type_t<GLenum>>(
            GL_TESS_EVALUATION_SHADER
        ),
        Geometry = static_cast<std::underlying_type_t<GLenum>>(GL_GEOMETRY_SHADER),
        Fragment = static_cast<std::underlying_type_t<GLenum>>(GL_FRAGMENT_SHADER),
        Compute = static_cast<std::underlying_type_t<GLenum>>(GL_COMPUTE_SHADER)
    };


    /**
     * A type definition for a callback function that is called if any of
     * the tracked files is changed.
     */
    using ShaderObjectCallback = std::function<void()>;

    /**
     * This constructor creates a shader of the passed type and loads the shader source
     * from the provided filename if it is provided. If the filename is a valid file, its
     * contents will be used to initialize this ShaderObject. The internal name, debug
     * label and logging category will be set based on the user provided name if provided.
     *
     * \param shaderType The type of shader that this ShaderObject will represent
     * \param filename The name of the file that will be used to load the source of this
     *        shader
     * \param name The human readable name of this ShaderObject
     * \param dictionary The dictionary that is used for the ShaderPreprocessor
     *
     * \throw ShaderObjectError If no new OpenGL name for the ShaderObject could be
     *        generated
     * \pre \p filename must not be empty
     * \pre \p filename must be a file that exists
     */
    ShaderObject(ShaderType shaderType, const std::filesystem::path& filename = "",
        std::string name = "", Dictionary dictionary = Dictionary());

    /**
     * A move constructor that will move all of the internal state, and the shader source,
     * but it will leave the other object in an invalid state.
     */
    ShaderObject(ShaderObject&& rhs) noexcept;

    /**
     * The destructor will mark the OpenGL name as unused again. Because of the way the
     * OpenGL functions work, the name might still be in use if the ShaderObject is
     * attached to a ProgramObject prior to deleting.
     */
    ~ShaderObject();

    /**
     * The casting operator that enabled this ShaderObject to be used in native OpenGL
     * functions and it returns the OpenGL name.
     */
    operator GLuint() const;

    /**
     * Returns the internal name of this ShaderObject.
     *
     * \return The internal name of this ShaderObject
     */
    const std::string& name() const;

    /**
    * Returns the dictionary that will be used to compile the shader object.
    *
    * \return The dictionary
    */
    Dictionary dictionary() const;

    /**
     * Sets the dictionary of the shader object. Will trigger a rebuild from file.
     */
    void setDictionary(Dictionary dictionary);

    /**
     * Sets the shader object callback.
     */
    void setShaderObjectCallback(ShaderObjectCallback changeCallback);

    /**
     * Returns the filepath for the shader object file.
     *
     * \return The filename
     */
    std::filesystem::path filename() const;

    /**
     * Rebuild the shader object from file using the file set by setFilename and the
     * dictionary set by setDictionary.
     */
    void rebuildFromFile();

    /**
     * This method will mark the OpenGL name as unused. Because of the way the OpenGL
     * functions, the name might still be in use if the ShaderObject is attached to a
     * ProgramObject prior to deleting.
     */
    void deleteShader() const;

    /**
     * This method will compile the shader source in this ShaderObject and returns the
     * success of this operation.
     *
     * \throw ShaderCompileError If there was an error while compiling the ShaderObject
     */
    void compile();

    /**
     * Returns the type of this ShaderObject as a human readable string.
     *
     * \return The type of this ShaderObject as a human readable string
     */
    std::string_view typeAsString() const;

    /**
     * Returns the ShaderObject `type` as a human readable string.
     *
     * \return The ShaderObject `type` as a human readable string
     */
    static std::string_view stringForShaderType(ShaderType type);

private:
    /// The OpenGL name of this ShaderObject
    const GLuint _id = 0;

    /// The type of this ShaderObject
    const ShaderType _type;

    /// The internal name of this ShaderObject; initialized to empty
    const std::string _shaderName;

    /// The logger category that will be used for logging of ShaderObject methods
    const std::string _loggerCat;

    /// The callback function if any of the tracked files are changed
    ShaderObjectCallback _onChangeCallback = nullptr;

    /// The preprocessor to process the shader file and track changes
    ShaderPreprocessor _preprocessor;
};

} // namespace ghoul::opengl

#endif // __GHOUL___SHADEROBJECT___H__
