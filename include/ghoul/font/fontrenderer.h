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

#ifndef __GHOUL___FONTRENDERER___H__
#define __GHOUL___FONTRENDERER___H__

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>
#include <string_view>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }

namespace ghoul::fontrendering {

class Font;

/**
 * The FontRenderer is a class that can take a Font object and renderer it at a given
 * position. It has two separate usage modes; 1. it works as a singleton that has a
 * predefined vertex and fragment shader that perform rendering in a predetermined way.
 * 2. Instances of the FontRenderer can be instantiated that need a custom ProgramObject
 * that handles the rendering. As the rendering is performed in screen space, the
 * size of the rendering window in pixels has to be provided to the FontRenderer
 * (#setFramebufferSize) for a correct rendering.
 *
 * The main methods for rendering are the #render methods and their varieties.
 */
class FontRenderer {
public:
    /**
     * Information about the bounding box for a rendered text.
     */
    struct BoundingBoxInformation {
        glm::vec2 boundingBox = glm::vec2(0.f);
        int numberOfLines = 0;
    };

    /**
     * Information about the projected rendered text.
     */
    struct ProjectedLabelsInformation {
        bool enableDepth = false;
        bool enableFalseDepth = false;
        bool disableTransmittance = false;
        float scale = 0.f;
        int renderType = 0;
        int minSize = 0;
        int maxSize = 0;
        glm::dmat4 mvpMatrix = glm::dmat4(1.0);
        glm::dmat4 modelViewMatrix = glm::dmat4(1.0);
        glm::dmat4 projectionMatrix = glm::dmat4(1.0);
        glm::vec3 orthoRight = glm::vec3(0.f);
        glm::vec3 orthoUp = glm::vec3(0.f);
        glm::dvec3 cameraPos = glm::dvec3(0.0);
        glm::dvec3 cameraLookUp = glm::dvec3(0.0);
    };

    /**
     * This constructor requires a custom ProgramObject that handles the rendering of any
     * passed text. In addition the initial size of the rendering window has to be passed.
     * The inputs for the ProgramObject are as follows:
     *
     * The **vertex shader** recieves three `vec2` for each vertex: The `Position`
     * (location 0) in pixel screen space coordinates, the `Base Texture Coordinates`
     * (location 1) which provides the texture coordinates for the base font layer, and
     * the `Outline Texture Coordinates` (location 2) which provides the texture
     * coordinates for the outline font layer. Furhermore, the following uniforms are
     * provided: The `projection` (`mat4`) contains the projection matrix that is derived
     * using the provided window size and maps the pixel coordinates to normalized device
     * coordinates, the `tex` (`sampler2D`) is the TextureAtlas that contains all glyphs
     * and into which the `Base Texture Coordinates` and `Outline Texture Coordinates`
     * index into. The `baseColor` (`vec4`) contains the user-specified color for the base
     * layer, whereas the `outlineColor` (`vec4`) is the color for the outline layer.
     * Finally, the `hasOutline` (`bool`) is `true` whether the passed font has an outline
     * or not.
     *
     * \param program The custom ProgramObject that is used to render any passed text
     * \param framebufferSize The size of the framebuffer into which this FontRenderer
     *        renders
     */
    FontRenderer(std::unique_ptr<opengl::ProgramObject> program,
        glm::vec2 framebufferSize);

    /**
     * Default destructor that cleans used OpenGL names and the ProgramObject.
     */
    ~FontRenderer();

    /**
     * Create a new instance of the FontRenderer with a default ProgramObject. This method
     * requires the FileSystem to be initialized, as temporary files containing the source
     * code of the ShaderObjects will be created. This method requires a valid OpenGL
     * state.
     *
     * \return A raw pointer to the new default instance
     */
    static std::unique_ptr<FontRenderer> createDefault();

    /**
     * Create a new instance of the FontRenderer with a perspective subject ProgramObject.
     * This method requires the FileSystem to be initialized, as temporary files
     * containing the source code of the ShaderObjects will be created. This method
     * requires a valid OpenGL state.
     *
     * \return A raw pointer to the new default instance
     */
    static std::unique_ptr<FontRenderer> createProjectionSubjectText();

    /**
     * Initialize the singleton variant of the FontRenderer with the default
     * ProgramObject. This method requires the FileSystem to be initialized, as temporary
     * files containing the source code of the ShaderObjects will be created. The method
     * returns the success of the compilation and linking of the default ProgramObject.
     * This method requires a valid OpenGL state. An assertion is triggered if the
     * singleton FontRenderer is initialized twice.
     */
    static void initialize();

    /**
     * Deinitialize the singleton variant of the FontRenderer and cleans all used OpenGL
     * objects. Therefore, it requires a valid OpenGL state.
     */
    static void deinitialize();

    /**
     * Check whether the singleton variant of the FontRenderer has already been
     * initialized.
     *
     * \return `true` if the singleton FontRenderer has already been initialized; `false`
     *         otherwise
     */
    static bool isInitialized();

    /**
     * Returns the singleton FontRenderer. This method triggers an assertion if the
     * FontRenderer was not initialized before usage.
     *
     * \return The singleton FontRenderer
     */
    static FontRenderer& defaultRenderer();

    /**
     * Returns the singleton FontRenderer that renders projected text. This method
     * triggers an assertion if the FontRenderer was not initialized before usage.
     *
     * \return The singleton Fontrenderer rendering projected text
     */
    static FontRenderer& defaultProjectionRenderer();

    /**
     * Sets the size of the framebuffer that is used as the target for this FontRenderer.
     * It is used to convert the pixel coordinates (used in the #render) method to
     * normalized device coordinates.
     *
     * \param framebufferSize The size of the target framebuffer
     */
    void setFramebufferSize(glm::vec2 framebufferSize);

    /**
     * Renders the provided \p text to the pixel coordinates \p pos using the Font \p font
     * in the base color \p color. If the \p font has an outline, it is rendered in black
     * and the alpha value from the provided \p color.
     *
     * \param font The Font that is used to render the provided text
     * \param pos The screen-space position (in pixel coordinates) for 2D text rendering,
     *        or world-space coordinate for projection subject rendering, that is used to
     *        render the text
     * \param text The text that is rendered to the screen. The \p text can also contain
     *        `\n` to have a linebreak, which is of the correct length with regard to the
     *        selected font
     * \param color The base color that is used to render the text
     * \return The bounding box of the text and the number of lines that were printed
     */
    BoundingBoxInformation render(Font& font, const glm::vec2& pos, std::string_view text,
        const glm::vec4& color = glm::vec4(1.f)) const;

    /**
     * Renders the provided \p text to the pixel coordinates \p pos using the Font \p font
     * in the base color \p color and the outline color \p outlineColor. If the \p font
     * does not have an outline, the \p outlineColor is ignored.
     *
     * \param font The Font that is used to render the provided text
     * \param pos The screen-space position (in pixel coordinates) for 2D text rendering,
     *        or world-space coordinate for projection subject rendering, that is used to
     *        render the text
     * \param text The text that is rendered to the screen. The \p text can also contain
     *        `\n` to have a linebreak, which is of the correct length with regard to the
     *        selected font
     * \param color The base color that is used to render the text
     * \param outlineColor The outline color that is used to render the text if the Font
     *        has an outline
     * \return The bounding box of the text and the number of lines that were printed
     */
    BoundingBoxInformation render(Font& font, const glm::vec2& pos, std::string_view text,
        const glm::vec4& color, const glm::vec4& outlineColor) const;

    /**
     * Renders the provided \p text to the coordinates \p pos using the \p font in the
     * base \p color and the \p outlineColor. If the \p font does not have an outline, the
     * \p outlineColor is ignored.
     *
     * \param font The Font that is used to render the provided text
     * \param pos The world-space coordinate for projection subject rendering that is used
     *        to render the text
     * \param text The text that is rendered to the screen.The \p text can also contain
     *        `\n` to have a linebreak, which is of the correct length with regard to the
     *        selected font
     * \param color The base color that is used to render the text
     * \param outlineColor The outline color that is used to render the text if the Font
     *        has an outline
     * \param labelInfo Label information for the rendered text
     * \param offset Moves the label in xy position of the text's plane
     * \return The bounding box of the text and the number of lines that were printed
     */
    BoundingBoxInformation render(Font& font, const glm::vec3& pos,
        std::string_view text, const glm::vec4& color, const glm::vec4& outlineColor,
        const ProjectedLabelsInformation& labelInfo,
        const glm::vec2& offset = glm::vec2(0.f, 0.f)) const;

    /**
     * Renders the provided \p text to the coordinates \p pos using the \p font in the
     * base \p color. If the \p font has an outline, it is rendered in black, using the
     * \p color's alpha value.
     *
     * \param font The Font that is used to render the provided text
     * \param pos The world-space coordinate for projection subject rendering that is used
     *        to render the text
     * \param text The text that is rendered to the screen.The \p text can also contain
     *        `\n` to have a linebreak, which is of the correct length with regard to the
     *        selected font
     * \param color The base color that is used to render the text
     * \param labelInfo Label information for the rendered text
     * \param offset Moves the label in xy position of the text's plane
     * \return The bounding box of the text and the number of lines that were printed
     */
    BoundingBoxInformation render(Font& font, const glm::vec3& pos,
        std::string_view text, const glm::vec4& color,
        const ProjectedLabelsInformation& labelInfo,
        const glm::vec2& offset = glm::vec2(0.f, 0.f)) const;

    /**
     * Renders the provided \p text to the coordinates \p pos using the \p font in white
     * and a black outline, if the \p font has one.
     *
     * \param font The Font that is used to render the provided text
     * \param pos The world-space coordinate for projection subject rendering that is used
     *        to render the text
     * \param text The text that is rendered to the screen.The \p text can also contain
     *        '\n' to have a linebreak, which is of the correct length with regard to the
     *        selected font.
     * \param labelInfo Label information for the rendered text
     * \return The bounding box of the text and the number of lines that were printed
     */
    BoundingBoxInformation render(Font& font, const glm::vec3& pos,
        std::string_view text, const ProjectedLabelsInformation& labelInfo) const;

private:
    /// The singleton instance of the default FontRenderer
    static std::unique_ptr<FontRenderer> _defaultRenderer;

    /// The singleton instance of the default projection FontRenderer
    static std::unique_ptr<FontRenderer> _defaultProjectionRenderer;

    /// The framebuffer size that is used to compute the transformation from pixel
    /// coordinates to normalized device coordinates
    glm::vec2 _framebufferSize = glm::vec2(0.f);

    /// The ProgramObject that is used to render the text
    std::unique_ptr<opengl::ProgramObject> _program;

    struct GpuData {
        unsigned int vao = 0;

        /// The vertex buffer object containing the vertices for the text to be rendered
        unsigned int vbo = 0;

        /// The index buffer object allowing the reuse of glyph vertices
        unsigned int ibo = 0;
    };
    GpuData _orthogonal;
    GpuData _perspective;

    UniformCache(baseColor, outlineColor, tex, hasOutline, projection) _uniformCache;
    UniformCache(baseColor, outlineColor, tex, hasOutline, modelViewTransform,
        enableFalseDepth, disableTransmittance) _uniformCacheProjection;
    int _uniformMvp = -1;
};

enum class CrDirection {
    Up = 0,
    None,
    Down
};

/**
 * This helper method prints the passed arguments using the Font::render function of the
 * default font. It is equivalent to calling defaultRenderer::render with the same
 * arguments and discarding the second return value. This value moves the \p pos down by
 * the computed distance.
 *
 * \param font The Font that is used to render the provided text
 * \param pos The screen-space position (in pixel coordinates) that to render the text
 * \param text The text that is rendered to the screen using the default renderer
 * \param direction Determines whether the \p pos should be modified based on how many
 *        lines were rendered. None leaves the \p pos unmodified, and Up and Down move it
 *        up and down by the number of lines times the height of the used font
 * \param color The color that is used to the render the text
 * \param outlineColor The outline color that is used to the render the text if the
 *        provided \p font has one
 * \return The bounding box of the text that was printed
 */
glm::vec2 RenderFont(Font& font, glm::vec2& pos, std::string_view text,
    const glm::vec4& color, CrDirection direction, const glm::vec4& outlineColor);

/**
 * This helper method prints the passed arguments using the Font::render function of the
 * default font. It is equivalent to calling defaultRenderer::render with the same
 * arguments and discarding the second return value.
 *
 * \param font The Font that is used to render the provided text
 * \param pos The screen-space position (in pixel coordinates) that to render the text
 * \param text The text that is rendered to the screen using the default renderer
 * \param color The color that is used to the render the text
 * \param outlineColor The outline color that is used to the render the text if the
 *        provided \p font has one
 * \return The bounding box of the text that was printed
 */
glm::vec2 RenderFont(Font& font, const glm::vec2& pos, std::string_view text,
    const glm::vec4& color, const glm::vec4& outlineColor);

/**
 * This helper method prints the passed arguments using the Font::render function of the
 * default font. It is equivalent to calling defaultRenderer::render with the same
 * arguments and discarding the second return value. This value moves the \p pos down by
 * the computed distance.
 *
 * \param font The Font that is used to render the provided text
 * \param pos The screen-space position (in pixel coordinates) that to render the text
 * \param text The text that is rendered to the screen using the default renderer
 * \param color The color that is used to the render the text
 * \param direction Determines whether the \p pos should be modified based on how many
 *        lines were rendered. None leaves the \p pos unmodified, and Up and Down move it
 *        up and down by the number of lines times the height of the used font
 * \return The bounding box of the text that was printed
 */
glm::vec2 RenderFont(Font& font, glm::vec2& pos, std::string_view text,
    const glm::vec4& color, CrDirection direction = CrDirection::None);

/**
 * This helper method prints the passed arguments using the Font::render function of the
 * default font. It is equivalent to calling defaultRenderer::render with the same
 * arguments and discarding the second return value.
 *
 * \param font The Font that is used to render the provided text
 * \param pos The screen-space position (in pixel coordinates) that to render the text
 * \param text The text that is rendered to the screen using the default renderer
 * \param color The color that is used to the render the text
 * \return The bounding box of the text that was printed
 */
glm::vec2 RenderFont(Font& font, const glm::vec2& pos, std::string_view text,
    const glm::vec4& color = glm::vec4(1.f));

/**
 * This helper method prints the passed arguments using the Font::render function of the
 * default font. It is equivalent to calling defaultRenderer::render with the same
 * arguments and discarding the second return value. This value moves the \p pos down by
 * the computed distance.
 *
 * \param font The Font that is used to render the provided text
 * \param pos The screen-space position (in pixel coordinates) that to render the text
 * \param text The text that is rendered to the screen using the default renderer
 * \param direction Determines whether the \p pos should be modified based on how many
 *        lines were rendered. None leaves the \p pos unmodified, and Up and Down move it
 *        up and down by the number of lines times the height of the used font
 * \return The bounding box of the text that was printed
 */
glm::vec2 RenderFont(Font& font, glm::vec2& pos, std::string_view text,
    CrDirection direction = CrDirection::None);

/**
 * This helper method prints the passed arguments using the Font::render function of the
 * default font. It is equivalent to calling defaultRenderer::render with the same
 * arguments and discarding the second return value.
 *
 * \param font The Font that is used to render the provided text
 * \param pos The screen-space position (in pixel coordinates) that to render the text
 * \param text The text that is rendered to the screen using the default renderer
 * \return The bounding box of the text that was printed
 */
glm::vec2 RenderFont(Font& font, const glm::vec2& pos, std::string_view text);

} // namespace ghoul::fontrendering

#endif // __GHOUL___FONTRENDERER___H__
