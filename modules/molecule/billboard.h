#pragma once

#include <glbinding/gl/types.h>
#include <glm/glm.hpp>

void billboardGlInit();
void billboardGlDeinit();

/// Draw the pre-rendered screen-space texture (colorTex, depthTex) in a circular billboard.
/// transform: Model-to-screen transform of the billboard rect (model is -0.5, 0.5).
/// colorTex: Color values to draw inside billboard.
/// depthTex: Depth values to draw inside billboard.
/// stroke: Color of the billboard outline.
/// width: Width of the billboard outline.
/// depth: Depth of the billboard outline.
/// falloffExp: Fall-off exponent of the billboard outline.
void billboardDraw(glm::mat4 const& transform, gl::GLuint colorTex, gl::GLuint depthTex, glm::vec4 const& stroke, float width, float depth, float falloffExp = 2.0f);
