#pragma once

#include <glm/glm.hpp>

void billboardGlInit();
void billboardGlDeinit();

void billboardDraw(glm::mat4 const& transform, glm::vec4 const& fill, glm::vec4 const& stroke, float width, float depth);
