uniform float lineWidth;
uniform vec2 dimensions;
uniform bool showCrosshair;
uniform bool showRectangle;
uniform vec4 lineColor;

in vec2 vs_st;
in vec4 vs_position;

// A factor which states how much thicker vertical lines are rendered than horizontal
// This compensates for the optical illusion that vertical lines appear thinner
#define VERTICAL_THICKNESS 1.15f

float createLine(in float lineCenter, in float lineWidth, in float coord) {
  // Calculate edges of line
  float start_edge = lineCenter - (lineWidth * 0.5f);
  float end_edge = lineCenter + (lineWidth * 0.5f);

  // Create line
  float line = step(start_edge, coord) - step(end_edge, coord);

  return line;
}

float createRectangle(in float linewidth_y, in float ratio, in vec2 coord) {
  // Calculate the widths and centers for the lines
  float linewidth_x = linewidth_y * ratio * VERTICAL_THICKNESS;
  float linecenter_x = linewidth_x * 0.5f;
  float linecenter_y = linewidth_y * 0.5f;

  // Create the four lines for the rectangle
  float l = createLine(linecenter_x, linewidth_x, coord.x);
  float r = createLine(1.0f - linecenter_x, linewidth_x, coord.x);
  float b = createLine(linecenter_y, linewidth_y, coord.y);
  float t = createLine(1.0f - linecenter_y, linewidth_y, coord.y);

  // Add all lines together
  return l + r + b + t;
}

float createCrosshair(in float linewidth, in float ratio, in vec2 coord) {
  float center = 0.5f;
  float crosshair_vertical = createLine(center, linewidth * ratio * VERTICAL_THICKNESS, coord.x);
  float crosshair_horizontal = createLine(center, linewidth, coord.y);

  return crosshair_horizontal + crosshair_vertical;
}

float createFilledRectangle(in float size, in float ratio, in vec2 coord) {
  float center = 0.5f;
  float horizontal = createLine(center, size, coord.y);
  float vertical = createLine(center, size, coord.x);

  return horizontal * vertical;
}

#include "fragment.glsl"

Fragment getFragment() {
    float ratio = dimensions.y / dimensions.x;
    float crosshair = 0.0f;
    float rectangle = 0.0f;

    if(showCrosshair) {
      crosshair = createCrosshair(lineWidth, ratio, vs_st);
    }

    if(showRectangle) {
      rectangle = createRectangle(lineWidth, ratio, vs_st);
    }

    // If both rectangle and crosshair are displayed, draw crosshair a bit smaller
    if(showCrosshair && showRectangle) {
      crosshair *= createFilledRectangle(lineWidth * 7.0f, ratio, vs_st);
    }

    float result = clamp(crosshair + rectangle, 0.0, 1.0);

    Fragment frag;
    frag.color = lineColor;
    frag.color.a *= result;

    return frag;
}
