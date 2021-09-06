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

float line(in float _lineCenter, in float _lineWidth, in float _coord) {
  // Calculate edges of line
  float start_edge = _lineCenter - (_lineWidth * 0.5f);
  float end_edge = _lineCenter + (_lineWidth * 0.5f);

  // Create line
  float line = step(start_edge, _coord) - step(end_edge, _coord);

  return line;
}

float rectangle(in float _linewidth_y, in float _ratio, in vec2 _coord) {
  // Calculate the widths and centers for the lines
  float linewidth_x = _linewidth_y * _ratio * VERTICAL_THICKNESS;
  float linecenter_x = linewidth_x * 0.5f;
  float linecenter_y = _linewidth_y * 0.5f;

  // Create the four lines for the rectangle
  float l = line(linecenter_x, linewidth_x, _coord.x);
  float r = line(1.0f - linecenter_x, linewidth_x, _coord.x);
  float b = line(linecenter_y, _linewidth_y, _coord.y);
  float t = line(1.0f - linecenter_y, _linewidth_y, _coord.y);

  // Add all lines together
  return l + r + b + t;
}

float crosshair(in float _linewidth, in float _ratio, in vec2 _coord) {
  float center = 0.5f;
  float crosshair_vertical = line(center, _linewidth * _ratio * VERTICAL_THICKNESS, _coord.x);
  float crosshair_horizontal = line(center, _linewidth, _coord.y);

  return crosshair_horizontal + crosshair_vertical;
}

float filledRectangle(in float _size, in float _ratio, in vec2 _coord) {
  float center = 0.5f;
  float horizontal = line(center, _size, _coord.y);
  float vertical = line(center, _size, _coord.x);

  return horizontal * vertical;
}

#include "fragment.glsl"

Fragment getFragment() {
    float _ratio = dimensions.y / dimensions.x;
    float _crosshair = 0.0f;
    float _rectangle = 0.0f;

    if(showCrosshair) {
      _crosshair = crosshair(lineWidth, _ratio, vs_st);
    }

    if(showRectangle) {
      _rectangle = rectangle(lineWidth, _ratio, vs_st);
    }

    // If both rectangle and crosshair are displayed, draw crosshair a bit smaller
    if(showCrosshair && showRectangle) {
      _crosshair *= filledRectangle(lineWidth * 7.0f, _ratio, vs_st);
    }

    float result = clamp(_crosshair + _rectangle, 0.0, 1.0);

    Fragment frag;
    frag.color = lineColor;
    frag.color.a *= result;

    return frag;
}
