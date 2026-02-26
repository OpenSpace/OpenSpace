-- Lua Vector Field Filter
-- This script defines a visibility filter for a RenderableVectorField. The filter is
-- evaluated at runtime for each vector in the field and determines whether the vector
-- should be rendered or hidden.
--
-- The filter function must be named `filter` and return a boolean value:
--   - true  -> render the vector
--   - false -> hide the vector
--
-- Function arguments:
--   pos: { x, y, z }
--     The position of the vector in local coordinates.
--
--   dir: { vx, vy, vz }
--     The direction (or velocity) vector at the given position.
--
-- This example filter renders only vectors within a fixed distance from the origin.

-- Visibility filter function called for each vector
function filter(pos, dir)
  local distance = math.sqrt(pos[1] * pos[1] + pos[2] * pos[2] + pos[3] * pos[3])
  return distance < 5000
end
