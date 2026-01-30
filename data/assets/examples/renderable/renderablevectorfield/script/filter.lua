-- Lua Vector Field Filter
--
-- This script defines a visibility filter for a RenderableVectorField. The filter is
-- evaluated at runtime for each vector in the field and determines whether the vector
-- should be rendered or hidden.
--
-- The filter function must be named `filter` and return a boolean value:
--   - true  → render the vector
--   - false → hide the vector
--
-- Function arguments:
--   pos : { x, y, z }
--     The position of the vector in global galactic coordinates.
--
--   dir : { vx, vy, vz }
--     The direction (or velocity) vector at the given position.
--
-- This example filter renders only vectors within a fixed distance from the origin.

-- Computes the Euclidean distance from the origin
function distance(p)
    return math.sqrt(p[1] * p[1] + p[2] * p[2] + p[3] * p[3])
end

-- Visibility filter function called for each vector
function filter(pos, dir)
    return distance(pos) < 5000
end
