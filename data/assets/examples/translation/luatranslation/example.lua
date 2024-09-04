-- The `translation` function takes exactly three arguments and returns the three
-- translation values (one for each axis) as a single table.
-- The three parameters are all provided as the number of seconds past the J2000 epoch
-- (2000-01-01 12:00:00), which can be both fractional numbers as well as negative numbers
-- for dates earlier than the epoch.
--  1. `simulationTime` is the value of the in-game clock for the current frame
--  2. `prevSimulationTime` is the value of the in-game clock for the previous frame
--  3. `wallTime` is the value of the computer clock as seconds past the epoch
function translation(simulationTime, prevSimulationTime, wallTime)
  -- Make the object rotate around the z axis in a circular pattern
  return { math.sin(simulationTime), math.cos(simulationTime), 0 }
end
