-- The `scale` function takes exactly three arguments and returns the three scaling
-- values (one for each axis) as a single table.
-- The three parameters are all provided as the number of seconds past the J2000 epoch
-- (2000-01-01 12:00:00), which can be both fractional numbers as well as negative numbers
-- for dates earlier than the epoch.
--  1. `simulationTime` is the value of the in-game clock for the current frame
--  2. `prevSimulationTime` is the value of the in-game clock for the previous frame
--  3. `wallTime` is the value of the computer clock as seconds past the epoch
function scale(simulationTime, prevSimulationTime, wallTime)
  -- Make the scaling along the x-axis oscillate between -3 and 3 once per second
  return { 3 * math.sin(simulationTime), 1, 1 }
end
