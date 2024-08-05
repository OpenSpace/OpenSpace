-- The `rotation` function takes exactly three arguments and returns the 9 values that
-- make up the final rotation matrix as a table.
-- The three parameters are all provided as the number of seconds past the J2000 epoch
-- (2000-01-01 12:00:00), which can be both fractional numbers as well as negative numbers
-- for dates earlier than the epoch.
--  1. `simulationTime` is the value of the in-game clock for the current frame
--  2. `prevSimulationTime` is the value of the in-game clock for the previous frame
--  3. `wallTime` is the value of the computer clock as seconds past the epoch
function rotation(simulationTime, prevSimulationTime, wallTime)
  -- Create a rotation around the x axis
  return {
    1,                        0,                         0,
    0, math.cos(simulationTime), -math.sin(simulationTime),
    0, math.sin(simulationTime),  math.cos(simulationTime)
  }
end
