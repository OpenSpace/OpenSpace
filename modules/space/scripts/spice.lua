openspace.space.documentation = {
  {
    Name = "tleToSpiceTranslation",
    Arguments = {
      { "tlePath", "String" }
    },
    Return = "{ Translation, SpiceKernel }",
    Documentation = [[
      Takes the provided TLE file, converts it into a SPICE kernel and returns a
      SpiceTranslation instance that can be used to access the information in the TLE
      file using SPICE's superior integral solver.

      The second return value is the spice kernel that should be loaded and unloaded by
      whoever called this function.
    ]]
  }
}

openspace.space.tleToSpiceTranslation = function(tlePath)
  --
  -- First we have to create a name for the temporary file
  --

  -- We first pass the path through the absPath function to both get rid of path tokens,
  -- but more importantly harmonize the path separators so we don't have to deal with
  -- / and \ variants
  tlePath = openspace.absPath(tlePath)

  -- We don't have a function to return the file component so we extract the directory
  -- and remove as many characters as it is long instead
  local dirComponent = openspace.directoryForPath(tlePath)
  -- +2 because the sub function is inclusive and we have a trailing \ or / at the end
  local filename = tlePath:sub(dirComponent:len() + 2, tlePath:len())
  local temporaryFile = openspace.absPath("${TEMPORARY}/" .. filename .. ".bsp")
  -- Now the temporary file for, for example ISS.txt will be the solution for:
  -- ${TEMPORARY}/ISS.txt.bsp

  local id = openspace.spice.convertTLEtoSPK(tlePath, temporaryFile)

  local translation = {
    Type = "SpiceTranslation",
    Target = tostring(id),
    Observer = "Earth",
    Frame = "J2000"
  }

  return translation, temporaryFile
end
