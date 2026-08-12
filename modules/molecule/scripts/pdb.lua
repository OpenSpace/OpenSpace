registerFunction(
  {
    Name = "addMoleculePDB",
    Arguments = {
      { "molecule", "String" }
    },
    Documentation = [[
      Searches the RCSB protein data bank for the provided molecule id, for example
      '1C17', downloads the PDB file and adds it as a new scene graph node in the center
      of the solar system.

      To find an entry to add, use the search field found at https://www.rcsb.org/, the
      entry id will then be a four letter code listed in the URL when visiting an entry
      or at the top of the page of that entry.
    ]],
    Function = function(molecule)
      if not openspace.directoryExists(openspace.absPath("${USER}/data/pdbs")) then
        openspace.createDirectory(openspace.absPath("${USER}/data/pdbs"))
      end

      local Input = "https://files.rcsb.org/download/" .. molecule .. ".pdb"
      local Output = openspace.absPath("${USER}/data/pdbs/" .. molecule .. ".pdb")
      local WaitForCompletion = true
      local OverrideExistingFile = false
      local success =
        openspace.downloadFile(Input, Output, WaitForCompletion, OverrideExistingFile)

      if not success then
        error("Could not find molecule '" .. molecule .. "'")
      end

      local Molecule = {
        Identifier = "Molecule-" .. openspace.makeIdentifier(molecule),
        Renderable = {
          Type = "RenderableMolecule",
          MoleculeFile = Output,
          Representations = {
            {
              Type = "Licorice"
            }
          }
        },
        Tag = { "molecule_visualization" },
        GUI = {
          Name = molecule,
          Path = "/Molecules/PDB",
          Description = "https://www.rcsb.org/structure/" .. molecule
        }
      }

      openspace.addSceneGraphNode(Molecule)
    end
  }
)
