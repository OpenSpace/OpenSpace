local seedPointsFileBatsrus = '${OPENSPACE_DATA}/scene/fieldlinessequence/seedpoints/BATS_R_US_all_combined.txt';
local seedPointsFileEnlil = '${OPENSPACE_DATA}/scene/fieldlinessequence/seedpoints/enlil.txt';

local volumeFolderBatsrus = '${OPENSPACE_DATA}/bats_sequence/';
local volumeFile1 = '${OPENSPACE_DATA}/bats_sequence/batsrus1.cdf';
local volumeFile2 = '${OPENSPACE_DATA}/bats_sequence/batsrus2.cdf';
local volumeFile3 = '${OPENSPACE_DATA}/bats_sequence/batsrus3.cdf';
local volumeFileEnlil = '${OPENSPACE_DATA}/Ailsa_Prise_101414_SH_1.enlil.0001.cdf';

return {
    {
        Name = "HNMReferenceFrame",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "HEEQ180",
                DestinationFrame = "GALACTIC",
                Kernels = "${OPENSPACE_DATA}/scene/fieldlinessequence/customkernels/HNM-enlil_ref_frame.tf",
            },
        },
    },
    {
        Name = "GSMReferenceFrame",
        Parent = "EarthBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "GSM",
                DestinationFrame = "GALACTIC",
                Kernels = "${OPENSPACE_DATA}/scene/fieldlinessequence/customkernels/GSM.ti",
            },
        },
    },
    {
        Name = "EarthsMagnetosphereFieldlines",
        Parent = "GSMReferenceFrame",
        Renderable = {
            Type = "RenderableFieldlinesSequence",
            VectorVolume = {
                -- Type = "VolumeKameleon",
                Directory = volumeFolderBatsrus,
                -- Model = "BATSRUS",
                TracingVariable = "b", -- "b" is the variable specifying the magnetic field
                -- Variables = {"bx", "by", "bz"},
                -- TimeDependent = true,
            },
            Fieldlines = {
                -- Stepsize = 1.0,
                -- Classification = true,
            },
            SeedPoints = {
                -- Type = "File",
                File = seedPointsFileBatsrus,
            }
        }
    },
    {
        Name = "EMF",
        Parent = "GSMReferenceFrame",
        Renderable = {
            Type = "RenderableFieldlinesSequence",
            VectorVolume = {
                -- Type = "VolumeKameleon",
                -- Directory = volumeFolderBatsrus,
                -- Model = "BATSRUS",
                -- TracingVariable = "bx",
                -- Variables = {"bx", "by", "bz"},
                -- TimeDependent = true,
            },
            Fieldlines = {
                -- Stepsize = 1.0,
                -- Classification = true,
            },
            SeedPoints = {
                -- Type = "File",
                -- File = seedPointsFileBatsrus,
            }
        }
    },

    -- {
    --     Name = "FieldlinesTimestep1",
    --     Parent = "GSMReferenceFrame",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile1,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"},
    --             TimeDependent = true,
    --         },
    --         Fieldlines = {
    --             Stepsize = 1.0,
    --             Classification = true,
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_all_combined.txt';
    --         }
    --     }
    -- },
    -- {
    --     Name = "FieldlinesTimestep2",
    --     Parent = "GSMReferenceFrame",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile2,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"},
    --             TimeDependent = true,
    --         },
    --         Fieldlines = {
    --             Stepsize = 1.0,
    --             Classification = true,
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_all_combined.txt';
    --         }
    --     }
    -- },
    -- {
    --     Name = "FieldlinesTimestep3",
    --     Parent = "GSMReferenceFrame",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile3,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"},
    --             TimeDependent = true,
    --         },
    --         Fieldlines = {
    --             Stepsize = 1.0,
    --             Classification = true,
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = file1;
    --         }
    --     }
    -- },
    -- {
    --     Name = "FieldlinesEnlil",
    --     Parent = "HNMReferenceFrame", --"Sun",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFileEnlil,
    --             Model = "ENLIL",
    --             Variables = {"br", "btheta", "bphi"},
    --             TimeDependent = false,
    --         },
    --         Fieldlines = {
    --             Stepsize = 1.0,
    --             Classification = true,
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/enlil.txt';
    --         }
    --     }
    -- },
}
