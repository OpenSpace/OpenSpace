local file1 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_closed_seeds_all.txt';
local file2 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_north_all.txt';
local file3 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_south_all.txt';
local file4 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_solar_wind_all.txt';
local file5 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_separatrix_seeds_all.txt';
local file6 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_all_combined.txt';

local volumeFile1 = '${OPENSPACE_DATA}/bats_sequence/batsrus1.cdf';
local volumeFile2 = '${OPENSPACE_DATA}/bats_sequence/batsrus2.cdf';
local volumeFile3 = '${OPENSPACE_DATA}/bats_sequence/batsrus3.cdf';
local volumeFileEnlil = '${OPENSPACE_DATA}/enlil_sequence/Ailsa_Prise_101414_SH_1.enlil.0001.cdf';

return {
    {
        Name = "HNMReferenceFrame",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                -- SourceFrame = "GALACTIC",
                -- DestinationFrame = "HEEQ",
                SourceFrame = "HEEQ180",
                DestinationFrame = "GALACTIC",
                Kernels = "${OPENSPACE_DATA}/spice/iSWAKernels/HNM-enlil_ref_frame.tf",
            },
        },
    },
    {
        Name = "GSMReferenceFrame",
        Parent = "EarthBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                -- SourceFrame = "HEEQ",
                -- DestinationFrame = "GALACTIC",

                SourceFrame = "GSM",
                DestinationFrame = "GALACTIC",
                Kernels = "${OPENSPACE_DATA}/spice/iSWAKernels/GSM.ti",
            },
        },
    },
    {
        Name = "FieldlinesTimestep1",
        Parent = "GSMReferenceFrame",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile1,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"},
                TimeDependent = true,
            },
            Fieldlines = {
                Stepsize = 1.0,
                Classification = true,
            },
            SeedPoints = {
                Type = "File",
                File = file6;
            }
        }
    },
    {
        Name = "FieldlinesTimestep2",
        Parent = "GSMReferenceFrame",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile2,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"},
                TimeDependent = true,
            },
            Fieldlines = {
                Stepsize = 1.0,
                Classification = true,
            },
            SeedPoints = {
                Type = "File",
                File = file6;
            }
        }
    },
    {
        Name = "FieldlinesTimestep3",
        Parent = "GSMReferenceFrame",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile3,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"},
                TimeDependent = true,
            },
            Fieldlines = {
                Stepsize = 1.0,
                Classification = true,
            },
            SeedPoints = {
                Type = "File",
                File = file1;
            }
        }
    },
    {
        Name = "FieldlinesEnlil",
        Parent = "HNMReferenceFrame", --"Sun",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFileEnlil,
                Model = "ENLIL",
                Variables = {"br", "btheta", "bphi"},
                TimeDependent = false,
            },
            Fieldlines = {
                Stepsize = 1.0,
                Classification = true,
            },
            SeedPoints = {
                Type = "File",
                File = '${OPENSPACE_DATA}/scene/fieldlinessequence/seedpoints/enlil.txt';
            }
        }
    },
    -- {
    --     Name = "Fieldlines1",
    --     Parent = "Earth",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"}
    --         },
    --         Fieldlines = {
    --             Stepsize = 1,
    --             Classification = true,
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_closed_seeds_all.txt';
    --         }
    --     }
    -- },
    -- {
    --     Name = "Fieldlines2",
    --     Parent = "Earth",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"}
    --         },
    --         Fieldlines = {
    --             Stepsize = 1,
    --             Classification = true
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_north_all.txt'
    --         }
    --     }
    -- },
    -- {
    --     Name = "Fieldlines3",
    --     Parent = "Earth",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"}
    --         },
    --         Fieldlines = {
    --             Stepsize = 1,
    --             Classification = true
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_south_all.txt'
    --         }
    --     }
    -- },
    -- {
    --     Name = "Fieldlines4",
    --     Parent = "Earth",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"}
    --         },
    --         Fieldlines = {
    --             Stepsize = 1,
    --             Classification = true
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_solar_wind_all.txt'
    --         }
    --     }
    -- },
    -- {
    --     Name = "Fieldlines5",
    --     Parent = "Earth",
    --     Renderable = {
    --         Type = "RenderableFieldlines",
    --         VectorField = {
    --             Type = "VolumeKameleon",
    --             File = volumeFile,
    --             Model = "BATSRUS",
    --             Variables = {"bx", "by", "bz"}
    --         },
    --         Fieldlines = {
    --             Stepsize = 1,
    --             Classification = true
    --         },
    --         SeedPoints = {
    --             Type = "File",
    --             File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_separatrix_seeds_all.txt'
    --         }
    --     }
    -- }
}
