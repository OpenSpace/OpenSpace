local seedPointsFileBatsrus = '${OPENSPACE_DATA}/scene/fieldlinessequence/seedpoints/BATS_R_US_all_combined.txt';
local seedPointsFileEnlil = '${OPENSPACE_DATA}/scene/fieldlinessequence/seedpoints/enlil.txt';
local eqSeedsEnlil2AU = '${OPENSPACE_DATA}/scene/fieldlinessequence/seedpoints/equitorialslice2_AU.txt';
local eqSeedsEnlil1AU_1nHalfdeg = '${OPENSPACE_DATA}/scene/fieldlinessequence/seedpoints/equitorialslice1_AU_1.5deg.txt';

-- local volumeFolderBatsrus = '${OPENSPACE_DATA}/bats_sequence';
local volumeFolderBatsrus = '${OPENSPACE_DATA}/new_bats';
local volumeFolderEnlil = '${OPENSPACE_DATA}/enlil_sequence';
local volumeFolderEnlilHighRes = '${OPENSPACE_DATA}/Leila_Mays_042517_SH_2';
-- local volumeFolderEnlil = '${OPENSPACE_DATA}/Ailsa';
local volumeFile1 = '${OPENSPACE_DATA}/bats_sequence/batsrus1.cdf';
local volumeFile2 = '${OPENSPACE_DATA}/bats_sequence/batsrus2.cdf';
local volumeFile3 = '${OPENSPACE_DATA}/bats_sequence/batsrus3.cdf';
local volumeFileEnlil = '${OPENSPACE_DATA}/Ailsa/Ailsa_Prise_101414_SH_1.enlil.0001.cdf';

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
    -- {
    --     Name = "EarthsMagnetosphereFieldlines",
    --     Parent = "GSMReferenceFrame",
    --     Renderable = {
    --         Type = "RenderableFieldlinesSequence",
    --         TracingMethod = "PreTraced",
    --         -- TracingMethod = "PreProcess",
    --         -- TracingMethod = "LiveTrace",
    --         ExtraVariables          = "T status",
    --         ExtraMagnitudeVariables = "jx jy jz",

    --         VectorVolume = {
    --             -- Type = "VolumeKameleon",
    --             Directory = volumeFolderBatsrus,
    --             -- Model = "BATSRUS",
    --             TracingVariable = "b", -- "b" is the variable specifying the magnetic field
    --             -- Variables = {"bx", "by", "bz"},
    --             -- TimeDependent = true,
    --         },
    --         Fieldlines = {
    --             MaximumTracingSteps = 1000.0,
    --             -- Stepsize = 1.0,
    --             -- Classification = true,
    --             Morphing = false,
    --             ResamplingType = 4, -- resampling will depend on: 1=length, 2=integral, 3=index
    --             NumResamples = 500,
    --             QuickMorphDistance = 637100000,
    --         },
    --         SeedPoints = {
    --             -- Type = "File",
    --             File = seedPointsFileBatsrus,
    --         }
    --     }
    -- },
    -- {
    --     Name = "ENLILFieldlinesSequence",
    --     Parent = "HNMReferenceFrame",
    --     Renderable = {
    --         Type = "RenderableFieldlinesSequence",
            -- TracingMethod = "PreTraced",
            -- TracingMethod = "PreTraced",
            -- TracingMethod = "PreTraced",
    --         VectorVolume = {
    --             -- Type = "VolumeKameleon",
    --             Directory = volumeFolderEnlil,
    --             -- Model = "BATSRUS",
    --             TracingVariable = "b", -- "b" is the variable specifying the magnetic field
    --             -- Variables = {"bx", "by", "bz"},
    --             -- TimeDependent = true,
    --         },
    --         Fieldlines = {
    --             MaximumTracingSteps = 100000.0,
    --             -- Stepsize = 1.0,
    --             -- Classification = true,
    --             Morphing = true,
    --             -- NumResamples = 5,
    --             ResamplingType = 4, -- resampling will depend on: 1=length, 2=integral, 3=index
    --         },
    --         SeedPoints = {
    --             -- Type = "File",
    --             File = seedPointsFileEnlil,
    --         }
    --     }
    -- },
    -- {
    --     Name = "EMF",
    --     Parent = "GSMReferenceFrame",
    --     Renderable = {
    --         Type = "RenderableFieldlinesSequence",
    --         VectorVolume = {
    --             -- Type = "VolumeKameleon",
    --             -- Directory = volumeFolderBatsrus,
    --             -- Model = "BATSRUS",
    --             -- TracingVariable = "bx",
    --             -- Variables = {"bx", "by", "bz"},
    --             -- TimeDependent = true,
    --         },
    --         Fieldlines = {
    --             -- Stepsize = 1.0,
    --             -- Classification = true,
    --         },
    --         SeedPoints = {
    --             -- Type = "File",
    --             -- File = seedPointsFileBatsrus,
    --         }
    --     }
    -- },

    {
        Name = "HighResENLILFieldlinesSequence",
        Parent = "HNMReferenceFrame",
        Renderable = {
            Type = "RenderableFieldlinesSequence",
            -- TracingMethod = "PreProcess",
            TracingMethod = "PreTraced",
            -- TracingMethod = "PreTraced",
            SourceFolder = volumeFolderEnlil,

            VectorVolume = {
                -- Type = "VolumeKameleon",
                Directory = volumeFolderEnlilHighRes,
                -- Model = "BATSRUS",
                TracingVariable = "b", -- "b" is the variable specifying the magnetic field
                -- Variables = {"bx", "by", "bz"},
                -- TimeDependent = true,
            },
            Fieldlines = {
                MaximumTracingSteps = 10000.0,
                -- Stepsize = 1.0,
                -- Classification = true,
                Morphing = false,
                -- NumResamples = 5,
                ResamplingType = 4, -- resampling will depend on: 1=length, 2=integral, 3=index
            },
            SeedPointInfo = {
                -- Type = "File",
                -- File = seedPointsFileEnlil,
                File = eqSeedsEnlil1AU_1nHalfdeg,
                -- File = eqSeedsEnlil2AU,
            }
        }
    },
}
