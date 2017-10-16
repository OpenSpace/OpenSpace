RosettaKernels = {
    "${OPENSPACE_DATA}/spice/Rosetta/SCLK/ROS_160718_STEP.TSC",
    "${OPENSPACE_DATA}/spice/Rosetta/SCLK/ros_triv.tsc",

    "${OPENSPACE_DATA}/spice/Rosetta/SPK/CORB_DV_243_01___T19_00325.BSP",
    "${OPENSPACE_DATA}/spice/Rosetta/SPK/CORB_DV_223_01___T19_00302.BSP",
    "${OPENSPACE_DATA}/spice/Rosetta/SPK/CORB_DV_145_01___T19_00216.BSP",

    "${OPENSPACE_DATA}/spice/Rosetta/SPK/LORB_DV_236_01___T19_00318.BSP",
    "${OPENSPACE_DATA}/spice/Rosetta/SPK/LORB_DV_223_01___T19_00302.BSP",
    "${OPENSPACE_DATA}/spice/Rosetta/SPK/LORB_DV_145_01___T19_00216.BSP",
    
    "${OPENSPACE_DATA}/spice/Rosetta/SPK/RORB_DV_243_01___T19_00325.BSP",
    "${OPENSPACE_DATA}/spice/Rosetta/SPK/RORB_DV_223_01___T19_00302.BSP",
    "${OPENSPACE_DATA}/spice/Rosetta/SPK/RORB_DV_145_01___T19_00216.BSP",

    "${OPENSPACE_DATA}/spice/Rosetta/CK/ATNR_P040302093352_00127.BC",

    "${OPENSPACE_DATA}/spice/Rosetta/SPK/ROS_STRUCT_V5.BSP",

    "${OPENSPACE_DATA}/spice/Rosetta/IK/ROS_NAVCAM_V01.TI",
    
    "${OPENSPACE_DATA}/spice/Rosetta/FK/ROS_CHURYUMOV_V01.TF",
    "${OPENSPACE_DATA}/spice/Rosetta/FK/ROS_V26.TF",

    -- CK
    -- Rosetta attitude
    "${OPENSPACE_DATA}/spice/Rosetta/CK/RATT_DV_243_01_01____00325.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/RATT_DV_223_01_01____00302.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/RATT_DV_145_01_01____00216.BC",

    -- Comet attitude
    "${OPENSPACE_DATA}/spice/Rosetta/CK/CATT_DV_243_01_______00325.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/CATT_DV_223_01_______00302.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/CATT_DV_145_01_______00216.BC",

    -- High gain antenna
    "${OPENSPACE_DATA}/spice/Rosetta/CK/ROS_HGA_2016_V0035.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/ROS_HGA_2015_V0053.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/ROS_HGA_2014_V0044.BC",

    -- Solar arrays
    "${OPENSPACE_DATA}/spice/Rosetta/CK/ROS_SA_2016_V0034.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/ROS_SA_2015_V0042.BC",
    "${OPENSPACE_DATA}/spice/Rosetta/CK/ROS_SA_2014_V0047.BC",


    "${OPENSPACE_DATA}/spice/Rosetta/PCK/ROS_CGS_RSOC_V03.TPC",
}

local RotationMatrix = {
    0, 1, 0,
    0, 0, 1,
    1, 0, 0
}



return {
    {
        Name = "Rosetta",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "ROSETTA",
                Observer = "SUN",
                Kernels = RosettaKernels
            },  
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ROS_SPACECRAFT",
                DestinationFrame = "GALACTIC",
            }
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "RosettaModel",
        Parent = "Rosetta",
        Transform = {
            Scale = {
                Type = "StaticScale",
                -- The scale of the model is in cm; OpenSpace is in m
                Scale = 0.01
            }
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_black_foil",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/black_foil.obj"
            },
            ColorTexture = "textures/foil_silver_ramp.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_black_parts",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/black_parts.obj"
            },
            ColorTexture = "textures/foil_silver_ramp.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_dish",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/dish.obj"
            },
            ColorTexture = "textures/dish_AO.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_parts",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/parts.obj"
            },
            ColorTexture = "textures/parts2_AO.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_silver_foil",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/silver_foil.obj"
            },
            ColorTexture = "textures/foil_silver_ramp.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_vents",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/vents.obj"
            },
            ColorTexture = "textures/tex_01.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_wing_a",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/wingA.obj"
            },
            ColorTexture = "textures/tex_01.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Rosetta_wing_b",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/wingB.obj"
            },
            ColorTexture = "textures/tex_01.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
        -- Transform = {
        --     Rotation = {
        --         Type = "SpiceRotation",
        --         SourceFrame = "-226025", -- ROS_SA
        --         DestinationFrame = "ROS_SPACECRAFT",
        --     }
        -- }
    },
    {
        Name = "Rosetta_yellow_foil",
        Parent = "RosettaModel",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/yellow_foil.obj"
            },
            ColorTexture = "textures/foil_gold_ramp.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Philae",
        Parent = "67PBarycenter",
        -- This should need a transform, but currently the model is intrinsically
        -- translated
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "PHILAE",
                Observer = "CHURYUMOV-GERASIMENKO",
                Kernels = RosettaKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ROS_SPACECRAFT",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                -- The scale of the model is in cm; OpenSpace is in m
                Scale = 0.01
            }
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Philae_foil",
        Parent = "Philae",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/lander_foil.obj"
            },
            ColorTexture = "textures/foil_silver_ramp.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Philae_lids",
        Parent = "Philae",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/lander_lids.obj"
            },
            ColorTexture = "textures/parts2_AO.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Philae_parts",
        Parent = "Philae",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/lander_parts.obj"
            },
            ColorTexture = "textures/foil_silver_ramp.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "Philae_solarp",
        Parent = "Philae",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/lander_solarp.obj"
            },
            ColorTexture = "textures/tex_01.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "RosettaCometTrail",
        Parent = "67PBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "ROSETTA",
                Observer = "CHURYUMOV-GERASIMENKO",
            },
            Color = { 0.288, 0.375, 0.934 },
            StartTime = "2014 AUG 01 12:00:00",
            EndTime = "2016 SEP 30 12:00:00",
            SampleInterval = 3600,
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "PhilaeTrail",
        Parent = "67PBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "PHILAE",
                Observer = "CHURYUMOV-GERASIMENKO",
            },
            Color = { 0.8, 0.5, 1.0 },
            StartTime = "2014 NOV 12 08:35:00",
            EndTime = "2014 NOV 12 17:00:00",
            SampleInterval = 2,
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "NAVCAM",
        Parent = "Rosetta",
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    {
        Name = "NAVCAM FOV",
        Parent = "NAVCAM",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "ROSETTA",
            Frame = "ROS_NAVCAM-A",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {
                Name       = "ROS_NAVCAM-A",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
            },
            PotentialTargets = {
                "CHURYUMOV-GERASIMENKO"
            },
            FrameConversions = {
                ["CHURYUMOV-GERASIMENKO"] = "67P/C-G_CK"
            }
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    },
    -- Latest image taken by NAVCAM
    { 
        Name = "ImagePlaneRosetta",
        Parent = "67P",
        Renderable = {
            Type = "RenderablePlaneProjection",
            Frame = "67P/C-G_CK",
            DefaultTarget = "CHURYUMOV-GERASIMENKO",
            Spacecraft = "ROSETTA",
            Instrument = "ROS_NAVCAM-A",
            Moving = false,
            Texture = "textures/defaultProj.png",
        },
        GuiPath = "/Solar System/Missions/Rosetta"
    }
}
