RosettaKernels = {
        --needed
    "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp",
    -- SPK  
    --long term orbits loaded first
    -- '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/LORL_DL_006_01____H__00156.BSP',
    -- '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/RORL_DL_006_01____H__00156.BSP',
    -- '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/CORL_DL_006_01____H__00156.BSP',

    --Jan 2014 - May 2015 (version match with 00162 ck files)
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/CORB_DV_097_01_______00162.BSP",
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/RORB_DV_097_01_______00162.BSP",
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/LORB_DV_097_01_______00162.BSP",

    --IK
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/IK/ROS_NAVCAM_V01.TI",
    "${OPENSPACE_DATA}/spice/RosettaKernels/IK/ROS_NAVCAM_V00-20130102.TI",

    --SCLK
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/SCLK/ROS_150227_STEP.TSC",

    -- FK
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/FK/ROS_CHURYUMOV_V01.TF",
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/FK/ROS_V24.TF",

    -- CK
    -- '${OPENSPACE_DATA}/spice/RosettaKernels/CK/RATT_DV_097_01_01____00162.BC',
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/CK/CATT_DV_097_01_______00162.BC",

    --SCLK
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/SCLK/ROS_150227_STEP.TSC",
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/SCLK/ROS_160425_STEP.TSC",

    -- FK

    "${OPENSPACE_DATA}/spice/RosettaKernels_New/FK/ROS_CHURYUMOV_V01.TF",
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/FK/ROS_V26.TF",
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/FK/ROS_V24.TF",
    -- CK
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/CK/RATT_DV_211_01_01____00288.BC",
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/CK/CATT_DV_211_01_______00288.BC",
    '${OPENSPACE_DATA}/spice/RosettaKernels/CK/RATT_DV_097_01_01____00162.BC',
    "${OPENSPACE_DATA}/spice/RosettaKernels/CK/CATT_DV_097_01_______00162.BC",

    -- PCK
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/PCK/ROS_CGS_RSOC_V03.TPC",
    -- "${OPENSPACE_DATA}/spice/RosettaKernels/PCK/ROS_CGS_RSOC_V03.TPC",



    "${OPENSPACE_DATA}/spice/RosettaKernels_New/CK/ROS_SA_2014_V0047.BC",
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/CK/ROS_SA_2015_V0042.BC",
    "${OPENSPACE_DATA}/spice/RosettaKernels_New/CK/ROS_SA_2016_V0019.BC",
}

return {
    {
        Name = "Rosetta",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceEphemeris",
                Body = "ROSETTA",
                Reference = "GALACTIC",
                Observer = "SUN",
                Kernels = RosettaKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ROS_SPACECRAFT",
                DestinationFrame = "GALACTIC",
            }
        }
    },
    {
        Name = "Rosetta_black_foil",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/black_foil.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/foil_silver_ramp.png"
            }
        }
    },
    {
        Name = "Rosetta_black_parts",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/black_parts.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/foil_silver_ramp.png"
            }
        }
    },
    {
        Name = "Rosetta_dish",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/dish.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/dish_AO.png"
            }
        }
    },
    {
        Name = "Rosetta_parts",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/parts.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/parts2_AO.png"
            }
        }
    },
    {
        Name = "Rosetta_silver_foil",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/silver_foil.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/foil_silver_ramp.png"
            }
        }
    },
    {
        Name = "Rosetta_vents",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/vents.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/tex_01.png"
            }
        }
    },
    {
        Name = "Rosetta_wing_a",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/wingA.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/tex_01.png"
            }
        }
    },
    {
        Name = "Rosetta_wing_b",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/wingB.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/tex_01.png"
            }
        }
    },
    {
        Name = "Rosetta_yellow_foil",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "rosetta/yellow_foil.obj"
            },
            Textures = {
                Type = "simple",
                Color = "textures/foil_gold_ramp.png"
            }
        }
    },
    {
        Name = "Philae",
        Parent = "Rosetta"
        -- This should need a transform, but currently the model is intrinsically
        -- translated
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
            Textures = {
                Type = "simple",
                Color = "textures/foil_silver_ramp.png"
            }
        }
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
            Textures = {
                Type = "simple",
                Color = "textures/parts2_AO.png"
            }
        }
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
            Textures = {
                Type = "simple",
                Color = "textures/foil_silver_ramp.png"
            }
        }
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
            Textures = {
                Type = "simple",
                Color = "textures/tex_01.png"
            }
        }
    },
    --[[ -- Rosetta Trail Module
        {   
        Name = "RosettaTrail",
        Parent = "67P",
        Renderable = {
            Type = "RenderableTrail",
            Body = "ROSETTA",
            Frame = "GALACTIC",
            Observer = "SUN",
            -- 3 Dummy values for compilation:
            TropicalOrbitPeriod = 10000.0,
            EarthOrbitRatio = 2,
            DayLength = 50,
            -- End of Dummy values
            RGB = { 0.7, 0.7, 0.4 },
            Textures = {
                Type = "simple",
                Color = "textures/glare.png"
            },  
        },
        GuiName = "RosettaTrail"
    }, --]]
    -- Comet Dance trail
    --[[{   
        Name = "RosettaCometTrail",
        Parent = "67P",
        Renderable = {
            Type = "RenderableTrail",
            Body = "ROSETTA",
            Frame = "GALACTIC",
            Observer = "CHURYUMOV-GERASIMENKO",
            TropicalOrbitPeriod = 20000.0,
            EarthOrbitRatio = 2,
            DayLength = 25,
            RGB = { 0.9, 0.2, 0.9 },
            Textures = {
                Type = "simple",
                Color = "textures/glare.png"
            },  
            StartTime = "2014 AUG 01 12:00:00",
            EndTime = "2016 MAY 26 12:00:00"
        },
        GuiName = "RosettaCometTrail"
    },
    ]]
    {   
        Name = "RosettaCometTrail",
        Parent = "67PBarycenter",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "ROSETTA",
            Frame = "GALACTIC",
            Observer = "CHURYUMOV-GERASIMENKO",
            -- Optional rendering properties
            LineColor = { 0.9, 0.2, 0.9 },
            PointColor = { 0.9, 0.2, 0.9 },
            LineFade = 0.0, -- [0,1]
            RenderPart = 0.5, -- [0,1]
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2014 AUG 01 12:00:00",
                End = "2016 MAY 26 12:00:00"
            },
            SampleDeltaTime = 3600, -- Seconds between each point
            SubSamples = 3,
        },
        GuiName = "/Solar/RosettaCometTrail"
    },
    {
        Name = "NAVCAM",
        Parent = "Rosetta",
        -- Transform = {
        --     Rotation = {
        --         Type = "SpiceRotation",
        --         SourceFrame = "NAVCAM",
        --         DestinationFrame = "ROS_SPACECRAFT",
        --     },
        -- },
        GuiName = "/Solar/Rosetta_navcam"
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
            }
        },
        GuiName = "/Solar/Rosetta_navcam"
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
        }
    }
}
