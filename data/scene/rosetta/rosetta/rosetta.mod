return {
  -- Rosetta Body module
    {
        Name = "Rosetta",
        Parent = "SolarSystemBarycenter", 
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA", 
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "obj/mainbodyros.obj",   
                Magnification = 1,
            },
            Textures = {
                Type = "simple",
                Color = "textures/gray.png",
            },
            Rotation = {
                Source = "ROS_SPACECRAFT",
                Destination = "J2000"
            },
        },
        Ephemeris = {
            Type = "Spice",
            Body = "ROSETTA",
            Reference = "GALACTIC",
            Observer = "SUN",
            Kernels = {
                --needed
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp",
                -- SPK  
                --long term orbits loaded first
                '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/LORL_DL_006_01____H__00156.BSP',
                '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/RORL_DL_006_01____H__00156.BSP',
                '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/CORL_DL_006_01____H__00156.BSP',

                --Jan 2014 - May 2015 (version match with 00162 ck files)
                "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/CORB_DV_097_01_______00162.BSP",
                "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/RORB_DV_097_01_______00162.BSP",
                "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/LORB_DV_097_01_______00162.BSP",

                --IK
                "${OPENSPACE_DATA}/spice/RosettaKernels/IK/ROS_NAVCAM_V00-20130102.TI",

                --SCLK
                "${OPENSPACE_DATA}/spice/RosettaKernels/SCLK/ROS_150227_STEP.TSC",

                -- FK
                "${OPENSPACE_DATA}/spice/RosettaKernels/FK/ROS_CHURYUMOV_V01.TF",
                "${OPENSPACE_DATA}/spice/RosettaKernels/FK/ROS_V24.TF",

                -- CK
                '${OPENSPACE_DATA}/spice/RosettaKernels/CK/RATT_DV_097_01_01____00162.BC',
                "${OPENSPACE_DATA}/spice/RosettaKernels/CK/CATT_DV_097_01_______00162.BC",

                "${OPENSPACE_DATA}/spice/RosettaKernels/CK/ROS_SA_2014_V0044.BC",
                "${OPENSPACE_DATA}/spice/RosettaKernels/CK/ROS_SA_2015_V0009.BC",
            }
        },
        GuiName = "/Solar/Rosetta"
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
    {   
        Name = "RosettaCometTrail",
        Parent = "67P",
        Renderable = {
            Type = "RenderableTrail",
            Body = "ROSETTA",
            Frame = "GALACTIC",
            Observer = "CHURYUMOV-GERASIMENKO",
            -- 3 Dummy values for compilation:
            TropicalOrbitPeriod = 20000.0,
            EarthOrbitRatio = 2,
            DayLength = 25,
            -- End of Dummy values
            RGB = { 0.9, 0.2, 0.9 },
            Textures = {
                Type = "simple",
                Color = "textures/glare.png"
            },  
            StartTime = "2014 AUG 01 12:00:00",
            EndTime = "2015 MAY 26 12:00:00"
        },
        GuiName = "RosettaCometTrail"
    },

    -- Rosetta Solar Array module 1
    {   
        Name = "RosettaSolar1",
        Parent = "Rosetta",  
        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA", 
            Geometry = {
                Type = "MultiModelGeometry",
                Magnification = 1,    
                GeometryFile = "obj/solarpanelleft.obj", 
            }, 
            Textures = {
                Type = "simple",
                Color = "textures/gray.png",
            },
            Rotation = {
                Source = "ROS_SA+Y",
                Destination = "ROS_SPACECRAFT"
            },
        },
    },

    -- Rosetta Solar Array module 2
    {   
        Name = "RosettaSolar2",
        Parent = "Rosetta", 

        Renderable = {
            Type = "RenderableModel",
            Body = "ROSETTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "obj/solarpanelright.obj", 
                Magnification = 1,
            },
            Textures = {
                Type = "simple",
                Color = "textures/gray.png",
            },
            Rotation = {
                Source = "ROS_SA-Y",
                Destination = "ROS_SPACECRAFT"
            },
        },
    },

    -- RosettaFov main module
    {   
        Name = "NAVCAM",
        Parent = "Rosetta",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "ROSETTA",
            Frame = "GALACTIC",
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
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderablePlaneProjection",
            Frame = "67P/C-G_CK",
            DefaultTarget = "CHURYUMOV-GERASIMENKO",
            Spacecraft = "ROSETTA",
            Instrument = "ROS_NAVCAM-A",
            Moving = false,
            Texture = "textures/defaultProj.png",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        }, 
    },
        -- NAVCAM FoV square
    {
        Name = "FovImagePlane",
        Parent = "Rosetta",
        Renderable = {
            Type = "RenderablePlaneProjection",
            Frame = "67P/C-G_CK",
            DefaultTarget = "CHURYUMOV-GERASIMENKO",
            Spacecraft = "ROSETTA",
            Instrument = "ROS_NAVCAM-A",
            Moving = true,
            Texture = "textures/squarefov.png",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        },
    }

}
