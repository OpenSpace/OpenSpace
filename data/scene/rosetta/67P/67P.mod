return {
    -- Comet 67P Body module
    {   
        Name = "67PBarycenter",
        Parent = "SolarSystemBarycenter", 
        Transform = {
            Translation = {
                Type = "SpiceEphemeris",
                Body = "CHURYUMOV-GERASIMENKO",
                Reference = "GALACTIC",
                Observer = "SUN",
                Kernels = {
                    --needed
                    "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp",
                    -- SPK  
                    --long term orbits loaded first
                    '${OPENSPACE_DATA}/spice/RosettaKernels_New/SPK/LORL_DL_009_02____P__00268.BSP',
                    '${OPENSPACE_DATA}/spice/RosettaKernels_New/SPK/RORL_DL_009_02____P__00268.BSP',
                    '${OPENSPACE_DATA}/spice/RosettaKernels_New/SPK/CORL_DL_009_02____P__00268.BSP',

                    '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/LORL_DL_006_01____H__00156.BSP',
                    '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/RORL_DL_006_01____H__00156.BSP',
                    '${OPENSPACE_DATA}/spice/RosettaKernels/SPK/CORL_DL_006_01____H__00156.BSP',
                    
                    --Jan 2014 - May 2015 (version match with 00162 ck files)
                    "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/CORB_DV_097_01_______00162.BSP",
                    "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/RORB_DV_097_01_______00162.BSP",
                    "${OPENSPACE_DATA}/spice/RosettaKernels/SPK/LORB_DV_097_01_______00162.BSP",

                    "${OPENSPACE_DATA}/spice/RosettaKernels_New/SPK/CORB_DV_211_01_______00288.BSP",
                    "${OPENSPACE_DATA}/spice/RosettaKernels_New/SPK/RORB_DV_211_01_______00288.BSP",
                    "${OPENSPACE_DATA}/spice/RosettaKernels_New/SPK/LORB_DV_211_01_______00288.BSP",
                    }
            },
        },
        GuiName = "/Solar/67PBarycenter",
    },
    {   
        Name = "67P",
        Parent = "67PBarycenter", 

        Renderable = {
            Type = "RenderableModelProjection",
            Body = "CHURYUMOV-GERASIMENKO",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "obj/67P_rotated_5_130.obj",
                Magnification = 0,
            }, 
            Textures = {
                Type = "simple",
                Color = "textures/gray.jpg",
                Project = "textures/defaultProj.png",
                Default = "textures/defaultProj.png"
            },
            Rotation = {
                Source = "67P/C-G_CK",
                Destination = "GALACTIC"
            },
            Projection = {
                Sequence   = "rosettaimages",
                SequenceType = "image-sequence",
                Observer   = "ROSETTA",
                Target     = "CHURYUMOV-GERASIMENKO",
                Aberration = "NONE",
                TextureMap = true
            },
            DataInputTranslation = {
                Instrument = {
                    NAVCAM = {
                        DetectorType  = "Camera",
                        Spice  = {"ROS_NAVCAM-A"},
                    },  
                },                  
                Target = { 
                    Read  = {
                        "TARGET_NAME",
                        "INSTRUMENT_HOST_NAME",
                        "INSTRUMENT_ID", 
                        "START_TIME", 
                        "STOP_TIME", 
                    },
                    Convert = {
                        CHURYUMOV               = {"CHURYUMOV-GERASIMENKO"},
                        ROSETTA                             = {"ROSETTA"              },
                        --NAVCAM                                = {"NAVCAM"},
                        ["ROSETTA-ORBITER"]                        = {"ROSETTA"              },
                        CHURYUMOVGERASIMENKO11969R1           = {"CHURYUMOV-GERASIMENKO"},
                        CHURYUMOVGERASIMENKO           = {"CHURYUMOV-GERASIMENKO"},
                        ["CHURYUMOV-GERASIMENKO1(1969R1)"] = {"CHURYUMOV-GERASIMENKO"},
                        --NAVIGATIONCAMERA                  = {"NAVCAM"               },
                    },
                },
            },

            Instrument = {                
                Name       = "ROS_NAVCAM-A",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
                Fovy       = 5.00,
                Aspect     = 1
            },
            BoundingSphereRadius = 5000.0
        },
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "67P/C-G_CK",
                DestinationFrame = "GALACTIC",
            },
        },
        GuiName = "/Solar/67P",
    },
    -- 67P Trail Module
    {   
        Name = "67PTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "CHURYUMOV-GERASIMENKO",
            Frame = "GALACTIC",
            Observer = "SUN",
            -- Optional rendering properties
            LineColor = { 0.1, 0.9, 0.2 },
            PointColor = { 0.1, 0.9, 0.2 },
            LineFade = 0.5, -- [0,1]
            RenderPart = 0.5,
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2014 JAN 01 00:00:00.000",
                End = "2017 JAN 01 00:00:00.000",
            },
            SampleDeltaTime = 3600, -- Seconds between each point
            SubSamples = 0,
        },
        GuiName = "/Solar/67PTrail"
    },
    --[[
    {   
        Name = "67PTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "CHURYUMOV-GERASIMENKO",
            Frame = "GALACTIC",
            Observer = "SUN",
            
            -- 3 Dummy values for compilation:
            TropicalOrbitPeriod = 1000.0,
            EarthOrbitRatio = 2,
            DayLength = 50,
            -- End of Dummy values
            
            RGB = { 0.1, 0.9, 0.2 },
            Textures = {
                Type = "simple",
                Color = "textures/glare.png"
            },  
        },
        GuiName = "/Solar/67PTrail"
    }
    ]]
}