return {
    -- Comet 67P Body module
    {   
        Name = "67PBarycenter",
        Parent = "SolarSystemBarycenter", 
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "CHURYUMOV-GERASIMENKO",
                Observer = "SUN",
            },
        },
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
                -- Color = "textures/may9_map.jpg",
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
                TextureMap = true,
                ShadowMap = true
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