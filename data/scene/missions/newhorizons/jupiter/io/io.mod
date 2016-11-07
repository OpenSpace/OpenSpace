return {
    -- Io module
    {   
        Name = "Io",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Frame = "IAU_IO", 
            Body = "IO",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 1.8213, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/io.jpg",
                Project = "textures/defaultProj.png",
                Sequencing = "true",
            },
            Projection = {
                Observer   = "NEW HORIZONS",
                Target     = "IO",
                Aberration = "NONE",
                AspectRatio = 2,

                Instrument = {
                    Name       = "NH_LORRI",
                    Method     = "ELLIPSOID",
                    Aberration = "NONE",
                    Fovy       = 0.2907,
                    Aspect     = 1,
                    Near       = 0.2,
                    Far        = 10000,
                },

                PotentialTargets = {
                    "JUPITER", "IO", "EUROPA", "GANYMEDE", "CALLISTO"
                }
            },
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "IO",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_IO",
                DestinationFrame = "ECLIPJ2000",
            },
        },
        --[[
        Ephemeris = {
            Type = "Spice",
            Body = "IO",
            Reference = "ECLIPJ2000",
            Observer = "JUPITER BARYCENTER",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_IO",
            Reference = "ECLIPJ2000"
        },
        ]]
    },
    {
        Name = "IoText",
        Parent = "Io",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 7.4},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Io-Text.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, -10000000, 0}
            },
        },
    },
    -- IoTrail module
    {   
        Name = "IoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "IO",
            Frame = "GALACTIC",
            Observer = "JUPITER BARYCENTER",
            RGB = { 0.7, 0.4, 0.2 },
            TropicalOrbitPeriod =  100 ,
            EarthOrbitRatio = 0.0045,
            DayLength = 9.9259,
            LineFade = 2.0,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
    }
}
