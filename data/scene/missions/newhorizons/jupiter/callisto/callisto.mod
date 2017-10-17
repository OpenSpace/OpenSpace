local callisto_radius = 1.8213E6

return {
    -- Callisto module
    {   
        Name = "Callisto",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Frame = "IAU_CALLISTO",
            Body = "CALLISTO",
            Radius = callisto_radius,
            Geometry = {
                Type = "SimpleSphere",
                Radius = callisto_radius,
                Segments = 100
            },
            ColorTexture = "textures/callisto.jpg",
            Textures = {
                Type = "simple",
                Project = "textures/defaultProj.png",
                Sequencing = "true",
            },
            Projection = {
                Observer   = "NEW HORIZONS",
                Target     = "CALLISTO",
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
                Target = "CALLISTO",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_CALLISTO",
                DestinationFrame = "ECLIPJ2000",
            },
        },
        GuiPath = "/Solar System/Planets/Jupiter/Moons"
    },
    {
        Name = "CallistoText",
        Parent = "Callisto",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10^7.4,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Callisto-Text.png",
            BlendMode = "Additive"
        },
        --[[
        Ephemeris = {
            Type = "Static",
            Position = {0, -1, 0, 7}
        }
        ]]
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, -10000000, 0}
            },
        },
        GuiPath = "/Solar System/Planets/Jupiter/Moons"
    },
    -- CallistoTrail module
    {   
        Name = "CallistoTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "CALLISTO",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.4, 0.3, 0.01 },
            Period =  17,
            Resolution = 1000
        },
        GuiPath = "/Solar System/Planets/Jupiter/Moons"
    }
}
