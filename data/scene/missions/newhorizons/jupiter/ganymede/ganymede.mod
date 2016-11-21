return {
    -- Ganymede module
    {   
        Name = "Ganymede",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Frame = "IAU_GANYMEDE", 
            Body = "GANYMEDE",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 1.8213, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/ganymede.jpg",
                Project = "textures/defaultProj.png",
                Sequencing = "true",
            },
            Projection = {
                Observer   = "NEW HORIZONS",
                Target     = "GANYMEDE",
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
                Body = "GANYMEDE",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_GANYMEDE",
                DestinationFrame = "ECLIPJ2000",
            },
        },
    },
    {
        Name = "GanymedeText",
        Parent = "Ganymede",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 7.4},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Ganymede-Text.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, -10000000, 0}
            },
        },
    },    
    -- GanymedeTrail module
    {   
        Name = "GanymedeTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "GANYMEDE",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.4, 0.3, 0.3 },
            Period =  172 / 24,
            Resolution = 1000
        }
    }
}
