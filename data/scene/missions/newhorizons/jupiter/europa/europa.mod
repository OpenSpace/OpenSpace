local europa_radius = 1.8213E6

return {
    -- Europa module
    {   
        Name = "Europa",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Frame = "IAU_EUROPA", 
            Body = "EUROPA",
            Radius = europa_radius,
            Geometry = {
                Type = "SimpleSphere",
                Radius = europa_radius,
                Segments = 100
            },
            ColorTexture = "textures/europa.jpg",
            Textures = {
                Type = "simple",
                Project = "textures/defaultProj.png",
                Sequencing = "true",
            },
            Projection = {
                Observer   = "NEW HORIZONS",
                Target     = "EUROPA",
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
                Target = "EUROPA",
                Observer = "JUPITER BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_EUROPA",
                DestinationFrame = "ECLIPJ2000",
            },
        },
        GuiPath = "/Solar System/Planets/Jupiter/Moons"
    },
    {
        Name = "EuropaText",
        Parent = "Europa",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10^7.4,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Europa-Text.png",
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
    -- EuropaTrail module
    {   
        Name = "EuropaTrail",
        Parent = "JupiterBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "EUROPA",
                Observer = "JUPITER BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period =  85 / 24,
            Resolution = 1000
        },
        GuiPath = "/Solar System/Planets/Jupiter/Moons"
    }
}
