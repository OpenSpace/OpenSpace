return {
    -- Solar system module
    {
        Name = "SolarSystem",
        Parent = "Root"
    },
    -- Sun barycenter module
    {
        Name = "SolarSystemBarycenter",
        Parent = "SolarSystem",
    },

    -- Sun module
    {
        Name = "Sun",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_SUN",
            Body = "SUN", 
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 2.783200, 9 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/sun.jpg",
            },
            PerformShading = false,
        },
        Transformation = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "SUN",
                Observer = "SSB",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_SUN",
                DestinationFrame = "GALACTIC"
            }
        }
    },

    {
        Name = "SunGlare",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.3, 10.5},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/sun-glare.png",
            BlendMode = "Additive"
        },
        Transformation = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "SUN",
                Observer = "SSB",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        }
    },

    {
        Name = "SunMarker",
        Parent = "Sun",
        Renderable = {
            Type = "RenderablePlane",
            Size = {3.0, 11.0},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/marker.png",
            BlendMode = "Additive"
        },
        Transformation = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, 0, 0, 5}
            }
        }
    }
}
