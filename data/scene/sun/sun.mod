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
                --Radius = 2.783200E9,
                Radius = 6.957E8,
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/sun.jpg",
            },
            PerformShading = false,
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "SUN",
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
            Size = 1.3*10^10.5,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/sun-glare.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "SUN",
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
            Size = 3.0E11,
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
