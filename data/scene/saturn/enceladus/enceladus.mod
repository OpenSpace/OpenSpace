return {
    {
        Name = "Enceladus",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_ENCELADUS",
            Body = "ENCELADUS",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.257, 3 },
                Segments = 50
            },
            Textures = {
                Color = "textures/enceladus.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "ENCELADUS",
                Observer = "SATURN BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/sat375.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_ENCELADUS",
                DestinationFrame = "GALACTIC"
            }
        }
    },
    {
        Name = "EnceladusTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "ENCELADUS",
                Observer = "SATURN BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period = 33 / 24,
            Resolution = 1000
        }
    }
}