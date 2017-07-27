return {
    {
        Name = "Tethys",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_TETHYS",
            Body = "TETHYS",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 0.538E3,
                Segments = 50
            },
            Textures = {
                Color = "textures/tethys.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "TETHYS",
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
        Name = "TethysTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "TETHYS",
                Observer = "SATURN BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period = 45 / 24,
            Resolution = 1000
        }
    }
}