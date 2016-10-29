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
            Type = "RenderableTrail",
            Body = "ENCELADUS",
            Frame = "GALACTIC",
            Observer = "SATURN BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod = 60,
            EarthOrbitRatio = 0.005,
            DayLength = 0.9424218
        }
    }
}