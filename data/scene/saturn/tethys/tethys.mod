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
                Radius = { 0.538, 3 },
                Segments = 50
            },
            Textures = {
                Color = "textures/tethys.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "TETHYS",
                Observer = "SATURN BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/sat375.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_ENCELADUS",
                DestinationFrame = "IAU_JUPITER"
            }
        }
    },
    {
        Name = "TethysTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "TETHYS",
            Frame = "GALACTIC",
            Observer = "SATURN BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod = 60,
            EarthOrbitRatio = 0.005,
            DayLength = 0.9424218
        }
    }
}