return {
    {
        Name = "Iapetus",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_IAPETUS",
            Body = "IAPETUS",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.746, 3 },
                Segments = 50
            },
            Textures = {
                Color = "textures/iapetus.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "IAPETUS",
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
        Name = "IapetusTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "IAPETUS",
            Frame = "GALACTIC",
            Observer = "SATURN BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod = 60,
            EarthOrbitRatio = 0.1,
            DayLength = 0.9424218
        }
    }
}