return {
    {
        Name = "Titan",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_TITAN",
            Body = "TITAN",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.2575, 4 },
                Segments = 50
            },
            Textures = {
                Color = "textures/titan.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "TITAN",
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
        Name = "TitanTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "TITAN",
            Frame = "GALACTIC",
            Observer = "SATURN BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod = 60,
            EarthOrbitRatio = 0.05,
            DayLength = 0.9424218
        }
    }
}