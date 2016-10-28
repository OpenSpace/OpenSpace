return {
    {
        Name = "Mimas",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_MIMAS",
            Body = "MIMAS",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.28, 3 },
                Segments = 50
            },
            Textures = {
                Color = "textures/mimas.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MIMAS",
                Observer = "SATURN BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/sat375.bsp"
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MIMAS",
                DestinationFrame = "IAU_JUPITER"
            }
        }
    },
    {
        Name = "MimasTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "MIMAS",
            Frame = "GALACTIC",
            Observer = "SATURN BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod = 60,
            EarthOrbitRatio = 0.0025,
            DayLength = 0.9424218
        }
    }
}