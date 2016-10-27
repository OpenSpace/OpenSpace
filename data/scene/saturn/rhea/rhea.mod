return {
    {
        Name = "Rhea",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_RHEA",
            Body = "RHEA",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.765, 3 },
                Segments = 50
            },
            Textures = {
                Color = "textures/rhea.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "RHEA",
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
        Name = "RheaTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "RHEA",
            Frame = "GALACTIC",
            Observer = "SATURN BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod = 60,
            EarthOrbitRatio = 0.01,
            DayLength = 0.9424218
        }
    }
}