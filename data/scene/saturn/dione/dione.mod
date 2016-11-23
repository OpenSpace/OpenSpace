return {
    {
        Name = "Dione",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_DIONE",
            Body = "DIONE",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.563, 3 },
                Segments = 50
            },
            Textures = {
                Color = "textures/dione.jpg"
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "DIONE",
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
        Name = "DioneTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "DIONE",
            Frame = "GALACTIC",
            Observer = "SATURN BARYCENTER",
            RGB = { 0.5, 0.3, 0.3 },
            TropicalOrbitPeriod = 60,
            EarthOrbitRatio = 0.0075,
            DayLength = 0.9424218
        }
    }
}