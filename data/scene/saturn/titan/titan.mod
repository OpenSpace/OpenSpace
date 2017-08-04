return {
    {
        Name = "Titan",
        Parent = "SaturnBarycenter",
        -- SphereOfInfluency unit is meters                
		SphereOfInfluency = 2.5E+7,
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_TITAN",
            Body = "TITAN",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 0.2575E4,
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
                DestinationFrame = "GALACTIC"
            }
        }
    },
    {
        Name = "TitanTrail",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "TITAN",
                Observer = "SATURN BARYCENTER",
            },
            Color = { 0.5, 0.3, 0.3 },
            Period = 16,
            Resolution = 1000
        }
    }
}