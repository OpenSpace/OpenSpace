return {
    -- Saturn barycenter module
    {
        Name = "SaturnBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "SATURN BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        GuiPath = "/Solar System/Planets/Saturn"
    },

    -- Saturn module
    {   
        Name = "Saturn",
        Parent = "SaturnBarycenter",
        Renderable = {
            Type = "RenderableGlobe",
            Radii = { 60268000, 60268000, 54364000 },
            SegmentsPerPatch = 64,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Saturn Texture",
                        FilePath = "textures/saturn.jpg",
                        Enabled = true
                    }
                }
            }
        },
        Tag = { "planet_solarSystem", "planet_giants" },
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_SATURN",
                DestinationFrame = "GALACTIC"
            }
        },
        GuiPath = "/Solar System/Planets/Saturn"
    },
    {
        Name = "SaturnRings",
        Parent = "Saturn",
        Renderable = {
            Type = "RenderableRings",
            Texture = "textures/saturn_rings.png",
            Size = 140220000,
            Offset = { 74500 / 140445.100671159, 1.0 } -- min / max extend
        },
        GuiPath = "/Solar System/Planets/Saturn"
    },
    -- SaturnTrail module
    {   
        Name = "SaturnTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "SATURN BARYCENTER",
                Observer = "SUN"
            },
            Color = {0.85,0.75,0.51 },
            Period = 10746.94,
            Resolution = 1000,
        },
        Tag = { "planetTrail_solarSystem", "planetTrail_giants" },
        GuiPath = "/Solar System/Planets/Saturn"
    }
}
