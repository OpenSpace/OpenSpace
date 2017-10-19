return {
    -- Constellations module
    {
        Name = "Constellations (Extragalactic)",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            Color = { 1.0, 0.4, 0.2 },
            Transparency = 1.0,
            ScaleFactor = 1.0,
            File = "speck/constellationsEXGAL.speck",
            LabelFile = "speck/constellationsEXGAL.label",
            TextColor = { 0.8, 0.8, 0.8, 1.0 },
            TextSize = 15.5,
            TextMinSize = 8.0,
            MeshColor = { { 0.6, 0.4, 0.4 }, { 0.8, 0.0, 0.0 }, { 0.0, 0.3, 0.8 } },
            Unit = "Mpc",
        },
        GuiPath = "/Milky Way/Constellations"
    },
    {
        Name = "Constellations",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            Color = { 1.0, 0.4, 0.2 },
            Transparency = 1.0,
            ScaleFactor = 1.0,
            File = "speck/constellations.speck",
            LabelFile = "speck/constellations.label",
            TextColor = { 0.8, 0.8, 0.8, 1.0 },
            TextSize = 15.5,
            TextMinSize = 8.0,
            MeshColor = { { 0.6, 0.4, 0.4 }, { 0.8, 0.0, 0.0 }, { 0.0, 0.3, 0.8 } },
            Unit = "pc",
        },
        GuiPath = "/Milky Way/Constellations"
    }
}
