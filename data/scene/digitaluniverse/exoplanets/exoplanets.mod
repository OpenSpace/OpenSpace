return {
    -- Exoplanets module
    {
        Name = "Exoplanets",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 0.65,
            ScaleFactor = 10.0,
            Texture = "textures/target-blue.pbm",
            File = "speck/expl.speck",
            LabelFile = "speck/expl.label",
            ScaleFactor = 380.0,
            TextColor = { 0.3, 0.3, 0.8, 1.0 },
            TextSize = 14.8,
            TextMinSize = 10.0,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Exoplanets"
    }
}
