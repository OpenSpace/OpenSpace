return {
    -- Pulsars module
    {
        Name = "Pulsars",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = { 0.7, 0.0, 0.0 },
            Transparency = 0.5,
            File = "speck/pulsar.speck",
            Texture = "textures/point4.png",
            PolygonSides = 4,
            LabelFile = "speck/pulsar.label",
            TextColor = { 0.7, 0.0, 0.0, 1.0 },
            ScaleFactor = 418.33,
            TextSize = 16.68,
            TextMinSize = 4.5,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Pulsars"
    }
}
