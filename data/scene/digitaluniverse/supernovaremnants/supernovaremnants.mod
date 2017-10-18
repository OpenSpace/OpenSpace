return {
    -- Supernova Remnants module
    {
        Name = "Supernova Remnants",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 0.5, 0.0 },
            Transparency = 0.5,
            File = "speck/snr.speck",
            Texture = "textures/point4.png",
            PolygonSides = 7,
            LabelFile = "speck/snr.label",
            TextColor = { 0.6, 0.3, 0.0, 1.0 },
            ScaleFactor = 440.08,
            TextSize = 17.5,
            TextMinSize = 8.0,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Supernova Remnants"
    }
}
