return {
    -- Globular Clusters module
    {
        Name = "Globular Clusters",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 0.8, 0.8, 0.0 },
            Transparency = 0.35,
            File = "speck/gc.speck",
            Texture = "textures/point4.png",
            PolygonSides = 5,
            LabelFile = "speck/gc.label",
            TextColor = { 0.5, 0.5, 0.0, 1.0 },
            ScaleFactor = 440.0,
            TextSize = 17.5,
            TextMinSize = 10.0,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Globular Clusters"
    }
}
