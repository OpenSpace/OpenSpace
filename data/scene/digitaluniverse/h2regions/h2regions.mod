return {
    -- HII Regions module
    {   
        Name = "HII Regions",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 0.0, 0.5, 1.0 },
            Transparency = 0.35,
            File = "speck/h2.speck",
            Texture = "textures/point4.png",
            PolygonSides = 6,
            LabelFile = "speck/h2.label",
            TextColor = { 0.5, 0.5, 0.5, 1.0 },
            ScaleFactor = 420,
            TextSize = 17.25,
            TextMinSize = 2.0,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/HII"
    }
}
