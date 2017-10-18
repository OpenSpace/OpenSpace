return {
    -- Open Clusters module
    {   
        Name = "Open Star Clusters",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 0.1, 0.8, 0.4 },
            Transparency = 0.5,
            File = "speck/oc.speck",
            Texture = "textures/point4.png",
            PolygonSides = 12,
            TextColor = { 0.05, 0.4, 0.2, 1.0 },
            LabelFile = "speck/oc.label",
            ScaleFactor = 418.33,
            TextSize = 16.68,
            TextMinSize = 4.5,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Open Clusters"
    }
}
