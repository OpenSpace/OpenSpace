return {
    -- Alternate Stars Labels module
    {
        Name = "Stars Labels - Alternate",
        Parent = "Root",
        Renderable = {
            --Type = "RenderablePoints",
            Type = "RenderableBillboardsCloud",
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.65,
            --ScaleFactor = 10.0,
            --File = "speck/stars.speck",
            LabelFile = "speck/stars-altlbl.label",
            TextColor = { 0.4, 0.4, 0.4, 1.0 },
            DrawLabels = true,
            TextSize = 14.7,
            TextMinSize = 6.0,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Stars"
    }
}
