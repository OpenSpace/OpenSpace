return {
    -- Stars Labels module
    {
        Name = "Stars Labels",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 0.65,
            LabelFile = "speck/stars.label",
            TextColor = { 0.4, 0.4, 0.4, 1.0 },
            DrawLabels = true,
            TextSize = 14.7,
            TextMinSize = 6.0,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Stars"
    }
}
