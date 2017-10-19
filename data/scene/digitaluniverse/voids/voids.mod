return {
    -- Voids module
    {
        Name = "Voids",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 0.65,
            --ScaleFactor = 10.0,
            LabelFile = "speck/voids.label",
            TextColor = { 0.0, 0.4, 0.7, 1.0 },
            TextSize = 21.9,
            TextMinSize = 8.0,
            Unit = "Mpc"
        },
        GuiPath = "/Universe/Galaxies"
    },
}
