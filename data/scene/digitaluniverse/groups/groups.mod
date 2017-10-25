return {
    -- Nearby Galaxy Groups module
    {   
        Name = "Nearby Galaxy Groups",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 0.65,
            --ScaleFactor = 10.0,
            LabelFile = "speck/groups.label",
            TextColor = {0.1, 0.6, 0.2, 1.0},
            TextSize = 21.5,
            TextMinSize = 8.0,
            Unit = "Mpc",
        },
        GuiPath = "/Universe/Galaxies"
    }
}
