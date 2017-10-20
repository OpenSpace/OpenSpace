return {
    -- Galaxy Clusters module
    {   
        Name = "Galaxy Cluster Labels",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.65,
            LabelFile = "speck/galclust.label",
            TextColor = {0.7, 0.3, 0.0, 1.0},
            DrawLabels = true,
            TextSize = 22,
            TextMinSize = 8.0,
            Unit = "Mpc"
        },
        GuiPath = "/Universe/Galaxies"
    }
}
