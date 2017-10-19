return {
    -- Galaxy Superclusters module
    {
        Name = "Galaxy Superclusters",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 0.65,
            File = "speck/superclust.speck",
            Texture = "textures/point3.png",
            LabelFile = "speck/superclust.label",
            TextColor = { 0.6, 0.6, 0.6, 1.0 },
            ScaleFactor = 531.0,
            TextSize = 22.44,
            TextMinSize = 8.0,
            Unit = "Mpc",
        },
        GuiPath = "/Universe/Galaxies"
    }
}
