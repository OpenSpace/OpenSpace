return {
    -- Planetary Nebulae module
    {
        Name = "Planetary Nebulae",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = {0.4, 0.4, 0.9},
            Transparency = 0.35,
            File = "speck/pn.speck",
            Texture = "textures/point4.png",
            PolygonSides = 3,
            LabelFile = "speck/pn.label",
            TextColor = {0.25, 0.25, 0.65, 1.0},
            ScaleFactor = 418.33,
            TextSize = 16.68,
            TextMinSize = 4.5,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Planetary Nebulae"
    },
}
