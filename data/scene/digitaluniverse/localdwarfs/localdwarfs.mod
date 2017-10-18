return {
    -- Local Dwarfs module
    {   
        Name = "Local Dwarf Galaxies",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 0.5, 1.0, 0.2 },
            Transparency = 0.3,
            File = "speck/localdwarfs.speck",
            Texture = "textures/point4.png",
            PolygonSides = 12,
            LabelFile = "speck/localdwarfs.label",
            TextColor = { 0.3, 0.3, 1.0, 1.0 },
            ScaleFactor = 478,
            TextSize = 19.2,
            TextMinSize = 7.3,
            Unit = "Mpc",
        },
        GuiPath = "/Universe/Galaxies"
    }
}
