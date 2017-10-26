return {
    -- Dwarfs module
    {   
        Name = "Dwarfs",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = { 0.5, 1.0, 0.2 },
            Transparency = 0.999,
            File = "speck/dwarfs.speck",
            Texture = "textures/point3.png",
            LabelFile = "speck/dwarfs.label",
            TextColor = { 0.5, 0.1, 0.2, 1.0 },
            TextSize = 14.6,
            TextMinSize = 10.0,
            ScaleFactor = 360,
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Brown Dwarfs"
    }
}
