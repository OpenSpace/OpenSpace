return {
    -- Kepler module
    {   
        Name = "Kepler Planetary Candidates",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = { 1.0, 1.0, 0.0 },
            Transparency = 0.99,
            ScaleFactor = 395.0,
            File = "speck/kepler.speck",
            Texture = "textures/halo.png",
            Unit = "pc"
        },
        GuiPath = "/Milky Way/Exoplanets"
    }
}
