return {
    -- SloanDDS module
    {
        Name = "Sloan Digital Sky Survey",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = { 0.8, 0.8, 1.0 },
            Transparency = 1.0,
            ScaleFactor = 507.88,
            File = "speck/SDSSgals.speck",
            ColorMap = "speck/lss.cmap",
            ColorOption = {"redshift", "prox5Mpc"},
            ColorRange = { { 0.0, 0.075 }, { 1.0, 50.0 } },
            Texture = "textures/point3.png",
            Unit = "Mpc"
        },
        GuiPath = "/Universe/Galaxies"
    }
}
