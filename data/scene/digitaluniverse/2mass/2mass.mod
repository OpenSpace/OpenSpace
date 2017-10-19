return {
    -- 2MASS Galaxies
    {   
        Name = "2MASS Galaxies",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 0.4, 0.2 },
            Transparency = 1.0,
            File = "speck/2MASS.speck",
            Texture = "textures/point3.png",
            ColorMap = "speck/lss.cmap",
            ColorOption = { "redshift", "prox5Mpc" },
            ColorRange = { { 0.0, 0.075 }, { 1.0, 50.0 } },
            Unit = "Mpc",
            ScaleFactor = 508.0
        },
        GuiPath = "/Universe/Galaxies"
    }
}