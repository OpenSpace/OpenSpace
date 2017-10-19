return {
    -- 2dF Galaxies
    {
        Name = "2dF Galaxies",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 1.0,
            File = "speck/2dF.speck",
            Texture = "textures/point3.png",
            ColorMap = "speck/lss.cmap",
            ColorOption = { "redshift", "prox5Mpc" },
            ColorRange = { { 0.0, 0.075 }, { 1.0, 50.0 } },
            --ColorOption = {"prox5Mpc"},
            --ColorRange = {{1.0, 50.0}},             
            Unit = "Mpc",
            ScaleFactor = 508.0
        },
        GuiPath = "/Universe/Galaxies"
    }
}
