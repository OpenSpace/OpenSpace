return {
    -- 6dF Galaxies
    {
        Name = "6dF Galaxies",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 1.0,
            File = "speck/6dF.speck",
            Texture = "textures/point3.png",
            ColorMap = "speck/lss.cmap",
            ColorOption = { "redshift", "prox5Mpc" },
            ColorRange = { { 0.0, 0.075 }, { 1.0, 50.0 } },
            Unit = "Mpc"
        },
        GuiPath = "/Universe/Galaxies"
    }
}
