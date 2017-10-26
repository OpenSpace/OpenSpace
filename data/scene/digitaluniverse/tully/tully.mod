return {
    -- Tully Galaxies module (Points)
    {
        Name = "Tully Galaxies",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = { 1.0, 0.4, 0.2 },
            Transparency = 0.99,
            ScaleFactor = 502.77,
            File = "speck/tully.speck",
            Texture = "textures/point3.png",
            ColorMap = "speck/lss.cmap",
            ColorOption = { "prox5Mpc" },
            ColorRange = { { 1.0, 80.0 } },
            LabelFile = "speck/tully.label",
            TextColor = { 0.7, 0.7, 0.7, 1.0 },
            TextSize = 20.50,
            TextMinSize = 16.0,
            Unit = "Mpc"
        },
        Transform = {
            Rotation = {
                Type = "StaticRotation",
                Rotation = {
                    -0.7357425748,    0.67726129641,  0.0,
                    -0.074553778365, -0.080991471307, 0.9939225904,
                    0.67314530211,    0.73127116582,  0.11008126223
                }
            }
        },
        GuiPath = "/Universe/Galaxies"
    },

    -- Tully Galaxies module (Textures)
    {   
        Name = "Tully Galaxies Pics",
        Parent = "Root",
        Renderable = {
            Type = "RenderablePlanesCloud",
            Enabled = false,
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 0.99,
            ScaleFactor = 1.0,
            File = "speck/tully.speck",
            TexturePath = "textures",
            Luminosity = "diamkpc",
            ScaleLuminosity = 0.001,
            Unit = "Mpc",
        },
        Transform = {
            Rotation = {
                Type = "StaticRotation",
                Rotation = {
                    -0.7357425748,    0.67726129641,  0.0,
                    -0.074553778365, -0.080991471307, 0.9939225904,
                    0.67314530211,    0.73127116582,  0.11008126223
                }
            }
        },
        GuiPath = "/Universe/Galaxies"
    }
}
