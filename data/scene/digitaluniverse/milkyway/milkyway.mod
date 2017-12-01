return {
    -- MilkyWay Galaxy (Texture)
    {   
        Name = "MilkyWay Pic",
        Parent = "Root",
        Renderable = {
            Type = "RenderablePlanesCloud",
            Enabled = true,
            Color = { 1.0, 1.0, 1.0 },
            Transparency = 0.90,
            ScaleFactor = 2.3,
            File = "speck/galaxy.speck",
            TexturePath = "textures",
            Luminosity = "size",
            ScaleLuminosity = 1.0,
            --[[
            TransformationMatrix = {
                -0.7357425748,    0.67726129641,  0.0,           0.0,
                -0.074553778365, -0.080991471307, 0.9939225904,  0.0,
                0.67314530211,    0.73127116582,  0.11008126223, 0.0,
                0.0,              0.0,            0.0,           1.0
                },
            ]]
            -- Fade in value in the same unit as "Unit"
            FadeInThreshould = 0.1,
            PlaneMinSize = 5.0,
            Unit = "pc",
        },        
        GuiPath = "/Universe/Galaxies"
    }
}
