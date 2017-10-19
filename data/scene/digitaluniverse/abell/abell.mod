return {
    -- Abell Galaxies module
    {   
        Name = "Abell Galaxy Clusters",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Color = { 1.0, 0.4, 0.2 },
            Transparency = 1.0,
            ScaleFactor = 525.0,
            File = "speck/abell.speck",
            Texture = "textures/point3.png",
            LabelFile = "speck/abell.label",
            TextColor = { 0.0, 0.8, 0.0, 1.0 },
            TextSize = 22,
            TextMinSize = 10.0,
            Unit = "Mpc",
            TransformationMatrix = {
                -0.7357425748,    0.67726129641,  0.0,           0.0,
                -0.074553778365, -0.080991471307, 0.9939225904,  0.0,
                0.67314530211,    0.73127116582,  0.11008126223, 0.0,
                0.0,              0.0,            0.0,           1.0
            },
        },
        --[[
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
        ]]--
        GuiPath = "/Universe/Galaxies"
    }
}
