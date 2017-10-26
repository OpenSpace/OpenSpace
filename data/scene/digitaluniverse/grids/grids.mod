return {
    {
        Name = "Ecliptic Grid Labels",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.65,
            --ScaleFactor = 10.0,
            LabelFile = "speck/eclip.label",
            TextColor = {0.5, 0.5, 0.5, 1.0},
            TextSize = 15.4,
            TextMinSize = 5.0,
            Unit = "pc",
        },
        Transform = {
            Rotation = {
                Type = "StaticRotation",
                Rotation = {
                    -0.05487554,  0.4941095, -0.8676661,
                    -0.9938214 , -0.1109906, -0.0003515167,
                    -0.09647644,  0.8622859,  0.4971472
                }
            }
        },
        GuiPath = "/Other/Grids"
    },
    {
        Name = "Equatorial Grid Labels",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.65,
            --ScaleFactor = 10.0,
            LabelFile = "speck/radec.label",
            TextColor = {0.5, 0.5, 0.5, 1.0},
            TextSize = 15.3,
            TextMinSize = 5.0,
            Unit = "pc",
        },
        GuiPath = "/Other/Grids"
    },
    -- Galactic Grid Labels
    {   
        Name = "Galactic Grid Labels",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.65,
            --ScaleFactor = 10.0,
            LabelFile = "speck/galac.label",
            TextColor = {0.5, 0.5, 0.5, 1.0},
            TextSize = 16.25,
            TextMinSize = 5.0,
            Unit = "pc",
        },
        GuiPath = "/Other/Grids"
    },
    -- Plane Grids
    {   
        Name = "100kly Grid",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.4,
            ScaleFactor = 1.0,
            File = "speck/100kly.speck",
            MeshColor = {{0.1, 0.5, 0.6}},
            LabelFile = "speck/100kly.label",
            TextColor = {0.0, 0.2, 0.5, 1.0},
            TextSize = 18.6,
            TextMinSize = 7.0,
            Unit = "Mpc",
        },
        GuiPath = "/Other/Grids"
    },
    {
        Name = "1Mly Grid",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.4,
            ScaleFactor = 1.0,
            File = "speck/1Mly.speck",
            MeshColor = {{0.1, 0.5, 0.6}},
            LabelFile = "speck/1Mly.label",
            TextColor = {0.0, 0.2, 0.5, 1.0},
            TextSize = 19.6,
            TextMinSize = 7.0,
            Unit = "Mpc",
        },
        GuiPath = "/Other/Grids"
    },
    {   
        Name = "10Mly Grid",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.4,
            ScaleFactor = 1.0,
            File = "speck/10Mly.speck",
            MeshColor = {{0.1, 0.5, 0.6}},
            LabelFile = "speck/10Mly.label",
            TextColor = {0.0, 0.2, 0.5, 1.0},
            TextSize = 20.6,
            TextMinSize = 7.0,
            Unit = "Mpc",
        },
        GuiPath = "/Other/Grids"
    },
    {
        Name = "100Mly Grid",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.4,
            ScaleFactor = 1.0,
            File = "speck/100Mly.speck",
            MeshColor = {{0.1, 0.5, 0.6}},
            LabelFile = "speck/100Mly.label",
            TextColor = {0.0, 0.2, 0.5, 1.0},
            TextSize = 21.6,
            TextMinSize = 7.0,
            Unit = "Mpc",
        },
        GuiPath = "/Other/Grids"
    },
    {
        Name = "20Gly Grid",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            Enabled = false,
            Color = {1.0, 1.0, 1.0},
            Transparency = 0.4,
            ScaleFactor = 1.0,
            File = "speck/20Gly.speck",
            MeshColor = {{0.1, 0.5, 0.6}},
            LabelFile = "speck/20Gly.label",
            TextColor = {0.0, 0.2, 0.5, 1.0},
            TextSize = 23.6,
            TextMinSize = 7.0,
            Unit = "Mpc",
        },
        GuiPath = "/Other/Grids"
    },
}
