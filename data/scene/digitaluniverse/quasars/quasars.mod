return {
    -- Quasars module
    {
        Name = "Quasars",
        Parent = "Root",
        Renderable = {
            Type = "RenderableBillboardsCloud",
            Enabled = true,
            Color = { 1.0, 0.4, 0.2 },
            Transparency = 1.0,
            File = "speck/quasars.speck",
            Texture = "textures/point3.png",
            Unit = "Mpc",
            ScaleFactor = 537.31,
            -- Fade in value in the same unit as "Unit"
            FadeInThreshould = 1000.0,
            BillboardMaxSize = 200.0,
            BillboardMinSize = 0.5,
        },
        GuiPath = "/Universe/Quasars"
    }
}
