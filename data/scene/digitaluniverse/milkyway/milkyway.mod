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
            ScaleFactor = 2.8,
            File = "speck/galaxy.speck",
            TexturePath = "textures",
            Luminosity = "size",
            ScaleLuminosity = 1.0,           
            -- Fade in value in the same unit as "Unit"
            FadeInThreshould = 119441,
            PlaneMinSize = 5.0,
            Unit = "pc",
        },        
        GuiPath = "/Universe/Galaxies"
    }
}
