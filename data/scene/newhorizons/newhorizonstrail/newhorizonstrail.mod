return {
    -- NewHorizonsTrail module
    {   
        Name = "NewHorizonsTrail",
        Parent = "Root",
        Renderable = {
            Type = "RenderableTrail",
            Body = "NEW HORIZONS",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.1,0.01,0.30 },
            TropicalOrbitPeriod = 6330.595 ,
            EarthOrbitRatio = 0.857,
            DayLength = 9.9259,
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },    
        },
        GuiName = "/Solar/NewHorizonsTrail"
    }
}