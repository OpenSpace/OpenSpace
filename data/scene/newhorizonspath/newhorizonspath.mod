return {
    -- NewHorizonsPath module
    {   
        Name = "NewHorizonsPath",
        Parent = "Root",
        Renderable = {
            Type = "RenderablePath",
            Body = "NEW HORIZONS",
            Frame = "GALACTIC",
            Observer = "SUN",
			RGB = { 0.8, 0.7, 0.7 },
		    Textures = {
                Type = "simple",
			    Color = "textures/glare_blue.png",
				-- need to add different texture
            },	
		},
        GuiName = "/Solar/NewHorizonsPath"
    }
}