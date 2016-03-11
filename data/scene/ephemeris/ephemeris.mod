return {
    -- Ephemeris module
    {   
        Name = "Ephemeris",
        Parent = "Root",
        Renderable = {
            Type = "RenderableEphemeris",
		    Textures = {
                Type = "simple",
			    Color = "textures/glare_blue.png",
				-- need to add different texture
            },	
		},
        GuiName = "/Solar/Ephemeris"
    }
}