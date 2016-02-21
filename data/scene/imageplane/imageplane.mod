return {
    -- Latest image taken by LORRI
	{ 
        Name = "ImagePlane",
        Parent = "NewHorizons",
        Renderable = {
            Type = "RenderablePlaneProjection",
			Frame = "IAU_JUPITER",
			DefaultTarget = "JUPITER",
			Spacecraft = "NEW HORIZONS",
			Instrument = "NH_LORRI",
			Moving = false,
            Texture = "textures/test.jpg",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        }, 
    },
	-- LORRI FoV square
	{
        Name = "ImagePlane2",
        Parent = "NewHorizons",
        Renderable = {
            Type = "RenderablePlaneProjection",
			Frame = "IAU_JUPITER",
			DefaultTarget = "JUPITER",
			Spacecraft = "NEW HORIZONS",
			Instrument = "NH_LORRI",
			Moving = true,
            Texture = "textures/squarefov.png",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        },
    }

}

