return {
    -- CharonProjection module
    {   
        Name = "Charon",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
			Frame = "IAU_CHARON", 
			Body = "CHARON",
            Geometry = {
                Type = "SimpleSphereProjection",
                Radius = { 6.035 , 5 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/charon_highres.jpg",
				Project = "textures/defaultProj.png",
				Sequencing = "true",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            },
			Projection = {
				Observer   = "NEW HORIZONS",
				Target     = "CHARON",
				Aberration = "NONE",
			},
			Instrument = {                
				Name       = "NH_LORRI",
				Method     = "ELLIPSOID",
				Aberration = "NONE",
				Fovy       = 0.2907,
				Aspect     = 1,
				Near       = 0.2,
				Far        = 10000,
			},
            PotentialTargets = {
                "PLUTO",
                "CHARON"
            }
        },
        Ephemeris = {
            Type = "Spice",
            Body = "CHARON",
            Reference = "ECLIPJ2000",
            Observer = "PLUTO BARYCENTER",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_CHARON",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/Charon"
    },
    {
        Name = "CharonText",
        Parent = "Charon",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 6.3},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Charon-Text.png"
        },
        Ephemeris = {
            Type = "Static",
             Position = {0, -10, 0, 5}
        }
    },
	{
        Name = "CharonShadow",
        Parent = "Charon",
        Renderable = {
            Type = "RenderableShadowCylinder",
			TerminatorType = "PENUMBRAL", 
			LightSource = "SUN",
			Observer = "NEW HORIZONS",
			Body = "CHARON",
			BodyFrame = "IAU_CHARON",
			MainFrame = "GALACTIC",
			Aberration = "NONE",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 5}
        }
    },    
    -- CharonTrail module
    {   
        Name = "CharonTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "CHARON",
            Frame = "GALACTIC",
            Observer = "PLUTO BARYCENTER",
            RGB = {0.00,0.62,1.00},
            TropicalOrbitPeriod = 120 ,
            EarthOrbitRatio = 0.03,
            DayLength = 1,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/CharonTrail"
    }
}
