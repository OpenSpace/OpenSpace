if UseAccurateNewHorizonsKernels then
    NewHorizonsKernels = {
        "${SPICE}/nh_kernels/spk/NavSE_plu047_od122.bsp"
    }
else
    NewHorizonsKernels = {
        "${SPICE}/NewHorizonsKernels/nh_p4p5_revised.bsp"
    }
end

return {
    -- Styx module
    {   
        Name = "Kerberos",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
			Frame = "IAU_PLUTO",
			Body = "KERBEROS",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.1 , 5 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/gray.jpg",
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Ephemeris = {
            Type = "Spice",
            Body = "KERBEROS",
            Reference = "ECLIPJ2000",
            Observer = "PLUTO BARYCENTER",
            Kernels = NewHorizonsKernels
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_PLUTO",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/Kerberos"
    },
    {
        Name = "KerberosText",
        Parent = "Kerberos",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 6.3},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Kerberos-Text.png"
        },
        Ephemeris = {
            Type = "Static",
            Position = {1, 0, 1, 6}
        }
    },    
    -- StyxTrail module
    {   
        Name = "KerberosTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "KERBEROS",
            Frame = "GALACTIC",
            Observer = "PLUTO BARYCENTER",
            RGB = {0.00,0.62,1.00},
            TropicalOrbitPeriod = 150 ,
            EarthOrbitRatio = 0.2,
            DayLength = 1,
            DayLength = 16.11,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/KerberosTrail"
    }
	
}
