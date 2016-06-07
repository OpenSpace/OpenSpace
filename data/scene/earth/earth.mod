return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Static = true,
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
    },
    -- Earth module
    {   
        Name = "Earth",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_EARTH",
			Body = "EARTH",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 6.371, 6 },
                Segments = 100
            },
            Shadow_Group = {
                Source1 = {
                    Name = "Sun",
                    -- All radius in meters
                    Radius = {696.3, 6}
                },
                --Source2 = { Name = "Monolith", Radius = {0.01, 6} },
                Caster1 = { 
                    Name = "Moon",
                    -- All radius in meters
                    Radius = {1.737, 6}
                },
                --Caster2 = { Name = "Independency Day Ship", Radius = {0.0, 0.0} }
            },
            Textures = {
                Type = "simple",
                Color = "textures/earth_bluemarble.jpg",
				Night = "textures/earth_night.jpg",
                --Height = "textures/earth_bluemarble_height.jpg",                
                -- Depth = "textures/earth_depth.png",
                Reflectance = "textures/earth_reflectance.png",
                Clouds = "textures/earth_clouds.jpg"
            },
            Atmosphere = {
                -- Atmosphere radius in Km
                AtmoshereRadius = 6420,
                --AtmoshereRadius = 6390,
                --PlanetRadius    = 6371,
                PlanetRadius    = 6360,
                PlanetAverageGroundReflectance = 0.1,
                Rayleigh = {
                    Coefficients = {
                        -- Wavelengths are given in 10^-9m
                        Wavelengths = {680, 550, 440},
                        -- Reflection coefficients are given in km^-1
                        Scattering = {5.8e-3, 1.35e-2, 3.31e-2},
                        -- In Rayleigh scattering, the coefficients of absorption and scattering are the same.
                    },
                    -- Thichkness of atmosphere if its density were uniform, in Km
                    H_R = 8.0,
                },
                -- Default
                Mie = {
                    Coefficients = {
                        -- Reflection coefficients are given in km^-1
                        Scattering = {4e-3, 4e-3, 4e-3},
                        --Scattering = {2e-5, 2e-5, 2e-5},
                        -- Extinction coefficients are a fraction of the Scattering coefficients
                        Extinction = {4e-3/0.9, 4e-3/0.9, 4e-3/0.9}
                        -- Height scale (atmosphere thickness for constant density) in Km
                    },
                    H_M = 1.2,
                    -- Mie Phase Function Value (G e [-1.0, 1.0]. If G = 1.0, Mie phase function = Rayleigh Phase Function)
                    G = 1.0,
                },
                -- Clear Sky
                -- Mie = {
                --     Coefficients = {
                --         Scattering = {20e-3, 20e-3, 20e-3},
                --         Extinction = 1.0/0.9,
                --      }
                --     H_M = 1.2,
                --     G = 0.76,
                -- },
                -- Cloudy
                -- Mie = {
                --     Coefficients = {
                --         Scattering = {3e-3, 3e-3, 3e-3},
                --         Extinction = 1.0/0.9,
                --      }
                --     H_M = 3.0,
                --     G = 0.65,
                -- },                
            }
        },
        
        GuiName = "/Solar/Planets/Earth"
    },
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "EARTH",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.5, 0.8, 1.0},
            TropicalOrbitPeriod = 365.242,
            EarthOrbitRatio = 1,
            DayLength = 24
        },
        GuiName = "/Solar/EarthTrail"
    },
    {
        Name = "EarthMarker",
        Parent = "Earth",
        Renderable = {
            Type = "RenderablePlane",
            Size = {3.0, 11.0},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/marker.png"
        },
		Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 5}
        }
    }
 -- Plane
    -- {   
    --     Name = "EarthPlane",
    --     Parent = "Earth",
    --     Renderable = {
    --         Type = "RenderablePlane",
    --         Billboard = true,
    --         Size = { 6.371, 6 },
    --         Texture = "textures/graph.jpg",
    --         Atmosphere = {
    --             Type = "Nishita", -- for example, values missing etc etc
    --             MieFactor = 1.0,
    --             MieColor = {1.0, 1.0, 1.0}
    --         }
    --     },
    --     Ephemeris = {
    --         Type = "Static",
    --         Position = { 6.371*2, 0, 0, 6},
    --     },
    -- }
}
