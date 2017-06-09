return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "EARTH",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        }
    },
    {
    -- The default reference frame for Earth-orbiting satellites
        Name = "EarthInertial",
        Parent = "EarthBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "J2000",
                DestinationFrame = "GALACTIC",
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
                Radius = 6.371E6,
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/earth_bluemarble.jpg",
                Night = "textures/earth_night.jpg",
                Height = "textures/earth_bluemarble_height.jpg"
            },
            Atmosphere = {
                -- Atmosphere radius in Km
                --AtmoshereRadius = 6450,
                --AtmoshereRadius = 6420.0,
                AtmoshereRadius = 6447.0,
                --PlanetRadius    = 6378.137,
                PlanetRadius    = 6377.0,
                --PlanetRadius    = 6360.0,
                PlanetAverageGroundReflectance = 0.1,
                Rayleigh = {
                    Coefficients = {
                        -- Wavelengths are given in 10^-9m
                        Wavelengths = {680, 550, 440},
                        -- Reflection coefficients are given in km^-1
                        Scattering = {5.8E-3, 13.5E-3, 33.1E-3},
                        -- In Rayleigh scattering, the coefficients of absorption and scattering are the same.
                    },
                    -- Thichkness of atmosphere if its density were uniform, in Km
                    H_R = 8.0,
                },
                --[[
                Ozone = {
                     Coefficients = {
                        -- Extinction coefficients 
                        Extinction = {3.426E-5, 8.298E-5, 0.356E-5}
                     },
                     H_O = 8.0,
                },
                ]]--
                -- Default
                Mie = {
                    Coefficients = {
                        -- Reflection coefficients are given in km^-1
                        Scattering = {4.0e-3, 4.0e-3, 4.0e-3},
                        --Scattering = {2.0e-5, 2.0e-5, 2.0e-5},
                        -- Extinction coefficients are a fraction of the Mie coefficients
                        Extinction = {4.0e-3/0.9, 4.0e-3/0.9, 4.0e-3/0.9}
                    },
                    -- Height scale (atmosphere thickness for constant density) in Km
                    H_M = 1.2,
                    -- Mie Phase Function Value (G e [-1.0, 1.0]. If G = 1.0, Mie phase function = Rayleigh Phase Function)
                    G = 0.85
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
                Image = {
                    ToneMapping = jToneMapping,
                    Exposure = 0.4,
                    Gamma = 1.85,                                                                      
                },
                Debug = {
                    -- PreCalculatedTextureScale is a float from 1.0 to N, with N > 0.0 and N in Naturals (i.e., 1, 2, 3, 4, 5....)
                    PreCalculatedTextureScale = 1.0,
                    SaveCalculatedTextures = false, 
                },                   
            },
        },
        Tag = {"planet_solarSystem", "planet_terrestrial"},
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_EARTH",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        GuiName = "/Solar/Planets/Earth"
    },
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "EARTH",
                Observer = "SUN"
            },
            Color = { 0.5, 0.8, 1.0 },
            -- StartTime = "2016 JUN 01 12:00:00.000",
            -- EndTime = "2017 JAN 01 12:00:00.000",
            -- SampleInterval = 3600
            Period = 365.242,
            Resolution = 1000,
            Tag = {"planetTrail_solarSystem", "planetTrail_terrestrial"}
        },
        GuiName = "/Solar/EarthTrail",
    },
    --[[
    {
        Name = "EarthMarker",
        Parent = "Earth",
        Renderable = {
            Type = "RenderablePlane",
            Size = 3.0E11,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/marker.png",
            BlendMode = "Additive"
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 5}
        }
    }
    ]]
}
