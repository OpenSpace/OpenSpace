local BENNU_BODY = "2101955"
local OSIRIS_REX_BODY = "OSIRIS-REX"

return {
    ------------------------
    --        Bennu       --
    ------------------------
    {   
        Name = "Bennu2",
        Parent = "SolarSystemBarycenter",

        Ephemeris = {
            Type = "Spice",
            Body = BENNU_BODY,
            -- Reference = "ECLIPJ2000",
            Reference = "GALACTIC",
            Observer = "SUN",
        },

        Renderable = {
            Frame = "IAU_BENNU",
            Body = BENNU_BODY,

            Type = "RenderableGlobe",
            Radii = {275.0, 275.0, 246.0},
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 1024,
                OverlayMinimumSize = 1024,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Type = "SingleImage",
                        Name = "Basic",
                        FilePath = "../debugglobe/map_service_configs/ESRI_Imagery_World_2D.wms",
                        Enabled = true,
                    },
                }
            }
        },
        
        GuiName = "/Solar/Bennu"
    },

    {   
        Name = "BennuTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = BENNU_BODY,
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.4, 0.0, 0.7},
            TropicalOrbitPeriod = 436.649,
            EarthOrbitRatio = 1.3559, -- worst parameter I've ever seen
            DayLength = 4.288 -- why this for a path??
        },
        GuiName = "/Solar/BennuTrail"
    },

    
    ------------------------
    --     Osiris Rex     --
    ------------------------
    {
        Name = "OsirisRex",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "GALACTIC",
            Body = OSIRIS_REX_BODY,

            Type = "RenderableGlobe",
            Radii = {15.0, 15.0, 15.0},
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 1024,
                OverlayMinimumSize = 1024,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Type = "SingleImage",
                        Name = "Basic",
                        FilePath = "../debugglobe/map_service_configs/ESRI_Imagery_World_2D.wms",
                        Enabled = true,
                    },
                }
            }
        },

        Ephemeris = {
            Type = "Spice",
            Body = OSIRIS_REX_BODY,
            -- Reference = "ECLIPJ2000",
            Reference = "GALACTIC",
            Observer = "SUN",
        },

        GuiName = "/Solar/OsirisRex"
    },

    {   
        Name = "OsirisRexPath",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePath",
            Body = OSIRIS_REX_BODY,
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.8, 0.8, 0.2},
            TimeSteps = 900,
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },  
            DrawLine = true,
            
            StartTime = "2016 SEP 8 12:00:00",
            EndTime = "2022 OCT 17 12:00:00"
        },
        GuiName = "/Solar/OsirisRexTrail"
    },
}
