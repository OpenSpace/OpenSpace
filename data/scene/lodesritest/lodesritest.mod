return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
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
            Resolution = 1000
        },
        GuiName = "/Solar/EarthTrail"
    },
    -- RenderableGlobe module
    {
        Name = "LodEarth",
        Parent = "EarthBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH",
            Observer = "SUN",
            Kernels = "${OPENSPACE_DATA}/spice/de430.bsp"
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_EARTH",
            Reference = "ECLIPJ2000"
        },
        Renderable = {
            Type = "RenderableGlobe",
            Frame = "IAU_EARTH",
            Body = "EARTH",
            Radii = {6378137.0, 6378137.0, 6356752.314245}, -- Earth's radii
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 512,--512,
                OverlayMinimumSize = 512,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Name = "Black Marble Next Generation (January)",
                        FilePath = "earth/BmngJan.wms",
                        Enabled = true
                    },
                    {
                        Name = "Black Marble Next Generation Bathymetry (January)",
                        FilePath = "earth/BmngJanBathy.wms",
                    },
                      
                },
                GrayScaleOverlays = {
                   
                },
                NightTextures = {
                },
                WaterMasks = {
                },
                Overlays = {
                },
                HeightMaps = {
                    {
                        Name = "Black Marble Next Generation (Januray)",
                        FilePath = "earth/BmngJanTopo.wms",
                        Enabled = true,
                    },
                },
                HeightMapOverlays = {
                },
            },
        }
    },
    {
        Name = "LodMoon",
        Parent = "EarthBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "MOON",
            Observer = "SUN",
            Kernels = "${OPENSPACE_DATA}/spice/de430.bsp"
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_MOON",
            Reference = "ECLIPJ2000"
        },
        Renderable = {
            Type = "RenderableGlobe",
            Frame = "IAU_MOON",
            Body = "MOON",
            Radii = {1737000.0, 1737000.0, 1737000.0}, -- Earth's radii
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 512,--512,
                OverlayMinimumSize = 512,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Name = "ClemUvvis",
                        FilePath = "moon/ClemUvvis.wms",
                        Enabled = true,
                    },
                    {
                        Name = "Gebco",
                        FilePath = "moon/Gebco.wms",
                    },
                    {
                        Name = "Kaguya",
                        FilePath = "moon/Kaguya.wms",
                    },
                    -- {
                    --     Name = "LolaClrShade",
                    --     FilePath = "moon/LolaClrShade.wms",
                    -- },
                    -- {
                    --     Name = "LolaShade",
                    --     FilePath = "moon/LolaShade.wms",
                    -- },
                    {
                        Name = "UvvisHybrid",
                        FilePath = "moon/UvvisHybrid.wms",
                    },
                    {
                        Name = "Wac",
                        FilePath = "moon/Wac.wms",
                    },

                },
                GrayScaleOverlays = {
                   
                },
                NightTextures = {
                },
                WaterMasks = {
                },
                Overlays = {
                },
                HeightMaps = {
                    {
                        Name = "LolaDem",
                        FilePath = "moon/LolaDem.wms",
                        Enabled = true,
                    },
                },
                HeightMapOverlays = {

                },
            },
        },
    },
    {
        Name = "LodMars",
        Parent = "EarthBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "MARS",
            Observer = "SUN",
            Kernels = "${OPENSPACE_DATA}/spice/MAR063.BSP"
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_MARS",
            Reference = "ECLIPJ2000"
        },
        Renderable = {
            Type = "RenderableGlobe",
            Frame = "IAU_MARS",
            Body = "MARS",
            Radii = {3390000.0, 3390000.0, 3390000.0}, -- Earth's radii
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 512,--512,
                OverlayMinimumSize = 512,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Name = "MDIM",
                        FilePath = "mars/Mdim.wms",
                        Enabled = true,
                    },
                    {
                        Name = "MolaPseudoColor",
                        FilePath = "mars/MolaPseudoColor.wms",
                    },
                },
                GrayScaleOverlays = {
                    {
                        Name = "ThemisIRDay",
                        FilePath = "mars/ThemisIRDay.wms",
                    },
                    {
                        Name = "ThemisIRNight",
                        FilePath = "mars/ThemisIRNight.xml",
                    },
                   
                },
                NightTextures = {
                },
                WaterMasks = {
                },
                Overlays = {
                },
                HeightMaps = {
                    {
                        Name = "MolaElevation",
                        FilePath = "mars/MolaElevation.wms",
                        Enabled = true,
                    },
                },
                HeightMapOverlays = {
                },
            },
        }
    },
    {
        Name = "LodMercury",
        Parent = "EarthBarycenter",
        Ephemeris = {
            Type = "Spice",
            Body = "MERCURY",
            Observer = "SUN",
            Kernels = "${OPENSPACE_DATA}/spice/de430.bsp"
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_MERCURY",
            Reference = "ECLIPJ2000"
        },
        Renderable = {
            Type = "RenderableGlobe",
            Frame = "IAU_MERCURY",
            Body = "MERCURY",
            Radii = {2440000.0, 2440000.0, 2440000.0}, -- Earth's radii
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 512,--512,
                OverlayMinimumSize = 512,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Name = "MessengerMdis",
                        FilePath = "mercury/MessengerMdis.wms",
                        Enabled = true
                    },
                    {
                        Name = "MessengerMosaic",
                        FilePath = "mercury/MessengerMosaic.wms",
                    }
                },
                GrayScaleOverlays = {
                   
                },
                NightTextures = {
                },
                WaterMasks = {
                },
                Overlays = {
                },
                HeightMaps = {
                },
                HeightMapOverlays = {

                },
            },
        }
    },
}
