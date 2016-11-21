return {
    -- Mercury barycenter module
    {
        Name = "MercuryBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MERCURY",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },

    -- RenderableGlobe module
    {   
        Name = "Mercury",
        Parent = "MercuryBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MERCURY",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {2439700, 2439700.0, 2439700.0},
            Frame = "IAU_MERCURY",
            Body = "MERCURY",
            
            CameraMinHeight = 300,
            InteractionDepthBelowEllipsoid = 0, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            TextureInitData = {
                ColorTextureMinimumSize = 512,
                OverlayMinimumSize = 512,
                HeightMapMinimumSize = 64,
            },
            Textures = {
                ColorTextures = {
                    {
                        Name = "On Mercury Color",
                        FilePath = "map_service_configs/OnMercuryColor.xml",
                        Enabled = true,
                    },
                    {
                        Name = "On Mercury Image",
                        FilePath = "map_service_configs/OnMercuryImage.xml",
                    },
                },
                GrayScaleOverlays = { },
                NightTextures = { },
                WaterMasks = { },
                Overlays = { },
                HeightMaps = {
                    {
                        Name = "On Mercury Height",
                        FilePath = "map_service_configs/OnMercuryElevationGaskell.xml",
                        Enabled = true,
                    },
                },
            },
        },
    },

    -- MercuryTrail module
    {   
        Name = "MercuryTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "MERCURY",
                Observer = "SUN",
            },
            Color = {0.6, 0.5, 0.5 },
            Period = 87.968,
            Resolution = 100
        }
    }
}
