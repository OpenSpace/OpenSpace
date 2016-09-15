earthEllipsoid = {6378137.0, 6378137.0, 6356752.314245} -- Earth's radii
return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceEphemeris",
                Body = "EARTH",
                Reference = "ECLIPJ2000",
                Observer = "SUN",
                Kernels = {
                    "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
                }
            },
        },
    },
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "SolarSystemBarycenter",
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
    -- RenderableGlobe module
    {
        Name = "Earth",
        Parent = "EarthBarycenter",
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
        Renderable = {
            Type = "RenderableGlobe",
            Radii = earthEllipsoid,
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
                        Type = "Temporal",
                        Name = "Temporal VIIRS SNPP",
                        FilePath = "map_service_configs/Temporal_VIIRS_SNPP_CorrectedReflectance_TrueColor.xml",
                    },
                    {
                        Type = "SingleImage",
                        Name = "Debug Tiles",
                        FilePath = "../debugglobe/textures/test_tile.png",
                    },
                    {
                        Type = "Temporal",
                        Name = "Temporal MODIS Aqua CorrectedRecflectance TrueColor",
                        FilePath = "map_service_configs/Temporal_MODIS_Aqua_CorrectedReflectance_TrueColor.xml",
                    },
                    {
                        Name = "MODIS_Terra_CorrectedReflectance_TrueColor",
                        FilePath = "map_service_configs/MODIS_Terra_CorrectedReflectance_TrueColor.xml",
                    },
                    {
                        Name = "ESRI Imagery World 2D",
                        FilePath = "map_service_configs/ESRI_Imagery_World_2D.wms",
                        Enabled = true,
                    }
                },
                GrayScaleOverlays = {
                   
                },
                NightTextures = {
                    {
                        Name = "Earth at Night 2012",
                        FilePath = "map_service_configs/VIIRS_CityLights_2012.xml",
                    },
                },
                WaterMasks = {
                    {
                        Name = "MODIS_Water_Mask",
                        FilePath = "map_service_configs/MODIS_Water_Mask.xml",
                    },
                },
                Overlays = {
                    {
                        Name = "Coastlines",
                        FilePath = "map_service_configs/Coastlines.xml",
                    },
                    {
                        Name = "Reference_Features",
                        FilePath = "map_service_configs/Reference_Features.xml",
                    },
                    {
                        Name = "Reference_Labels",
                        FilePath = "map_service_configs/Reference_Labels.xml",
                    },
                    {
                        Type = "SizeReference",
                        Name = "Size Reference",
                        Radii = earthEllipsoid,
                        BackgroundImagePath = "../debugglobe/textures/arrows.png",
                    },
                },
                HeightMaps = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/TERRAIN.wms",
                        Enabled = true,
                        MinimumPixelSize = 90,
                        DoPreProcessing = true,
                    },
                },
            },
        },
        GuiName = "/Solar/Planets/LodEarth"
    },
}