local marsEllipsoid = {3396190.0, 3396190.0, 3376200.0}
return {
    -- Mars barycenter module
    {
        Name = "MarsBarycenter",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "MARS BARYCENTER",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
        },
    },
    -- RenderableGlobe module
    {   
        Name = "Mars",
        Parent = "MarsBarycenter",
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_MARS",
                DestinationFrame = "GALACTIC",
            },
            Scale = {
                Type = "StaticScale",
                Scale = 1,
            },
        },
        Renderable = {
            Type = "RenderableGlobe",
            Radii = marsEllipsoid,
            CameraMinHeight = 1000,
            InteractionDepthBelowEllipsoid = 10000, -- Useful when having negative height map values
            SegmentsPerPatch = 64,
            Textures = {
                ColorTextures = {
                    {
                        Type = "SingleImage",
                        Name = "Debug Tiles",
                        FilePath = "../debugglobe/textures/test_tile.png",
                    },
                    {
                        Name = "MARS_Viking",
                        FilePath = "map_service_configs/MARS_Viking_MDIM21.xml",
                        Enabled = true,
                    },
                    --[[
                    {
                        Name = "Mars Viking Clr",
                        FilePath = "map_datasets/Viking/Mars_Viking_ClrMosaic_global_925m_longlat_full.vrt",
                        Enabled = true,
                    },
                    ]]
                },
                GrayScaleTextures = {
                    
                },
                GrayScaleOverlays = {
                --[[
                    {
                        Name = "CTX Mosaic",
                        FilePath = "map_service_configs/CTX_Mosaic.xml",
                        Enabled = true,
                    },
                    {
                        Name = "West Candor Chasma",
                        FilePath = "map_datasets/CTX/West_Candor_Chasma_longlat_global.vrt",
                        --Enabled = true,
                    },
                    {
                        Name = "Layered Rock Outcrops in Southwest Candor Chasma",
                        FilePath = "map_datasets/HiRISE/Layered_Rock_Outcrops_in_Southwest_Candor_Chasma_Texture.vrt",
                    },
                    {
                        Name = "MER_Meridianni_Endeavor_Basemap_25cm",
                        FilePath = "map_datasets/Basemap/MER_Meridianni_Endeavor_Basemap_25cm.vrt",
                    },
                    {
                        Name = "Part of Area Traversed by the Mars Exploration Rover",
                        FilePath = "map_datasets/HiRISE/Part_of_Area_Traversed_by_the_Mars_Exploration_Rover_Texture.vrt",
                    },
                    ]]
                },
                NightTextures = {

                },
                WaterMasks = {

                },
                Overlays = {
                    {
                        Type = "TileIndex",
                        Name = "Indices",
                    },
                    {
                        Type = "SizeReference",
                        Name = "Size Reference",
                        Radii = marsEllipsoid,
                        BackgroundImagePath = "../debugglobe/textures/arrows.png",
                    },
                },
                HeightMaps = {
                    --[[
                    {
                        Name = "Mola Elevation",
                        FilePath = "map_service_configs/Mars_MGS_MOLA_DEM.xml",
                        Enabled = true,
                        MinimumPixelSize = 64,
                        DoPreProcessing = true,
                    },
                    
                    {
                        Name = "Mola Elevation",
                        FilePath = "map_service_configs/Mola_Elevation.xml",
                        Enabled = true,
                        MinimumPixelSize = 90,
                        DoPreProcessing = true,
                    },
                    {
                        Name = "West Candor Chasma",
                        FilePath = "map_datasets/CTX/West_Candor_Chasma_DEM_longlat_global.vrt",
                        --Enabled = true,
                        MinimumPixelSize = 90,
                        DoPreProcessing = true,
                    },
                    {
                        Name = "Layered Rock Outcrops in Southwest Candor Chasma",
                        FilePath = "map_datasets/HiRISE/Layered_Rock_Outcrops_in_Southwest_Candor_Chasma_Heightmap.vrt",
                        MinimumPixelSize = 90,
                        DoPreProcessing = true,
                    },
                    {
                        Name = "Part of Area Traversed by the Mars Exploration Rover",
                        FilePath = "map_datasets/HiRISE/Part_of_Area_Traversed_by_the_Mars_Exploration_Rover_Heightmap.vrt",
                    },
                    ]]
                },
            },
        }
    },
    -- MarsTrail module
    {   
        Name = "MarsTrail",
        Parent = "Sun",
        Renderable = {
            Type = "RenderableTrail",
            Body = "MARS BARYCENTER",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 1, 0.8, 0.5 },
            TropicalOrbitPeriod = 686.973,
            EarthOrbitRatio = 1.881,
            DayLength = 24.6597,
        }
    }
}
