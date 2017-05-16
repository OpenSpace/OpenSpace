local marsEllipsoid = {3396190.0, 3396190.0, 3376200.0}
return {
    -- Barycenter module
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
            CameraMinHeight = 10,
            SegmentsPerPatch = 90,
            -- Allows camera to go down 10000 meters below the reference ellipsoid
            InteractionDepthBelowEllipsoid = 10000, -- Useful when having negative height map values
            Atmosphere = {
                -- Atmosphere radius in Km
                AtmoshereRadius = 3416.0,
                PlanetRadius    = 3396.19,
                PlanetAverageGroundReflectance = 0.1,
                Rayleigh = {
                    Coefficients = {
                        -- Wavelengths are given in 10^-9m
                        Wavelengths = {680, 550, 440},
                        -- Reflection coefficients are given in km^-1
                        Scattering = {19.918E-3, 13.57E-3, 5.75E-3},
                        -- In Rayleigh scattering, the coefficients of absorption and scattering are the same.
                    },
                    -- Thichkness of atmosphere if its density were uniform, in Km
                    H_R = 11.0,
                },
                -- Default
                Mie = {
                    Coefficients = {
                        -- Reflection coefficients are given in km^-1
                        Scattering = {4.0e-3, 4.0e-3, 4.0e-3},
                        -- Extinction coefficients are a fraction of the Scattering coefficients
                        Extinction = {4.0e-3/0.9, 4.0e-3/0.9, 4.0e-3/0.9}                        
                    },
                    -- Mie Height scale (atmosphere thickness for constant density) in Km
                    H_M = 1.2,
                    -- Mie Phase Function Value (G e [-1.0, 1.0]. If G = 1.0, Mie phase function = Rayleigh Phase Function)
                    G = 0.85,
                },
            },                                              
            Layers = {
                ColorLayers = {
                    {
                        Name = "Viking combo",
                        Type = "ByLevel",
                        LevelTileProviders = {
                            {
                                MaxLevel = 3, 
                                TileProvider = { FilePath = "textures/mars.jpg", }, 
                            },
                            {
                                MaxLevel = 22, 
                                TileProvider = { FilePath = "map_service_configs/MARS_Viking_MDIM21.xml" },
                            },
                        },
                        Enabled = true,
                    },
                    -- {
                    --     Type = "SingleImage",
                    --     Name = "Debug Tiles",
                    --     FilePath = "../../debugglobe/textures/test_tile.png",
                    -- },
                    --{
                    --    Name = "MARS_Viking",
                    --    FilePath = "map_service_configs/MARS_Viking_MDIM21.xml",
                    --    Enabled = true,
                    --},
                    {
                        Name = "MOLA Pseudo Color",
                        FilePath = "map_service_configs/Utah/MolaPseudoColor.xml",
                        -- Enabled = true,
                    },
                    --[[
                    {
                        Name = "Mars Viking Clr",
                        FilePath = "map_datasets/Viking/Mars_Viking_ClrMosaic_global_925m_longlat_full.vrt",
                        Enabled = true,
                    },
                    ]]
                },
                GrayScaleLayers = {
                    
                },
                GrayScaleColorOverlays = {
                    {
                        Name = "CTX Mosaic [Europe]",
                        FilePath = "map_service_configs/CTX_Mosaic.xml",
                        --Enabled = true,
                    },
                    {
                        Name = "CTX Mosaic [Utah]",
                        FilePath = "map_service_configs/Utah/CTX_Mosaic.xml",
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
                    --[[{
                        Name = "Themis IR Day",
                        FilePath = "map_service_configs/Utah/ThemisIRDay.xml",
                    },                    
                    {
                        Name = "Themis IR Night",
                        FilePath = "map_service_configs/Utah/ThemisIRNight.xml",
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
                NightLayers = { },
                WaterMasks = { },
                ColorOverlays = {
                    {
                        Type = "TileIndex",
                        Name = "Indices",
                    },
                    {
                        Type = "SizeReference",
                        Name = "Size Reference",
                        Radii = marsEllipsoid,
                        BackgroundImagePath = "../arrows.png",
                    },
                },
                HeightLayers = {
                    {
                        Name = "Mola Elevation [Europe]",
                        FilePath = "map_service_configs/Mola_Elevation.xml",
                        Enabled = true,
                        TilePixelSize = 90,
                        DoPreProcessing = true,
                    },
                    --[[
                    {
                        Name = "Mola Elevation [Utah]",
                        FilePath = "map_service_configs/Utah/Mola_Elevation.xml",
                        Enabled = false,
                        TilePixelSize = 90,
                        DoPreProcessing = true,
                    },
                    {
                        Name = "Mola Elevation CTX",
                        FilePath = "map_service_configs/Utah/MolaCTX_Elevation.xml",
                        -- Enabled = true,
                        TilePixelSize = 90,
                        DoPreProcessing = true,
                    },]]   
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
                        TilePixelSize = 90,
                        DoPreProcessing = true,
                    },
                    --[[
                    {
                        Name = "West Candor Chasma",
                        FilePath = "map_datasets/CTX/West_Candor_Chasma_DEM_longlat_global.vrt",
                        --Enabled = true,
                        TilePixelSize = 90,
                        DoPreProcessing = true,
                    },]]              
                    --[[
                    {
                        Name = "Part of Area Traversed by the Mars Exploration Rover",
                        FilePath = "map_datasets/HiRISE/Part_of_Area_Traversed_by_the_Mars_Exploration_Rover_Heightmap.vrt",
                    },
                    ]]
                },
            },
        }
    },
    -- Trail module
    {   
        Name = "MarsTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "MARS BARYCENTER",
                Observer = "SUN",
            },
            Color = { 0.814, 0.305, 0.220 },
            Period = 686.973,
            Resolution = 1000
        }
    }
}
