local marsEllipsoid = {3396190.0, 3396190.0, 3376200.0}

local patches = {
    {
        Name = "West Candor Chasma",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/CTX/West_Candor_Chasma_longlat_global.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/CTX/West_Candor_Chasma_DEM_longlat_global.vrt")
    },
    {
        Name = "Southwest Candor Chasma",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Layered_Rock_Outcrops_in_Southwest_Candor_Chasma_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Layered_Rock_Outcrops_in_Southwest_Candor_Chasma_Heightmap.vrt")
    },
    {
        Name = "Kaiser Crater",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Active_Dune_Gullies_in_Kaiser_Crater_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Active_Dune_Gullies_in_Kaiser_Crater_Heightmap.vrt")
    },
    {
        Name = "Eberswalde Crater",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Delta_Structure_in_Eberswalde_Crater_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Delta_Structure_in_Eberswalde_Crater_Heightmap.vrt")
    },
    {
        Name = "Mojave Crater",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Dissected_Wall_of_Mojave_Crater_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Dissected_Wall_of_Mojave_Crater_Heightmap.vrt")
    },
    {
        Name = "Tartarus Montes",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Field_of_Cones_in_the_Tartarus_Montes_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Field_of_Cones_in_the_Tartarus_Montes_Heightmap.vrt")
    },
    {
        Name = "Juventae Chasma 2",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Light-toned_Layering_in_Plains_West_of_Juventae_Chasma_2_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Light-toned_Layering_in_Plains_West_of_Juventae_Chasma_2_Heightmap.vrt")
    },
    {
        Name = "Ganges Chasma",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Light-Toned_Mounds_in_Ganges_Chasma_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Light-Toned_Mounds_in_Ganges_Chasma_Heightmap.vrt")
    },
    {
        Name = "MSL EDL Landing Site",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/MSL_EDL_Landing_Site_6_Days_texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/MSL_EDL_Landing_Site_6_Days_hole_filled_heightmap.vrt")
    },
    {
        Name = "Northeast Melas Chasma",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/northeast_melas_chasma_dune_fields_and_wall_rock_texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/northeast_melas_chasma_dune_fields_and_wall_rock_heightmap.vrt")
    },
    {
        Name = "Olympus Mons",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Olympus_Mons-Fissure_and_Channel-17N127W_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Olympus_Mons-Fissure_and_Channel-17N127W_Heightmap.vrt")
    },
    {
        Name = "Melas Chasma",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Sulfates_and_Valley_System_in_Melas_Chasma_Basin_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Sulfates_and_Valley_System_in_Melas_Chasma_Basin_Heightmap.vrt")
    },
    {
        Name = "Elysium Mons",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Unusual_Depression_Near_Elysium_Mons_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Unusual_Depression_Near_Elysium_Mons_Heightmap.vrt")
    },
    {
        Name = "Mars Exploration Rover",
        Texture = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Part_of_Area_Traversed_by_the_Mars_Exploration_Rover_Texture.vrt"),
        Height = openspace.absPath("${OPENSPACE_DATA}/scene/lodglobes/mars/map_datasets/HiRISE/Part_of_Area_Traversed_by_the_Mars_Exploration_Rover_Heightmap.vrt")
    },
}

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
            Layers = {
                ColorLayers = {
                    {
                        Name = "Viking",
                        FilePath = "map_service_configs/MARS_Viking_MDIM21.xml",
                    },
                    {
                        Name = "mars_COL_v006_equirectangular_rgb",
                        FilePath = "map_datasets/other/mars_COL_v006_equirectangular_rgb.tif",
                        Enabled = true,
                    },
                    {
                        Name = "mars_COL_v006_equirectangular_rgb",
                        FilePath = "map_datasets/other/mars_COL_v006_mars2000_rgb.tif",
                    },
                    {
                        Name = "MOLA Pseudo Color",
                        FilePath = "map_service_configs/Utah/MolaPseudoColor.xml",
                        -- Enabled = true,
                    },
                },
                GrayScaleLayers = {
                    
                },
                GrayScaleColorOverlays = {
                    {
                        Name = "CTX Mosaic [AWS]",
                        FilePath = "map_service_configs/CTX.wms",
                        Enabled = true,
                    },
                    {
                        Name = "CTX Mosaic [Europe]",
                        FilePath = "map_service_configs/CTX_Mosaic.xml",
                        --Enabled = true,
                    },
                    {
                        Name = "CTX Mosaic [Utah]",
                        FilePath = "map_service_configs/Utah/CTX_Mosaic.xml",
                    },
                    table.unpack(openspace.globebrowsing.createTextureLayers(patches))
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
                        Type = "TileIndexTileLayer",
                        Name = "Indices",
                    },
                    {
                        Type = "SizeReferenceTileLayer",
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
                    table.unpack(openspace.globebrowsing.createHeightLayers(patches))
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
