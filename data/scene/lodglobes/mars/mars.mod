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
            SegmentsPerPatch = 90,
            Layers = {
                ColorLayers = {
                    {
                        Name = "Viking",
                        FilePath = "map_service_configs/MARS_Viking_MDIM21.xml",
                        Enabled = true,
                    },
                    {
                        Name = "MOLA Pseudo Color",
                        FilePath = "map_service_configs/Utah/MolaPseudoColor.xml",
                    },
                    {
                        Name = "CTX Mosaic [Europe]",
                        FilePath = "map_service_configs/CTX_Mosaic.xml",
                        BlendMode = "Color"
                    },
                    {
                        Name = "CTX Mosaic [Utah]",
                        FilePath = "map_service_configs/Utah/CTX_Mosaic.xml",
                        BlendMode = "Color"
                    },
                },
                Overlays = {
                    {
                        Type = "TileIndexTileLayer",
                        Name = "Indices",
                    },
                    {
                        Type = "SizeReferenceTileLayer",
                        Name = "Size Reference",
                        Radii = marsEllipsoid,
                    },
                },
                HeightLayers = {
                    {
                        Name = "Mola Elevation [Europe]",
                        FilePath = "map_service_configs/Mola_Elevation.xml",
                        Enabled = true,
                        TilePixelSize = 90,
                    },
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
