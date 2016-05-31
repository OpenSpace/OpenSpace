return {
    -- DebugGlobe module
    {   
        Name = "DebugGlobe",
        Parent = "Root",
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {6378137.0, 6378137.0, 6356752.314245}, -- Earth's radii
            SegmentsPerPatch = 64,
            Textures = {
                ColorTextures = {
                    {
                        Name = "VIIRS_SNPP_CorrectedReflectance_TrueColor",
                        FilePath = "map_service_configs/VIIRS_SNPP_CorrectedReflectance_TrueColor.xml"
                    },
                    {
                        Name = "Coastlines",
                        FilePath = "map_service_configs/Coastlines.xml",
                    },
                    {
                        Name = "ESRI Imagery World 2D",
                        FilePath = "map_service_configs/ESRI_Imagery_World_2D.wms",
                    },
            
                },

                HeightMaps = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/TERRAIN.wms",
                    },
                },
            },
        },
        GuiName = "/Solar/Planets/DebugGlobe"
    },

}
