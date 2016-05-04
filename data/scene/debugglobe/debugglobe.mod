return {
    -- DebugGlobe module
    {   
        Name = "DebugGlobe",
        Parent = "Root",
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {6378137.0, 6378137.0, 6356752.314245}, -- Earth's radii
            Textures = {
                ColorTextures = {
                    {
                        Name = "Terrain tileset",
                        FilePath = "map_service_configs/TERRAIN.wms",
                    },
                    --[[
                    {
                        Name = "MODIS Terra tileset",
                        FilePath = "map_service_configs/TERRA_CR_B143_2016-04-12.wms",
                    },
                    --]]

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
