return {
    -- DebugGlobe module
    {   
        Name = "DebugGlobe",
        Parent = "Root",
        Renderable = {
            Type = "RenderableGlobe",
            Radii = {6378137.0, 6378137.0, 6356752.314245}, -- Earth's radii
            Textures = {
                Color = "textures/earth_bluemarble.jpg",
            }
        },
        GuiName = "/Solar/Planets/DebugGlobe"
    },

}
