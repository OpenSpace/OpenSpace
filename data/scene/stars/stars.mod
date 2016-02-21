return {
    -- Stars module
    {   
        Name = "Stars",
        Parent = "Root",
        Renderable = {
            Type = "RenderableStars",
            File = "${OPENSPACE_DATA}/scene/stars/speck/stars.speck",
            Texture = "${OPENSPACE_DATA}/scene/stars/textures/halo.png",
            ColorMap = "${OPENSPACE_DATA}/scene/stars/colorbv.cmap"
        },
        Ephemeris = {
            Type = "Static"
        }
    }
}
