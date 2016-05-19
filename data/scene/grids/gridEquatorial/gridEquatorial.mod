return {
    -- SphericalGrid module
    {   
        Name = "SphericalGrid",
        Parent = "Root",
        Static = true,
        Renderable = {
            Type = "RenderableSphericalGrid",
            GridType = "ICRF",
            GridColor = { 0.0, 0.0, 0.4, 1},
            GridMatrix = { -0.05487554,  0.4941095, -0.8676661, 0.0,
                              -0.8734371 , -0.4448296, -0.1980764, 0.0,
                           -0.483835  ,  0.7469823,  0.4559838, 0.0,
                             0.0       ,  0.0      ,  0.0      , 1.0 },
            GridSegments = 36,
        },
        Ephemeris = {
            Type = "Static" -- for now, might change.
        },

        GuiName = "/Grid/Equatorial"
    }
}