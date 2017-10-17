return {
    -- SphericalGrid module
    {   
        Name = "Ecliptic Grid",
        Parent = "Root",
        Renderable = {
            Type = "RenderableSphericalGrid",
            GridColor = { 0.75, 0.0, 0.0, 1.0},
            LineWidth = 0.75,
            GridMatrix = { -0.05487554,  0.4941095, -0.8676661   , 0.0,
                           -0.9938214 , -0.1109906, -0.0003515167, 0.0,
                           -0.09647644,  0.8622859,  0.4971472   , 0.0,
                            0.0       ,  0.0      ,  0.0         , 1.0 }
        },
        GuiPath = "/Other/Grids"
    },
    {
        Name = "Equatorial Grid",
        Parent = "Root",
        Renderable = {
            Type = "RenderableSphericalGrid",
            GridColor = { 0.0, 0.0, 0.75, 1.0},
            LineWidth = 0.75,
            GridMatrix = { -0.05487554,  0.4941095, -0.8676661, 0.0,
                           -0.8734371 , -0.4448296, -0.1980764, 0.0,
                           -0.483835  ,  0.7469823,  0.4559838, 0.0,
                            0.0       ,  0.0      ,  0.0      , 1.0 }
        },
        GuiPath = "/Other/Grids"
    },
    {
        Name = "Galactic Grid",
        Parent = "SolarSystem",
        Renderable = {
            Type = "RenderableSphericalGrid",
            LineWidth = 0.75,
            GridColor = { 0.0, 0.75, 0.75, 1.0}
        },
        GuiPath = "/Other/Grids"
    }
}
