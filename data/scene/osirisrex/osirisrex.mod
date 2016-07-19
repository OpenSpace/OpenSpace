--dofile "osirisrex_spicekernels.lua"

return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Static = true,
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
    },

    {   
        Name = "OsirisRex",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_EARTH",
            Body = "EARTH",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 6.371, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/earth_bluemarble.jpg",
                Night = "textures/earth_night.jpg",
                --Height = "textures/earth_bluemarble_height.jpg",                
                -- Depth = "textures/earth_depth.png",
                Reflectance = "textures/earth_reflectance.png",
                Clouds = "textures/earth_clouds.jpg"
            }
        },
        
        GuiName = "/Solar/Planets/OsirisRex"
    },
}
