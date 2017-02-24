return {
    -- Stereo A Orbit Module
    {
        Name = "STEREO A",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO AHEAD",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 365.242,
            Resolution = 1000
        }
    }
}
