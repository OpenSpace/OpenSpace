
earth_radius = 6.371*2
earth_radius_s = 6

return {
    -- Volume module
    {   
        Name = "Volume",
        Parent = "Earth",
        Ephemeris = {
            Type = "Static",
            Position = { 0, 0, 0.0, 0}
        },
        Renderable = {
            Type = "RenderableVolumeGL",
            --[[
            BoxScaling = { 1.0, 1.0, 1.0},
            Volume = "${OPENSPACE_DATA}/skull.raw",
            Hints = {
                Dimensions = {256, 256, 256},
                Format = "RED",
                InternalFormat = "R8"
            },
            --]]

            ---[[
            VolumeName = "volume1",
            Volume = "${OPENSPACE_DATA}/batsrus.cdf",
            Hints = {
                        Dimensions = {300, 100, 100},
                        -- Dimensions = {600, 200, 200},
                        -- Dimensions = {900, 300, 300},
                        Variable = "rho",
                        Cache = true,
                        --Variables = {"bx", "by", "bz"},
            },
            --]]

            TransferFunctionName = "transferFunction1",
            TransferFunction = "transferfunctions/t1.txt",
            Sampler = "sampler.glsl",
        },
        GuiName = "/Volumes/Volume"
    }
}