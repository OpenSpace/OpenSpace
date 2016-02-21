f = 1
return {
    -- Volume module
    {   
        Name = "Volume3",
        Parent = "Root",
        Ephemeris = {
            Type = "Static",
            Position = { -1.0, -0.1, 0.1, 0}
        },
        RenderableToggle = "1",
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
            BoxScaling = { 2.8*f, 1*f, 1*f, 0.0},
            VolumeName = "volume3",
            Volume = "${OPENSPACE_DATA}/batsrus.cdf",
            Hints = {
                        -- Dimensions = {300, 100, 100},
                        Dimensions = {600, 200, 200},
                        -- Dimensions = {900, 300, 300},
                        Model = "BATSRUS",
                        Variable = "rho",
                        Cache = true,
                        --Variables = {"bx", "by", "bz"},
            },
            -- ]]

            TransferFunctionName = "transferFunction3",
            TransferFunction = "transferfunctions/t1.txt",
            Sampler = "sampler.glsl",
        },
        GuiName = "/Volumes/Volume"
    }
}