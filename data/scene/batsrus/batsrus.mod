return {
    -- Volume module
    ---[[
    {   
        Name = "BatsrusRho",
        Parent = "Earth",
        Ephemeris = {
            Type = "Static",
            Position = { 0, 0, 0.0, 0}
        },
        Renderable = {
            Type = "RenderableVolumeGL",
            -- BoxScaling = { 2.8, 1, 1},
            VolumeName = "BatsrusRhoVolume",
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
            TransferFunctionName = "BatsrusRhoTF",
            TransferFunction = "transferfunctions/rho.txt",
            Sampler = "rhosampler.glsl",
        },
        GuiName = "/Volumes/Volume"
    },
    --]

    ---[[
    {   
        Name = "BatsrusP",
        Parent = "Earth",
        Ephemeris = {
            Type = "Static",
            Position = { 0, 0, 0.0, 0}
        },
        Renderable = {
            Type = "RenderableVolumeGL",

            -- BoxScaling = { 2.8, 1, 1},
            VolumeName = "BatsrusPVolume",
            Volume = "${OPENSPACE_DATA}/batsrus.cdf",
            Hints = {
                        -- Dimensions = {300, 100, 100},
                        Dimensions = {600, 200, 200},
                        -- Dimensions = {900, 300, 300},
                        Model = "BATSRUS",
                        Variable = "p",
                        Cache = true,
                        --Variables = {"bx", "by", "bz"},
            },

            TransferFunctionName = "BatsrusPTF",
            TransferFunction = "transferfunctions/p.txt",
            Sampler = "psampler.glsl",
        },
        GuiName = "/Volumes/Volume"
    }
    --]]
}