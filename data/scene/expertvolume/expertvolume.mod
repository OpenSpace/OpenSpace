return {
    -- Volume module
    {   
        Name = "Volume",
        Parent = "Root",
        Ephemeris = {
            Type = "Static",
            Position = { 0, 0, -3.0, 0}
        },
        Renderable = {
            Type = "RenderableVolumeExpert",

            --BoxScaling = { 3.0, 1.0, 1.0},
            Volumes = {
                
                -- {
                --     File = "${OPENSPACE_DATA}/skull.raw",
                --     Hints = {
                --         Dimensions = {256, 256, 256},
                --         Format = "RED",
                --         InternalFormat = "R8"
                --     },
                -- },
                
                ---[[
                {
                    File = "${OPENSPACE_DATA}/batsrus.cdf",
                    Hints = {
                        Dimensions = {384, 128, 128},
                        Model = "BATSRUS",
                        Variable = "rho",
                    },
                },
                --]]
                --[[
                {
                    File = "${OPENSPACE_DATA}/enlil/Hong_Xie_120312_SH_1.enlil.0009.cdf",
                    Hints = {
                        Dimensions = {256, 30, 90},
                        Model = "ENLIL",
                        Variable = "rho",
                    },
                },
                --]]
            },
            TransferFunctions = {
                --"${CONFIG}/transferfunctions/fire.txt",
                "${OPENSPACE_DATA}/scene/expertvolume/transferfunctions/t1.txt",
                --"transferfunctions/t1.png",
                --"transferfunctions/t2.png"
            },

            Kernel = {
                Source = "${OPENSPACE_DATA}/scene/expertvolume/expertraycaster.cl",
                Options = {
                    OptDisable = true,
                    KernelArgInfo = true,
                },
                Definitions = {
                    MIP = "1",
                },
                Includes = {
                    "${KERNELS}/helpers"
                },
                UpdateOnSave = true,
            },
        },
        GuiName = "/Volumes/Volume"
    }
}
