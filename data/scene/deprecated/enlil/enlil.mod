f = 1;
return {
    -- Volume module
    {   
        Name = "Enlil",
        Parent = "Root",
        RenderableToggle = "e",
        Renderable = {
            Type = "RenderableVolumeGL",
            BoxScaling = { f, f, f},
            VolumeName = "EnlilVolume",
            Volume = "${OPENSPACE_DATA}/enlil/Hong_Xie_120312_SH_1.enlil.0016.cdf",
            Hints = {
                        Dimensions = {256, 30, 90},
                        -- Dimensions = {512, 90, 180},
                        Model = "ENLIL",
                        Variable = "rho",
                        Cache = true,
            },
            TransferFunctionName = "EnlilTF",
            TransferFunction = "transferfunctions/t2.txt",
            -- TransferFunction = "transferfunctions/t1.txt",
            Sampler = "sampler.glsl",
        }
    }
}
