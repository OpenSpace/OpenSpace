return {
    -- Volume module
    {   
        Name = "Enlil New Horizons",
        Parent = "Root",
        Ephemeris = {
            Type = "Static",
            Position = { 0.0, 0.0, 0.0, 0}
        },
        Renderable = {
            Type = "RenderableMultiresVolume",
            ReferenceFrame = "HEEQ",
            Translation = {0, 0, 0},
            Rotation = {2.1, 0, 0},
            Scaling = {1.1, 1.1, 1.1},
            ScalingExponent = 13,
            Source = "tsp/enlil_nh_128_128_16.tsp",
            ErrorHistogramsSource = "tsp/enlil_nh_128_128_16_50.errorHistograms",                                       
            TransferFunction = "transferfunctions/fire.txt",
            BrickSelector = "tf",
        },
        GuiName = "/Volumes/ENLIL New Horizons"
    }
}
