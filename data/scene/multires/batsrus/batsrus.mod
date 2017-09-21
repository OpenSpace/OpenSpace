return {
    -- Volume module
    {   
        Name = "BATSRUS",
        Parent = "Root",
        Renderable = {
            Type = "RenderableMultiresVolume",
            TspType = "sand",
            ReferenceFrame = "HEEQ",
            Translation = {0, 0, 0},
            Rotation = {0, 0, 0},
            Scaling = {1, 1, 1},
            ScalingExponent = 13,
            Source = "${OPENSPACE_DATA}/short.tsp",
            TransferFunction = "../transferfunctions/fire.txt",
            BrickSelector = "shen"
        }
    }
}
