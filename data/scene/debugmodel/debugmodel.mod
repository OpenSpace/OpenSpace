return {
    {   
        Name = "DebugModel",
        Parent = "Root", 
        Renderable = {
            Type = "RenderableModel",
            Body = "SUN",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/NewHorizonsCleanModel.obj",
                Magnification = 4,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/NHTexture.jpg",
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        GuiName = "/Solar/DebugModel"
    },
}