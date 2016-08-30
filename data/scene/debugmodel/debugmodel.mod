return {
    {   
        Name = "DebugModel",
        Parent = "Root", 
        Renderable = {
            Type = "RenderableModel",
            Body = "SUN",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/OSIRIS-REx GSFC Animation/OSIRIS-REx GSFC Animation/OREXE1.obj",
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