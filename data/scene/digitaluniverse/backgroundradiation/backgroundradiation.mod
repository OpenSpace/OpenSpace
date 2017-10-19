return {
    -- Background Radiation module
     {
        Name = "Wilkinson Microwave Anisotropy Probe (WMAP)",
        Parent = "Root",
        Renderable = {
            Type = "RenderableSphere",
            Size = 3975.41417036064E23,
            Segments = 80,
            Alpha = 0.5,
            Texture = "textures/wmap_ilc_7yr_v4_200uK_RGB_sos.png",
            Orientation = "Inside/Outside"
        },
        GuiPath = "/Universe/Cosmic Microwave Background"
     },
     {
        Name = "Cosmic Background Explorer",
        Parent = "Root",
        Renderable = {
            Type = "RenderableSphere",
            Size = 3975.41417036064E23,
            Segments = 80,
            Alpha = 0.5,
            Texture = "textures/COBErect.png",
            Orientation = "Inside/Outside"
        },
        GuiPath = "/Universe/Cosmic Microwave Background"
     },
     {
        Name = "Planck",
        Parent = "Root",
        Renderable = {
            Type = "RenderableSphere",
            Size = 3975.41417036064E23,
            Segments = 80,
            Alpha = 0.5,
            Texture = "textures/cmb4k.jpg",
            Orientation = "Inside/Outside",
        },
        GuiPath = "/Universe/Cosmic Microwave Background"
     },
}
