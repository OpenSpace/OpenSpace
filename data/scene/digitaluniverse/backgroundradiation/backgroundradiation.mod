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
            Texture = "2-WMAP/textures/wmap_ilc_7yr_v4_200uK_RGB_sos.png",
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
            Texture = "2-COBE/textures/COBErect.png",
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
            Texture = "2-Planck/textures/cmb4k.jpg",
            Orientation = "Inside/Outside",
        },
        GuiPath = "/Universe/Cosmic Microwave Background"
     },

    --[[
    {   
        Name = "Wilkinson Microwave Anisotropy Probe",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            File = "2-WMAP/speck/wmap.speck",
            Texture = "2-WMAP/textures/ilc_9yr_moll4096BW.png",
            Unit = "Mpc",
        },
    },
    {   
        Name = "Cosmic Background Explorer",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            File = "2-COBE/speck/cobe.speck",
            Texture = "2-COBE/textures/403322main_COBEallsky_full.png",
            Unit = "Mpc",
        },
    },
    {   
        Name = "CMB Planck",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            File = "2-Planck/speck/planck.speck",
            Texture = "2-Planck/textures/Planck_CMB_BB.png",
            Unit = "Mpc",
        },
    },
    {   
        Name = "CMB PreCOBE",
        Parent = "Root",
        Renderable = {
            Type = "RenderableDUMeshes",
            File = "2-PreCOBE/speck/preCOBE.speck",
            Texture = "2-PreCOBE/textures/preCOBE.png",
            Unit = "Mpc",
        },
    }
   ]]--
}
