return {
    -- Dwarf planet Ceres Body module
    {   
        Name = "Ceres",
        Parent = "SolarSystemBarycenter", 

        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_CERES",
            Body = "CERES",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 6.390E5,
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/gray.png",
            },
            StartTime = "2010 JAN 01 00:00:00",
            EndTime = "2018 JAN 22 12:00:00"
        },
        Ephemeris = {
            Type = "Spice",
            Body = "CERES",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/DawnKernels/pck/dawn_ceres_v01.tpc",
                "${OPENSPACE_DATA}/spice/DawnKernels/spk/sb_ceres_140724.bsp",
                "${OPENSPACE_DATA}/spice/DawnKernels/spk/sb_ceres_110211.bsp",
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_CERES",
            Reference = "GALACTIC"
        }
    },
    --[[ Ceres Trail Module
    {   
        Name = "CeresTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "CERES",
            Frame = "GALACTIC",
            Observer = "SUN",
            
            -- 3 Dummy values for compilation:
            TropicalOrbitPeriod = 500.0,
            EarthOrbitRatio = 0.2,
            DayLength = 2,
            -- End of Dummy values
            
            RGB = { 0.7, 0.5, 0.5 },
            Textures = {
                Type = "simple",
                Color = "textures/glare.png"
            },    
            StartTime = "2010 JAN 01T00:00:00",
            EndTime = "2018 JAN 22 12:00:00"
        }
    }
    --]]
}