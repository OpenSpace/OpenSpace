local Kernels = {
    '${SPICE}/voyager/vg2_v02.tf',
    '${SPICE}/voyager/vg200022.tsc',
    '${SPICE}/voyager/Voyager_2.m05016u.merged.bsp',
    '${SPICE}/voyager/voyager_2.ST+1992_m05208u.merged.bsp',
    '${SPICE}/voyager/vgr2_jup230.bsp',
    '${SPICE}/voyager/vgr2_sat337.bsp',
    '${SPICE}/voyager/vgr2_ura083.bsp',
    '${SPICE}/voyager/vgr2_nep081.bsp',
    '${SPICE}/voyager/vgr2_super.bc',
    '${SPICE}/voyager/vgr2_super_v2.bc'
}

local RotationMatrix = {
    -1, 0, 0,
    0, 0, -1,
    0, -1, 0
}

return {
    {
        Name = "Voyager 2",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "VG2_SC_BUS",
                DestinationFrame = "GALACTIC"
            }
        },
        GuiPath = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Main",
        Parent = "Voyager 2",
        Renderable = {
            Type = "RenderableModel",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "voyager-main.obj"
            },
            ColorTexture = "voyager-main.jpg",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Antanna",
        Parent = "Voyager 2",
        Renderable = {
            Type = "RenderableModel",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "voyager-antenna.obj"
            },
            ColorTexture = "voyager-antenna.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Voyager 2"
    },

    -- The trails are organized as follows.  The cruise phases can be resolved in relatively
    -- low resolution since they are just straight lines
    -- The encounter phases should be much higher resolution or otherwise artifacts would appear
    {
        Name = "Voyager 2 Trail Cruise Earth-Jupiter",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            Color = { 0.70,0.50,0.20 },
            StartTime = "1977 SEP 05",
            EndTime = "1979 JUL 06",
            SampleInterval = 669 * 2  -- 669 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Encounter Jupiter",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            Color = { 0.70,0.50,0.20 },
            EnableFade = false,
            StartTime = "1979 JUL 05 23:24:00", -- @TODO: Probably an off-by-one bug in RenderableTrailTrajectory?
            EndTime = "1979 JUL 15",
            SampleInterval = 100
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Cruise Jupiter-Saturn",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1979 JUL 15",
            EndTime = "1981 AUG 23",
            SampleInterval = 770 * 2  -- 770 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Encounter Saturn",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1981 AUG 22 23:08:30",  -- @TODO: Probably an off-by-one bug in RenderableTrailTrajectory?
            EndTime = "1981 AUG 30",
            SampleInterval = 100
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Cruise Saturn-Uranus",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1981 AUG 30",
            EndTime = "1986 JAN 22",
            SampleInterval = 1971  * 2  -- 1971 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Encounter Uranus",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1986 JAN 21 23:30:00", -- @TODO: Probably an off-by-one bug in RenderableTrailTrajectory?
            EndTime = "1986 JAN 27",
            SampleInterval = 100
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Cruise Uranus-Neptune",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1986 JAN 27",
            EndTime = "1989 AUG 24",
            SampleInterval = 1305  * 2  -- 1305 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Encounter Neptune",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1989 AUG 23 23:30:00", -- @TODO: Probably an off-by-one bug in RenderableTrailTrajectory?
            EndTime = "1989 AUG 26",
            SampleInterval = 100
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
    {
        Name = "Voyager 2 Trail Cruise Neptune-Inf",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 2",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1989 AUG 26",
            EndTime = "2021 JAN 01",
            SampleInterval = 11451 * 2  -- 11451 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 2"
    },
}
