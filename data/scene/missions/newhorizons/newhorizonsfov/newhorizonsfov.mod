local LorriOffset = { -6.626, -4.1, -3.23 }
local RalphOffset = {   -6.9, -4.6,  8.7  }
local AliceOffset = {   -7.9, -1.7,  8.3  }
local RexOffset   = {      0,    0,    0  }

return {
    -- NewHorizonsFov main module
    {
        Name = "NH_LORRI",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            Color = { 0.8, 0.7, 0.7 },
            Instrument = {
                Name       = "NH_LORRI",
                Aberration = "NONE"
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = LorriOffset
            }
        }
    },

    -- NewHorizonsFov module NH_RALPH_LEISA
{
    Name = "NH_RALPH_LEISA",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_LEISA",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

-- NewHorizonsFov module NH_RALPH_MVIC_PAN1
{
    Name = "NH_RALPH_MVIC_PAN1",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_MVIC_PAN1",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

-- NewHorizonsFov module NH_RALPH_MVIC_PAN2
{
    Name = "NH_RALPH_MVIC_PAN2",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_MVIC_PAN2",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

-- NewHorizonsFov module NH_RALPH_MVIC_RED
{
    Name = "NH_RALPH_MVIC_RED",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_MVIC_RED",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

 -- NewHorizonsFov module NH_RALPH_MVIC_BLUE
 {
    Name = "NH_RALPH_MVIC_BLUE",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_MVIC_BLUE",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

-- NewHorizonsFov module NH_RALPH_MVIC_FT
{
    Name = "NH_RALPH_MVIC_FT",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_MVIC_FT",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

-- NewHorizonsFov module NH_RALPH_MVIC_METHANE
{
    Name = "NH_RALPH_MVIC_METHANE",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_MVIC_METHANE",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

-- NewHorizonsFov module NH_RALPH_MVIC_NIR
{
    Name = "NH_RALPH_MVIC_NIR",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_RALPH_MVIC_NIR",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = RalphOffset
        }
    }
},

-- NewHorizonsFov module NH_ALICE_AIRGLOW
{
    Name = "NH_ALICE_AIRGLOW",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_ALICE_AIRGLOW",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = AliceOffset
        }
    }
},

-- NewHorizonsFov module NH_ALICE_SOC
{
    Name = "NH_ALICE_SOC",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableFov",
        Body  = "NEW HORIZONS",
        Frame = "NH_SPACECRAFT",
        RGB   = { 0.8, 0.7, 0.7 },
        Instrument = {
            Name       = "NH_ALICE_SOC",
            Aberration = "NONE"
        },
        PotentialTargets = {
            "Pluto",
            "Charon",
            "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
        }
    },
    Transform = {
        Translation = {
            Type = "StaticTranslation",
            Position = AliceOffset
        },
    }
 },

-- Module NH_REX
{
    Name = "NH_REX",
    Parent = "NewHorizonsPosition",
    Renderable = {
        Type  = "RenderableCrawlingLine",
        Source = "NH_REX",
        Target = "EARTH",
        Instrument = "NH_REX",
        Color = {
            Start = { 1.0, 0.7, 0.0, 1.0},
            End = {0.0, 0.0, 0.0, 0.0 }
        }
    },
    Transform = {
        Rotation = {
            Type = "StaticRotation",
            Rotation = {
                0.0, 1.0, 0.0,
                0.0, 0.0, 1.0,
                1.0, 0.0, 0.0
            }
        },
        Translation = {
            Type = "StaticTranslation",
            Position = RexOffset
        }
    }
}
}
