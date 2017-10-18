local zodiacs = {
    "Cancer", "Taurus", "Pisces", "Aries", "Libra", "Aquarius", "Capricornus", "Scorpius",
    "Virgo", "Sagittarius", "Gemini", "Leo"
}

return {
    -- Stars module
    {
        Name = "Constellation Bounds",
        Parent = "Root",
        Renderable = {
            Type = "RenderableConstellationBounds",
            File = "data/bound_20.dat",
            ConstellationFile = "data/constellations.dat"
            -- ConstellationSelection = zodiacs
        },
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "J2000",
                DestinationFrame = "GALACTIC"
            },
            Scale = {
                Type = "StaticScale",
                Scale = 10e17
            }
        },
        GuiPath = "/Milky Way/Constellations"
    }
}
