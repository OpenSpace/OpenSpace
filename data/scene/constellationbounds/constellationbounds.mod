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
            File = "${OPENSPACE_DATA}/scene/constellationbounds/data/bound_20.dat",
            ConstellationFile = "${OPENSPACE_DATA}/scene/constellationbounds/data/constellations.dat",
            -- ConstellationSelection = zodiacs
        },
        Transform = {
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "J2000",
                DestinationFrame = "GALACTIC"
            }
        }
    }
}