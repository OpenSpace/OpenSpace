return {
    -- Stars module
    {   
        Name = "Constellation Bounds",
        Parent = "Root",
        Renderable = {
            Type = "RenderableConstellationBounds",
            File = "${OPENSPACE_DATA}/scene/constellationbounds/data/bound_20.dat",
            ConstellationFile = "${OPENSPACE_DATA}/scene/constellationbounds/data/constellations.dat",
            ReferenceFrame = "J2000"
        },
        Ephemeris = {
            Type = "Static"
        }
    }
}