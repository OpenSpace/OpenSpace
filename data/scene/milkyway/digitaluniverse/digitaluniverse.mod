return {
    {
        Name = "MilkyWay",
        Parent = "SolarSystem",
        -- SphereOfInfluency unit is meters                
		-- SphereOfInfluency = 20.0E+22, 
        Renderable = {
            Type = "RenderableSphere",
            Size = 10E22,
            Segments = 40,
            Alpha = 0.2,
            Texture = "textures/DarkUniverse_mellinger_8k.jpg",
            Orientation = "Inside/Outside"
        }
    }
}
