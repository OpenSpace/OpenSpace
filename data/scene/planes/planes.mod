colors = {
    {1.0, 0.0, 0.0, 1.0},
    {0.0, 1.0, 0.0, 1.0},
    {0.0, 0.0, 1.0, 1.0}
}

result = {}

for i = 1, 100, 1 do
    result[i] = {
        Name = "Plane" .. i,
        Parent = "Root",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, i },
            -- Color = {math.floor(i/3), math.floor((i+1)/3), math.floor((i+2)/3)}
            Color = colors[((i - 1 )% 3) + 1]
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 1, i}
        }
    }

end

-- j = 1
-- print(result[j]['Renderable']['Color'][1])
-- print(result[j]['Renderable']['Color'][2])
-- print(result[j]['Renderable']['Color'][3])

return result
--  {

--     {
--         Name = "Plane1",
--         Parent = "Root",
--         Renderable = {
--             Type = "RenderablePlane",
--             Size = {1.0, 0.0},
--             Color = {1.0, 0.0, 0.0, 1.0}
--         },
--         Ephemeris = {
--             Type = "Static",
--             Position = { 0, 0, 0, 0}
--         },
--     },
--     {   
--         Name = "Plane2",
--         Parent = "Root",
--         Renderable = {
--             Type = "RenderablePlane",
--             Size = {1.0, 1.0},
--             Color = {1.0, 1.0, 0.0, 1.0}
--         },
--         Ephemeris = {
--             Type = "Static",
--             Position = { 0, 0, 1, 0}
--         },
--     },
--     {   
--         Name = "Plane3",
--         Parent = "Root",
--         Renderable = {
--             Type = "RenderablePlane",
--             Size = {1.0, 2.0},
--             Color = {1.0, 1.0, 1.0, 1.0}
--         },
--         Ephemeris = {
--             Type = "Static",
--             Position = { 0, 0, 1, 1}
--         },
--     }
-- }