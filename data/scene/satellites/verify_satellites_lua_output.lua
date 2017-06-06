#!/bin/lua

function tableLength(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end

output = dofile("satellites.mod")
outputLen = tableLength(output)

for i=1,outputLen do
  type=output[i].Renderable.Type
  print("------------", i)
  if( type == "RenderablePlane" ) then
    print(output[i].Name)
    print(output[i].Parent)
    print(output[i].Renderable.Type)
    print(output[i].Renderable.Size[1])
    print(output[i].Renderable.Size[2])
    print(output[i].Renderable.Origin)
    print(output[i].Renderable.Body)
    print(output[i].Renderable.Billboard)
    print(output[i].Renderable.Texture)
    print(output[i].Transform.Translation.Type)
    print(output[i].Transform.Translation.Body)
    print(output[i].Transform.Translation.Observer)
    print(output[i].Transform.Translation.File)
    print(output[i].Transform.Translation.LineNum)
    print(output[i].Transform.Scale.Type)
    print(output[i].Transform.Scale.Scale)
  elseif( type == "RenderableTrailOrbit" ) then
    print(output[i].Name)
    print(output[i].Parent)
    print(output[i].Renderable.Type)
    print(output[i].Renderable.Translation.Type)
    print(output[i].Renderable.Translation.Body)
    print(output[i].Renderable.Translation.Observer)
    print(output[i].Renderable.Translation.File)
    print(output[i].Renderable.Translation.LineNum)
    print(output[i].Renderable.Color[1])
    print(output[i].Renderable.Color[2])
    print(output[i].Renderable.Color[3])
    print(output[i].Renderable.Period)
    print(output[i].Renderable.Resolution)
    print(output[i].GuiName)
  end
end
