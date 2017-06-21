#!/bin/lua

--For debug purposes, but also for generating raw output in case the parsing
-- isn't working as intended.
function tableLength(T)
  local count = 0
  for _ in pairs(T) do count = count + 1 end
  return count
end

output = dofile("satellites.mod")
outputLen = tableLength(output)

print("return {")
for i=1,outputLen do
  type=output[i].Renderable.Type
  print("  {")
  if( type == "RenderablePlane" ) then
    print("    Name = \"" .. output[i].Name .. "\",")
    print("    Parent = \"" .. output[i].Parent .. "\",")
    print("    Renderable = {")
    print("        Type = \"" .. output[i].Renderable.Type .. "\",")
    print("        Size = {".. output[i].Renderable.Size[1] .. "," .. output[i].Renderable.Size[2] .. "},")
    print("        Origin = \"".. output[i].Renderable.Origin .. "\",")
    print("        Body = \"" .. output[i].Renderable.Body .. "\",")
    if (output[i].Renderable.Billboard) then
      print("        Billboard = true,")
    else
      print("        Billboard = false,")
    end
    print("        Texture = \"" .. output[i].Renderable.Texture .. "\",")
    print("    },")
    print("    Transform = {")
    print("        Translation = {")
    print("            Type = \"" .. output[i].Transform.Translation.Type .. "\",")
    print("            Body = \"" .. output[i].Transform.Translation.Body .. "\",")
    print("            Observer = \"" .. output[i].Transform.Translation.Observer .. "\",")
    print("            File = \"" .. output[i].Transform.Translation.File .. "\",")
    print("            LineNum = " .. output[i].Transform.Translation.LineNum .. ",")
    print("        },")
    print("        Scale = {")
    print("            Type = \"" .. output[i].Transform.Scale.Type .. "\",")
    print("            Scale = " .. output[i].Transform.Scale.Scale .. ",")
    print("        },")
    print("    },")
  elseif( type == "RenderableTrailOrbit" ) then
    print("    Name = \"" .. output[i].Name .. "\",")
    print("    Parent = \"" .. output[i].Parent .. "\",")
    print("    Renderable = {")
    print("        Type = \"" .. output[i].Renderable.Type .. "\",")
    print("        Translation = {")
    print("            Type = \"" .. output[i].Renderable.Translation.Type .. "\",")
    print("            Body = \"" .. output[i].Renderable.Translation.Body .. "\",")
    print("            Observer = \"" .. output[i].Renderable.Translation.Observer .. "\",")
    print("            File = \"" .. output[i].Renderable.Translation.File .. "\",")
    print("            LineNum = " .. output[i].Renderable.Translation.LineNum .. ",")
    print("        },")
    print("        Color = {" .. output[i].Renderable.Color[1] .. "," .. output[i].Renderable.Color[2] .. "," .. output[i].Renderable.Color[3] .. "},")
    print("        Period = " .. output[i].Renderable.Period .. ",")
    print("        Resolution = " .. output[i].Renderable.Resolution .. ",")
    print("    },")
    print("    GuiName = \"" .. output[i].GuiName .. "\",")
  end
  print("  },")
end
print("}")
