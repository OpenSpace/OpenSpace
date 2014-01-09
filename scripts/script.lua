--config = assert(loadfile("../../scripts/config.lua"))()

io.input("../../scripts/config.lua")
file = io.read("*all")
config = assert(load("return " .. file))()


--t = {a=1, b="foo", c= 0.1 }

--print(_G)
-- for key,value in pairs(_G) do print(key,value) end
-- a = {}
-- local x = 20
-- for i=1,10 do
    -- local y = 0
    -- a[i] = function () y=y+1; return x+y end
-- end
-- for key,value in pairs(a) do print(key,value()) end

-- m = getmetatable(x)
--m = getfenv(m)
-- print(m)
--for key,value in pairs(m) do print(key,value) end

-- for key,value in pairs(debug.getregistry()) do print(key,value) end
-- print (debug.getregistry())
-- print(debug)