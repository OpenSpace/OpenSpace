config = {}

function merge(t1, t2)
    for k, v in pairs(t2) do
        if (type(v) == "table") and (type(t1[k] or false) == "table") then
            merge(t1[k], t2[k])
        else
            t1[k] = v
        end
    end
    return t1
end

--[[
function printTable(t, i)
    i = i or ""
    for k,v in pairs(t) do
        if (type(v) == "table") then
            print(i .. k)
            printTable(v, i .. " ")
        else
            print(i ..k .. ' , ' .. v)
        end
    end
end
]]--

function loadConfiguration(file)
    io.input(file)
    contents = io.read("*all")
    source = "return " .. contents
    settings = assert(load(source))()
    
    merge(config, settings or {})
end

function getValue(key, t)
    t = t or config -- default value of 'config'
    pos = key:find('[.]')
    if (not pos) then
        return t[key]
    else
        newKey = t[key:sub(0, pos - 1)]
        if (not newKey) then
            return nil
        else
            return getValue(key:sub(pos + 1), newKey)
        end
    end
end

function setValue(key, v, t)
    t = t or config -- default value of 'config'
    pos = key:find('[.]')
    if (not pos) then
        t[key] = v
        return true
    else
        newKey = t[key:sub(0, pos - 1)]
        if (not newKey) then
            return false
        else
            return setValue(key:sub(pos + 1), v, newKey)
        end
    end
end