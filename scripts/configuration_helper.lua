-- Helper functions that are useful to customize the openspace.cfg loading
-- The public functions are found in the second part of this file


-- SGCT related functions
sgct = {}

-- Internal helper functions
local function generateResolution(res)

end

local function generateSingleViewport(down, up, left, right)
    assert(type(down) == "number", "down must be a number")
    assert(type(up) == "number", "up must be a number")
    assert(type(left) == "number", "left must be a number")
    assert(type(right) == "number", "right must be a number")

    return
[[
<Viewport>
    <Pos x="0.0" y="0.0" />
    <Size x="1.0" y="1.0" />
    <PlanarProjection>
        <FOV down="]]..down..[[" left="]]..left..[[" right="]]..right..[[" up="]]..up..[[" />
        <Orientation heading="0.0" pitch="0.0" roll="0.0" />
    </PlanarProjection>
</Viewport>
]]
end

local function generateFisheyeViewport(fov, quality, tilt, background)
    assert(type(fov) == "number", "fov must be a number")
    assert(type(quality) == "string", "quality must be a string")
    assert(type(tilt) == "number", "tilt must be a number")
    
    assert(type(background) == "table", "background must be a table")
    assert(type(background["r"]) == "number", "backgroud['r'] must be a number")
    assert(type(background["g"]) == "number", "backgroud['g'] must be a number")
    assert(type(background["b"]) == "number", "backgroud['b'] must be a number")
    assert(type(background["a"]) == "number", "backgroud['a'] must be a number")

return
[[
    <Viewport name="fisheye">
        <Pos x="0.0" y="0.0" />
        <Size x="1.0" y="1.0" />
        <FisheyeProjection fov="]]..fov..[[" quality="]]..quality..[[" tilt="]]..tilt..[[">
            <Background 
                r="]]..background["r"]..[["
                g="]]..background["g"]..[["
                b="]]..background["b"]..[["
                a="]]..background["a"]..[["
            />
        </FisheyeProjection>
    </Viewport>
]]
end

local function generateWindow(fullScreen, msaa, border, name, stereo, windowSize, res, pos, viewport)
    assert(type(fullScreen) == "boolean", "fullScreen must be a boolean")
    assert(type(msaa) == "number", "msaa must be a number")
    assert(type(border) == "boolean", "border must be a boolean")
    assert(type(name) == "string", "name must be a string")
    assert(type(stereo) == "string", "stereo must be a string")

    assert(type(windowSize) == "table", "windowSize must be a table")
    assert(type(windowSize[1]) == "number", "windowSize[1] must be a number")
    assert(type(windowSize[2]) == "number", "windowSize[2] must be a number")

    assert(type(pos) == "table", "windowSize must be a table")
    assert(type(pos[1]) == "number", "windowSize[1] must be a number")
    assert(type(pos[2]) == "number", "windowSize[2] must be a number")

    assert(type(viewport) == "string", "viewport must be a string")


    if res == nil or res[1] == nil or res[2] == nil then
        resolution = ""
    else
        assert(type(res[1]) == "number", "res[1] must be a number")
        assert(type(res[2]) == "number", "res[2] must be a number")
        resolution =
[[
    <Res x="]] .. res[1] .. [[" y="]] .. res[2] .. [[" />
]]
    end

    return
[[
    <Window 
        fullScreen="]] .. tostring(fullScreen) .. [["
        numberOfSamples="]] .. msaa .. [["
        border="]] .. tostring(border) .. [["
        name="]] .. name .. [["
    >
        <Stereo type="]] .. stereo .. [[" />
        <Size x="]] .. windowSize[1] .. [[" y="]] .. windowSize[2] .. [[" />
        <Pos x="]].. pos[1] ..[[" y="]] .. pos[2] .. [[" />
]]..resolution..
[[
]]..
viewport..
[[
    </Window>
]]
end


--[[
########################################################################################## 
##########################################################################################

                            Public functions

########################################################################################## 
########################################################################################## 
]]--

-- This function takes a string and returns a filename containing the string
-- making it possible to specify SGCT configuration files from a Lua script by
-- returning a string representation
function sgct.makeConfig(config)
    configFile = os.tmpname()


    file = io.open(configFile, "w+")

    file:write(config)

    io.close(file)
    return configFile
end

sgct.config = {}

-- Creates a configuration file similar to the default 'single.xml'
-- The parameter is a table and can contain the follow attributes:
-- first argument: horizontal window size {default: 1280}
-- second argument: vertical window size {default: 720}
-- res: A table containing the horizontal and vertical resolution [example: res={3840, 2160}]
-- pos: The position of the window on the screen [example: pos={50, 100}] {default: {50, 50}}
-- fullScreen: Whether the application should run in exclusive full screen [example: fullScreen=true] {default: false}
-- border: Whether the application should have window decorations (aka. border) [example: border=false] {default: true}
-- msaa: The multisampling anti-aliasing factor [example: msaa=4] {default: 8}
-- vsync: Whether the rendering speed is locked to the refreshrate [example: vsync=true] {default: false}
-- stereo: Select the stereo rendering mode as supported by SGCT [example: stereo='anaglyph_red_cyan'] {default: 'none'}
-- fov: The field of view settings [example: fov={ left=20, right=30, up=10, down=50}] {default: { left=30.0, right=30.0, up=16.875, down=16.875}}

-- 
--
-- Thus this function can be called the following ways:
-- sgct.config.single() -> Leading to a 1280x720 resolution window
-- sgct.config.single(1920, 1080) -> Leading to a 1920x1080 resolution window
-- sgct.config.single(640, 360, res={3840, 2160}) -> 640x360 window with 4K rendering resolution
-- sgct.config.single(msaa=1) -> 1280x720 resolution without multisampling
function sgct.config.single(arg)
    -- Set default values
    windowX = arg[1] or 1280
    windowY = arg[2] or 720
    res = arg["res"]
    if res then
        res[1] = res[1] or windowX
        res[2] = res[2] or windowY
    end
    pos = arg["pos"] or {50, 50}
    fullScreen = arg["fullScreen"] or false
    border = arg["border"] or true
    msaa = arg["msaa"] or 8
    vsync = arg["vsync"] or false
    stereo = arg["stereo"] or "none"
    fov = arg["fov"] or { down = 16.875, up = 16.875, left = 30.0, right = 30.0 }

    -- Convert values into XML style
    if vsync then
        vsync = 1
    else
        vsync = 0
    end

    viewport = generateSingleViewport(fov["down"], fov["up"], fov["left"], fov["right"])

    return sgct.makeConfig([[
<?xml version="1.0" ?>
<Cluster masterAddress="localhost" externalControlPort="20500">
    <Settings><Display swapInterval="]].. vsync ..[[" /></Settings>
    <Node address="localhost" port="20401">
]]..
    generateWindow(fullScreen, msaa, border, "OpenSpace", stereo,
        {windowX, windowY}, res, pos, viewport)..
[[
    </Node>
    <User eyeSeparation="0.065">
        <Pos x="0.0" y="0.0" z="0.0" />
    </User>
</Cluster>
]])
end
