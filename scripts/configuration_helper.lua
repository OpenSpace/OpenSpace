-- Helper functions that are useful to customize the openspace.cfg loading
-- The public functions are found in the second part of this file


-- SGCT related functions
sgct = {}

-- Internal helper functions
function generateSingleViewport(down, up, left, right)
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

function generateFisheyeViewport(fov, quality, tilt, background)
    return [[
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

function generateWindow(arg)
    local resolution = ""
    if arg["res"] then
        arg["res"][1] = arg["res"][1] or arg["windowSize"][1]
        arg["res"][2] = arg["res"][2] or arg["windowSize"][2]

        resolution =
[[
    <Res x="]] .. arg["res"][1] .. [[" y="]] .. arg["res"][2] .. [[" />
]]
    end

    return
[[
    <Window 
        fullScreen="]] .. tostring(arg["fullScreen"]) .. [["
        numberOfSamples="]] .. arg["msaa"] .. [["
        border="]] .. tostring(arg["border"]) .. [["
        name="]] .. arg["name"] .. [["
        monitor="]] .. arg["monitor"] .. [["
    >
        <Stereo type="]] .. arg["stereo"] .. [[" />
        <Size x="]] .. arg["windowSize"][1] .. [[" y="]] .. arg["windowSize"][2] .. [[" />
        <Pos x="]].. arg["windowPos"][1] ..[[" y="]] .. arg["windowPos"][2] .. [[" />
]]..resolution..
[[
]]..
arg["viewport"]..
[[
    </Window>
]]
end

function generateUser(arg)
    return [[
    <User eyeSeparation="]] .. arg["eyeSep"] .. [[">
        <Pos
            x="]] .. arg["eyePos"][1] .. [["
            y="]] .. arg["eyePos"][2] .. [["
            z="]] .. arg["eyePos"][3] .. [["
        />
    </User>
]]
end


function generateCluster(arg)
    local v
    if vsync then
        v = 1
    else
        v = 0
    end

    return [[
<?xml version="1.0" ?>
<Cluster
    masterAddress="localhost"
    externalControlPort="20500"
    debug="]] .. tostring(arg["sgctDebug"]) .. [["
>
    <Settings><Display swapInterval="]].. v ..[[" /></Settings>
    <Node address="localhost" port="20401">
]].. arg["window"] ..[[
    </Node>
]] .. arg["user"] .. [[
</Cluster>
]]
end

function generateSingleWindowConfig(arg)
    -- First some type checking
    assert(
        type(arg[1]) == "number" or type(arg[1]) == "nil",
        "First argument must be a number or nil"
    )
    assert(
        type(arg[2]) == "number" or type(arg[2]) == "nil",
        "Second argument must be a number or nil"
    )

    assert(
        type(arg["res"]) == "table" or type(arg["res"]) == "nil",
        "res must be a table or nil"
    )
    if (type(arg["res"]) == "table") then
        assert(type(arg["res"][1]) == "number", "res[1] must be a number")
        assert(type(arg["res"][2]) == "number", "res[2] must be a number")
    end

    assert(
        type(arg["windowSize"]) == "table" or type(arg["windowSize"]) == "nil",
        "windowSize must be a table or nil"
    )
    if (type(arg["windowSize"]) == "table") then
        assert(type(arg["windowSize"][1]) == "number", "windowPos[1] must be a number")
        assert(type(arg["windowSize"][2]) == "number", "windowPos[2] must be a number")
        assert(
            type(arg[1]) == "nil" and type(arg[2]) == "nil",
            "Only windowSize or the first and second arguments can be set. Not both"
        )
    end

    assert(
        type(arg["windowPos"]) == "table" or type(arg["windowPos"]) == "nil",
        "windowPos must be a table or nil"
    )
    if (type(arg["windowPos"]) == "table") then
        assert(type(arg["windowPos"][1]) == "number", "windowPos[1] must be a number")
        assert(type(arg["windowPos"][2]) == "number", "windowPos[2] must be a number")
    end

    assert(
        type(arg["name"]) == "string" or type(arg["name"]) == "nil",
        "name must be a string or nil"
    )

    assert(
        type(arg["fullScreen"]) == "boolean" or type(arg["fullScreen"]) == "nil",
        "fullScreen must be a boolean or nil"
    )

    assert(
        type(arg["monitor"]) == "number" or type(arg["monitor"]) == "nil",
        "monitor must be a number or nil"
    )

    assert(
        type(arg["border"]) == "boolean" or type(arg["border"]) == "nil",
        "border must be a boolean or nil"
    )

    assert(
        type(arg["msaa"]) == "number" or type(arg["msaa"]) == "nil",
        "msaa must be a number or nil"
    )

    assert(
        type(arg["vsync"]) == "boolean" or type(arg["vsync"]) == "nil",
        "vsync must be a boolean or nil"
    )
    
    assert(
        type(arg["stereo"]) == "string" or type(arg["stereo"]) == "nil",
        "stereo must be a boolean or nil"
    )

    assert(
        type(arg["eyeSep"]) == "number" or type(arg["eyeSep"]) == "nil",
        "eyeSep must be a number or nil"
    )

    assert(
        type(arg["eyePos"]) == "table" or type(arg["eyePos"]) == "nil",
        "eyePos must be a table or nil"
    )
    if (type(arg["eyePos"]) == "table") then
        assert(type(arg["eyePos"][1]) == "number", "eyePos[1] must be a number")
        assert(type(arg["eyePos"][2]) == "number", "eyePos[2] must be a number")
        assert(type(arg["eyePos"][3]) == "number", "eyePos[3] must be a number")
    end

    assert(type(arg["sgctDebug"]) == "boolean" or type(arg["sgctDebug"]) == "nil",
        "sgctDebug must be a boolean or nil"
    )

    assert(type(arg["viewport"]) == "string", "viewport must be a string")


    -- Then setting reasonable default values
    arg["vsync"] = arg["vsync"] or false
    arg["fullScreen"] = arg["fullScreen"] or false
    arg["monitor"] = arg["monitor"] or 0
    arg["msaa"] = arg["msaa"] or 8
    arg["border"] = arg["border"] or true
    arg["name"] = arg["name"] or "OpenSpace"
    arg["stereo"] = arg["stereo"] or "none"
    arg["windowPos"] = arg["windowPos"] or { 50, 50 }
    arg["eyeSep"] = arg["eyeSep"] or 0.065
    arg["eyePos"] = arg["eyePos"] or { 0.0, 0.0, 0.0 }
    arg["sgctDebug"] = arg["sgctDebug"] or false

    if not arg["windowSize"] then
        arg["windowSize"] = {}
        arg["windowSize"][1] = arg[1] or 1280
        arg["windowSize"][2] = arg[2] or 720
    end

    arg["window"] = generateWindow(arg)
    arg["user"] = generateUser(arg)

    return generateCluster(arg)
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
    local configFile = os.tmpname()

    local file = io.open(configFile, "w+")

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


-- Thus this function can be called the following ways:
-- sgct.config.single() -> Leading to a 1280x720 resolution window
-- sgct.config.single(1920, 1080) -> Leading to a 1920x1080 resolution window
-- sgct.config.single(640, 360, res={3840, 2160}) -> 640x360 window with 4K rendering resolution
-- sgct.config.single(msaa=1) -> 1280x720 resolution without multisampling
function sgct.config.single(arg)
    arg = arg or {}

    assert(
        type(arg["fov"]) == "table" or type(arg["fov"]) == "nil",
        "fov must be a table or nil"
    )
    if (type(arg["fov"]) == "table") then
        assert(type(arg["fov"]["left"]) == "number",  "fov['left'] must be a number")
        assert(type(arg["fov"]["right"]) == "number", "fov['right'] must be a number")
        assert(type(arg["fov"]["up"]) == "number",    "fov['up'] must be a number")
        assert(type(arg["fov"]["down"]) == "number",  "fov['down'] must be a number")
    end
    
    arg["fov"] = arg["fov"] or { down = 16.875, up = 16.875, left = 30.0, right = 30.0 }
    arg["viewport"] = generateSingleViewport(
        arg["fov"]["down"],
        arg["fov"]["up"], 
        arg["fov"]["left"],
        arg["fov"]["right"]
    )

    return sgct.makeConfig(generateSingleWindowConfig(arg))
end

function sgct.config.fisheye(arg)
    arg = arg or {}

    assert(
        type(arg["fov"]) == "number" or type(arg["fov"]) == "nil",
        "fov must be a number or nil"
    )

    assert(
        type(arg["quality"]) == "string" or type(arg["quality"]) == "nil",
        "quality must be a string or nil"
    )

    assert(
        type(arg["tilt"]) == "number" or type(arg["tilt"]) == "nil",
        "tilt must be a number or nil"
    )

    assert(
        type(arg["background"]) == "table" or type(arg["background"]) == "nil",
        "background must be a table or nil"
    )
    if type(arg["background"]) == "table" then
        assert(type(background["r"]) == "number", "backgroud['r'] must be a number")
        assert(type(background["g"]) == "number", "backgroud['g'] must be a number")
        assert(type(background["b"]) == "number", "backgroud['b'] must be a number")
        assert(type(background["a"]) == "number", "backgroud['a'] must be a number")
    end

    arg["fov"] = arg["fov"] or 180
    arg["quality"] = arg["quality"] or "1k"
    arg["tilt"] = arg["tilt"] or 0
    arg["background"] = arg["background"] or { r = 0.1, g = 0.1, b = 0.1, a = 1.0 }

    arg["viewport"] = generateFisheyeViewport(
        arg["fov"],
        arg["quality"],
        arg["tilt"],
        arg["background"]
    )

    return sgct.makeConfig(generateSingleWindowConfig(arg))
end
