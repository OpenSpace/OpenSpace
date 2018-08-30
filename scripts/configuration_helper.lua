-- Helper functions that are useful to customize the openspace.cfg loading

--[[
########################################################################################## 
                            Public functions
########################################################################################## 
]]--

-- SGCT related functions
sgct = {}
sgct.config = {}

-- This function takes a text definition for an SGCT configuration file and returns the path
-- to a temporary file containing the string which SGCT can use 
function sgct.makeConfig(config) end

-- Creates a configuration file similar to the default 'single.xml':
-- The parameter is a table and can contain the follow attributes:

-- first argument: horizontal window size {default: 1280}
-- second argument: vertical window size {default: 720}
-- res: A table containing the horizontal and vertical resolution [example: res={3840, 2160}]
-- windowPos: The position of the window on the screen [example: windowPos={50, 100}] {default: {50, 50}}
-- fullScreen: Whether the application should run in exclusive full screen [example: fullScreen=true] {default: false}
-- border: Whether the application should have window decorations (aka. border) [example: border=false] {default: true}
-- monitor: Determines the monitor on which the application is started [example: monitor=2] {default: 0}
-- shared: Determines whether the contents of the window should be shared using the SPOUT library [example: shared=true] {default: false}

-- Expert settings:
-- name: The name of the window [example: window="Foobar"] {default: "OpenSpace"}
-- tags: A list of string tags that are passed to the window [example: tags ={"GUI"}] {default: {}}
-- vsync: Whether the rendering speed is locked to the refreshrate [example: vsync=true] {default: false}
-- refreshRate: If vsync is enabled, this is the target framerate [example: refreshRate=30] {default: infinity}
-- stereo: Select the stereo rendering mode as supported by SGCT [example: stereo='anaglyph_red_cyan'] {default: 'none'}
-- msaa: The multisampling anti-aliasing factor [example: msaa=8] {default: 4}
-- eyeSep: The base eye separation in m [example: eyeSep=0.1] {default: 0.065}
-- eyePos: The location of the user [example: eyePos={0.0, 1.0, 0.0}] {default: {0.0, 0.0, 0.0}}
-- scene: Global settings to all scene objects (offset, orientation, scaling; each optional)
--      [example: scene = {offset = {x = 1.0, y = 1.0, z = 2.0}, orientation = { yaw = 120, pitch = 15, roll = 0.0 }, scale = 10.0}]
-- capture: Settings to configure the image capture [example: capture = { path = "./images"]
-- sgctDebug: Determines whether a higher debug level in SGCT is enabled [example: sgctDebug=true] {default: false}
-- fov: The field of view settings [example: fov={ left=20, right=30, up=10, down=50}] {default: { left=40.0, right=40.0, up=22.5, down=22.5}}
-- tracked: Determines whether the aspect ratio of the camera defined at application startup should persist when the window is resized [example: tracked=false] {default: true}

-- Thus this function can be called the following ways:
-- sgct.config.single() -> Leading to a 1280x720 resolution window
-- sgct.config.single(1920, 1080) -> Leading to a 1920x1080 resolution window
-- sgct.config.single(640, 360, res={3840, 2160}) -> 640x360 window with 4K rendering resolution
-- sgct.config.single(msaa=1) -> 1280x720 resolution without multisampling
function sgct.config.single(arg) end

-- Creates a configuration file similar to the default 'single_fisheye.xml'
-- The parameter is a table and can contain the follow attributes:

-- first argument: horizontal window size {default: 1280}
-- second argument: vertical window size {default: 720}
-- res: A table containing the horizontal and vertical resolution [example: res={3840, 2160}]
-- windowPos: The position of the window on the screen [example: windowPos={50, 100}] {default: {50, 50}}
-- fullScreen: Whether the application should run in exclusive full screen [example: fullScreen=true] {default: false}
-- border: Whether the application should have window decorations (aka. border) [example: border=false] {default: true}
-- monitor: Determines the monitor on which the application is started [example: monitor=2] {default: 0}
-- shared: Determines whether the contents of the window should be shared using the SPOUT library [example: shared=true] {default: false}

-- Expert settings:
-- name: The name of the window [example: window="Foobar"] {default: "OpenSpace"}
-- tags: A list of string tags that are passed to the window [example: tags ={"GUI"}] {default: {}}
-- vsync: Whether the rendering speed is locked to the refreshrate [example: vsync=true] {default: false}
-- refreshRate: If vsync is enabled, this is the target framerate [example: refreshRate=30] {default: infinity}
-- stereo: Select the stereo rendering mode as supported by SGCT [example: stereo='anaglyph_red_cyan'] {default: 'none'}
-- msaa: The multisampling anti-aliasing factor [example: msaa=8] {default: 4}
-- eyeSep: The base eye separation in m [example: eyeSep=0.1] {default: 0.065}
-- eyePos: The location of the user [example: eyePos={0.0, 1.0, 0.0}] {default: {0.0, 0.0, 0.0}}
-- scene: Global settings to all scene objects (offset, orientation, scaling; each optional)
--      [example: scene = {offset = {x = 1.0, y = 1.0, z = 2.0}, orientation = { yaw = 120, pitch = 15, roll = 0.0 }, scale = 10.0}]
-- capture: Settings to configure the image capture [example: capture = { path = "./images"]
-- sgctDebug: Determines whether a higher debug level in SGCT is enabled [example: sgctDebug=true] {default: false}
-- fov: The field of view for the fisheye [example: fov=360] {default: 180}
-- quality: The quality setting for the cubemap textures [example: quality="4k"] {default: "1k"}
-- tilt: The forwards tilt of the fisheye, relative to the center [example: tilt=90] {default: 0.0}
-- background: The background color used outside of the fisheye rendering [example: backgruound={r=1.0, g=0.25, b=0.25, a=1.0}] {default: {r=0.1, g=0.1, b=0.1, a=1.0}}

-- Thus this function can be called the following ways:
-- sgct.config.fisheye() -> Leading to a 1280x720 resolution window
-- sgct.config.fisheye(640, 640) -> Leading to a 640x650 resolution window
-- sgct.config.fisheye(640, 360, res={3840, 3840}) -> 640x360 window with 4K rendering resolution
-- sgct.config.fisheye(msaa=1) -> 1280x720 resolution without multisampling
function sgct.config.fisheye(arg) end

function sgct.config.cube(arg) end


--[[
########################################################################################## 
                            Internal helper functions
########################################################################################## 
]]--

function generateSingleViewportFOV(down, up, left, right, tracked)
    return
[[
<Viewport ]]..tracked..[[>
    <Pos x="0.0" y="0.0" />
    <Size x="1.0" y="1.0" />
    <PlanarProjection>
        <FOV down="]]..down..[[" left="]]..left..[[" right="]]..right..[[" up="]]..up..[[" />
        <Orientation heading="0.0" pitch="0.0" roll="0.0" />
    </PlanarProjection>
</Viewport>
]]
end



function generateSingleViewport(lowerLeft, upperLeft, upperRight, tracked)
    return
[[
<Viewport ]]..tracked..[[>
    <Pos x="0.0" y="0.0" />
    <Size x="1.0" y="1.0" />
    <Projectionplane>
        <Pos x="]] .. lowerLeft[1] .. [[" y="]] .. lowerLeft[2] .. [[" z="]] .. lowerLeft[3] .. [[" />
        <Pos x="]] .. upperLeft[1] .. [[" y="]] .. upperLeft[2] .. [[" z="]] .. upperLeft[3] .. [[" />
        <Pos x="]] .. upperRight[1] .. [[" y="]] .. upperRight[2] .. [[" z="]] .. upperRight[3] .. [[" />
    </Projectionplane>
</Viewport>
]]
end



function generateFisheyeViewport(fov, quality, tilt, background, crop, offset, trackedSpecifier)
    local b = [[
            <Background 
                r="]]..background["r"]..[["
                g="]]..background["g"]..[["
                b="]]..background["b"]..[["
                a="]]..background["a"]..[["
            />
]]

    local c = ""
    if crop then
        c = [[
            <Crop
                left="]] .. crop["left"] .. [["
                right="]] .. crop["right"] .. [["
                top="]] .. crop["top"] .. [["
                bottom="]] .. crop["bottom"] .. [["
            />
]]
    end

    local o = ""
    if offset then
        o = [[
            <Offset
                x="]] .. offset["x"] .. [["
                y="]] .. offset["y"] .. [["
                z="]] .. offset["z"] .. [["
            />
]]
    end

    return [[
    <Viewport name="fisheye" ]]..trackedSpecifier..[[>
        <Pos x="0.0" y="0.0" />
        <Size x="1.0" y="1.0" />
        <FisheyeProjection fov="]]..fov..[[" quality="]]..quality..[[" tilt="]]..tilt..[[">
]]..b..[[
]]..c..[[
]]..o..[[
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

    local tags = ""
    if arg["tags"] then
        tags = table.concat(arg["tags"], ",")
    end


    return
[[
    <Window 
        fullScreen="]] .. tostring(arg["fullScreen"]) .. [["
        numberOfSamples="]] .. arg["msaa"] .. [["
        border="]] .. tostring(arg["border"]) .. [["
        name="]] .. arg["name"] .. [["
        monitor="]] .. arg["monitor"] .. [["
        tags="]] .. tags .. [["
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



function generateScene(arg)
    local scene = arg["scene"]

    if scene == nil then
        return ""
    else
        local offset = nil
        if scene["offset"] then
            local o = scene["offset"]
            offset = [[<Offset x="]]..o["x"]..[[" y="]]..o["y"]..[[" z="]]..o["z"]..[[" />]]
        end

        local orientation = nil
        if scene["orientation"] then
            local o = scene["orientation"]
            orientation = [[<Orientation yaw="]]..o["yaw"]..[[" pitch="]]..o["pitch"]..[[" roll="]]..o["roll"]..[[" />]]
        end

        local scale = nil
        if scene["scale"] then
            scale = [[<Scale value="]] .. scene["scale"] .. [[" />]]
        end

        local sceneString = "    <Scene>"
        if offset then
            sceneString = sceneString .. "\n        " .. offset .. "\n"
        end
        if orientation then
            sceneString = sceneString .. "\n        " .. orientation .. "\n"
        end
        if scale then
            sceneString = sceneString .. "\n        " .. scale .. "\n"
        end

        sceneString = sceneString .. "    </Scene>\n"

        return sceneString

    --     return [[
    -- <Scene>
    --     ]]..offset..[[
    --     ]]..orientation..[[
    --     ]]..scale..[[
    -- </Scene>]]
    end
end



function generateSettings(arg)
    local v
    if arg["vsync"] then
        v = 1
    else
        v = 0
    end

    local refresh = ""
    if arg["refreshRate"] then
        refresh = "refreshRate=\"" .. arg["refreshRate"] .. "\" "
    end



    return [[
    <Settings>
        <Display swapInterval="]].. v ..[[" ]] .. refresh .. [[/>
    </Settings>
]]
end



function generateCapture(arg)
    if arg["capture"] == nil then
        return ""
    else
        local path = ""
        if arg["capture"]["path"] then
            path = 'path="' .. arg["capture"]["path"] .. '" '
        end

        local monoPath = ""
        if arg["capture"]["monoPath"] then
            path = 'monoPath="' .. arg["capture"]["monoPath"] .. '" '
        end

        local leftPath = ""
        if arg["capture"]["leftPath"] then
            path = 'leftPath="' .. arg["capture"]["leftPath"] .. '" '
        end

        local rightPath = ""
        if arg["capture"]["rightPath"] then
            path = 'rightPath="' .. arg["capture"]["rightPath"] .. '" '
        end

        local format = ""
        if arg["capture"]["format"] then
            path = 'format="' .. arg["capture"]["format"] .. '" '
        end
    end
end



function generateCluster(arg)
    return [[
<?xml version="1.0" ?>
<Cluster
    masterAddress="localhost"
    externalControlPort="20500"
    debug="]] .. tostring(arg["sgctDebug"]) .. [["
>
]] .. (arg["settings"] or "") .. [[
]] .. (arg["scene"] or "") .. [[
    <Node address="localhost" port="20401">
]] .. arg["window"] ..[[
    </Node>
]] .. arg["user"] .. [[
]] .. (arg["capture"] or "") .. [[
</Cluster>
]]
end



function generateSingleWindowConfig(arg)
    -- First some type checking
    assert(
        type(arg["res"]) == "table" or type(arg["res"]) == "nil",
        "res must be a table or nil"
    )
    if (type(arg["res"]) == "table") then
        assert(type(arg["res"][1]) == "number", "res[1] must be a number")
        assert(type(arg["res"][2]) == "number", "res[2] must be a number")
    end

    assert(
        type(arg["shared"]) == "boolean" or type(arg["shared"]) == "nil",
        "shared must be a boolean or nil"
    )

    assert(
        type(arg["tags"]) == "table" or type(arg["tags"]) == "nil",
        "tags must be a table or nil"
    )
    if (type(arg["tags"]) == "table") and (next(arg["tags"]) ~= nil) then
        for index, value in ipairs(arg["tags"]) do
            assert(type(value) == "string", "Each tag must be a string")
        end
    end

    assert(
        type(arg["windowSize"]) == "table" or type(arg["windowSize"]) == "nil",
        "windowSize must be a table or nil"
    )
    if (type(arg["windowSize"]) == "table") then
        assert(type(arg["windowSize"][1]) == "number", "windowPos[1] must be a number")
        assert(type(arg["windowSize"][2]) == "number", "windowPos[2] must be a number")
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
        type(arg["refreshRate"]) == "number" or type(arg["refreshRate"]) == "nil",
        "refreshRate must be a number or nil"
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

    assert(
        type(arg["sgctDebug"]) == "boolean" or type(arg["sgctDebug"]) == "nil",
        "sgctDebug must be a boolean or nil"
    )

    assert(
        type(arg["scene"]) == "table" or type(arg["scene"]) == "nil",
        "scene must be a table or nil"
    )
    if type(arg["scene"]) == "table" then
        local offset = arg["scene"]["offset"]
        assert(
            type(offset) == "table" or type(offset) == "nil",
            "scene['offset'] must be a table or nil"
        )
        if type(offset) == "table" then
            assert(type(offset["x"]) == "number", "scene['offset']['x'] must be a number")
            assert(type(offset["y"]) == "number", "scene['offset']['y'] must be a number")
            assert(type(offset["z"]) == "number", "scene['offset']['z'] must be a number")
        end

        local orientation = arg["scene"]["orientation"]
        assert(
            type(orientation) == "table" or type(orientation) == "nil",
            "scene['orientation] must be a table or nil"
        )
        if type(orientation) == "table" then
            assert(type(orientation["yaw"]) == "number", "orientation['yaw'] must be a number")
            assert(type(orientation["pitch"]) == "number", "orientation['pitch'] must be a number")
            assert(type(orientation["roll"]) == "number", "orientation['roll'] must be a number")
        end

        local scale = arg["scene"]["scale"]
        assert(
            type(scale) == "number" or type(scale) == "nil",
            "scene['scale'] must be a number or nil"
        )
    end

    assert(
        type(arg["capture"]) == "table" or type(arg["capture"]) == "nil",
        "capture must be a table or nil"
    )
    if type(arg["capture"]) == "table" then
        local c = arg["capture"]
        assert(
            type(c["path"]) == "string" or type(c["path"]) == "nil",
            "capture['path'] must be a string or nil"
        )
        assert(
            type(c["monoPath"]) == "string" or type(c["monoPath"]) == "nil",
            "capture['monoPath'] must be a string or nil"
        )
        assert(
            type(c["leftPath"]) == "string" or type(c["leftPath"]) == "nil",
            "capture['leftPath'] must be a string or nil"
        )
        assert(
            type(c["rightPath"]) == "string" or type(c["rightPath"]) == "nil",
            "capture['rightPath'] must be a string or nil"
        )
        assert(
            type(c["format"]) == "string" or type(c["format"]) == "nil",
            "capture['format'] must be a string or nil"
        )
    end

    assert(type(arg["viewport"]) == "string", "viewport must be a string")

    -- Then setting reasonable default values
    if arg["vsync"] == nil then
        arg["vsync"] = false
    end

    if arg["fullScreen"] == nil then
        arg["fullScreen"] = false
    end

    if arg["monitor"] == nil then
        arg["monitor"] = 0
    end
    
    if arg["tags"] == nil then
        arg["tags"] = {}
    end

    if arg["msaa"] == nil then
        arg["msaa"] = 4
    end

    if arg["border"] == nil then
        arg["border"] = true
    end

     if arg["shared"] then
        local t = arg["tags"]
        t[#t + 1] = "Spout"
    end

    if arg["name"] == nil then
        arg["name"] = "OpenSpace"
    end

    if arg["stereo"] == nil then
        arg["stereo"] = "none"
    end

    if arg["windowPos"] == nil then
        arg["windowPos"] = { 50, 50 }
    end

    if arg["eyeSep"] == nil then
        arg["eyeSep"] = 0.065
    end

    if arg["eyePos"] == nil then
        arg["eyePos"] = { 0.0, 0.0, 0.0 }
    end

    if arg["sgctDebug"] == nil then
        arg["sgctDebug"] = false
    end

    if not arg["windowSize"] then
        arg["windowSize"] = {1280, 720}
    end

    arg["scene"] = generateScene(arg)
    arg["settings"] = generateSettings(arg)
    arg["window"] = generateWindow(arg)
    arg["user"] = generateUser(arg)
    arg["capture"] = generateCapture(arg)

    return generateCluster(arg)
end


function normalizeArg(arg)
    arg = arg or {}

    if (type(arg["windowSize"]) == "table") then
        assert(
            type(arg[1]) == "nil" and type(arg[2]) == "nil",
            "Only windowSize or the first and second arguments can be set. Not both"
        )
    end
    assert(
        type(arg[1]) == "number" or type(arg[1]) == "nil",
        "First argument must be a number or nil"
    )
    assert(
        type(arg[2]) == "number" or type(arg[2]) == "nil",
        "Second argument must be a number or nil"
    )
    if (type(arg[1]) == "number") then
        if (type(arg[2]) == "nil") then
            arg[2] = arg[1] * 9/16
        end
        arg["windowSize"] = { arg[1], arg[2] }
        arg[1] = nil
        arg[2] = nil
    end

    return arg
end


function sgct.makeConfig(config)
    local configFile = os.tmpname()

    local file = io.open(configFile, "w+")

    file:write(config)

    io.close(file)
    return configFile
end



function sgct.config.single(arg)
    arg = normalizeArg(arg)

    assert(
        type(arg["windowSize"]) == "table" or type(arg["windowSize"]) == "nil",
        "windowSize must be a table or nil"
    )

    assert(
        type(arg["fov"]) == "table" or type(arg["fov"]) == "nil",
        "fov must be a table or nil"
    )
    if (type(arg["fov"]) == "table") then
        assert(type(arg["fov"]["left"]) == "number",  "fov['left'] must be a number")
        assert(type(arg["fov"]["right"]) == "number", "fov['right'] must be a number")
        assert(type(arg["fov"]["up"]) == "number",    "fov['up'] must be a number")
        assert(type(arg["fov"]["down"]) == "number",  "fov['down'] must be a number")
    else
        local defaultFov = 40
        local defaultRatio = 16/9 -- comes from default res 1280 x 720
        local tanDefaultFov = math.tan(math.pi * defaultFov / 180)

        if (type(arg["windowSize"]) == "table") then
            assert(type(arg["windowSize"][1]) == "number", "windowPos[1] must be a number")
            assert(type(arg["windowSize"][2]) == "number", "windowPos[2] must be a number")
            local tanHorizontalFov = tanDefaultFov
            local tanVerticalFov = tanDefaultFov

            local ratio = arg["windowSize"][1] / arg["windowSize"][2]

            -- ratio between w and h should be
            -- same as tan(horizontalFov) and tan(verticalFov)

            if (ratio > 1) then
                tanVerticalFov = tanHorizontalFov / ratio
            else
                tanHorizontalFov = tanVerticalFov * ratio
            end

            -- sgct expects fov in degrees
            arg["fov"] = {
                down = 180 * math.atan(tanVerticalFov) / math.pi,
                up = 180 * math.atan(tanVerticalFov) / math.pi,
                left = 180 * math.atan(tanHorizontalFov) / math.pi,
                right = 180 * math.atan(tanHorizontalFov) / math.pi
            }
        else
            -- sgct expects fov in degrees
            arg["fov"] = {
                down = 180 * math.atan(tanDefaultFov / defaultRatio) / math.pi,
                up = 180 * math.atan(tanDefaultFov / defaultRatio) / math.pi,
                left = 180 * math.atan(tanDefaultFov) / math.pi,
                right = 180 * math.atan(tanDefaultFov) / math.pi
            }
        end
    end

    assert(
        type(arg["tracked"]) == "boolean" or type(arg["tracked"]) == "nil",
        "tracked must be a boolean or nil"
    )
    trackedSpecifier = "tracked=\"true\""

    if (arg["tracked"] ~= nil and arg["tracked"] == false) then
        trackedSpecifier = "tracked=\"false\""
    end

    arg["viewport"] = generateSingleViewportFOV(
        arg["fov"]["down"],
        arg["fov"]["up"], 
        arg["fov"]["left"],
        arg["fov"]["right"],
        trackedSpecifier
    )
    return sgct.makeConfig(generateSingleWindowConfig(arg))
end



function sgct.config.fisheye(arg)
    arg = normalizeArg(arg)

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

    assert(
        type(arg["crop"]) == "table" or type(arg["crop"]) == "nil",
        "crop must be a table or nil"
    )
    if type(arg["crop"]) == "table" then
        assert(
            type(arg["crop"]["left"]) == "number", "crop['left'] must be a number"
        )
        assert(
            type(arg["crop"]["right"]) == "number", "crop['right'] must be a number"
        )
        assert(
            type(arg["crop"]["top"]) == "number", "crop['top'] must be a number"
        )
        assert(
            type(arg["crop"]["bottom"]) == "number", "crop['bottom'] must be a number"
        )
    end

    assert(
        type(arg["offset"]) == "table" or type(arg["offset"]) == "nil",
        "offset must be a table or nil"
    )
    if type(arg["offset"]) == "table" then
        assert(type(arg["offset"]["x"]) == "number", "offset['x'] must be a number")
        assert(type(arg["offset"]["y"]) == "number", "offset['y'] must be a number")
        assert(type(arg["offset"]["z"]) == "number", "offset['z'] must be a number")
    end

    if arg["fov"] == nil then
        arg["fov"] = 180
    end

    if arg["quality"] == nil then
        arg["quality"] = "1k"
    end

    if arg["tilt"] == nil then
        arg["tilt"] = 90
    end

    if arg["background"] == nil then
        arg["background"] = { r = 0.0, g = 0.0, b = 0.0, a = 1.0 }
    end

    if (arg["tracked"] ~= nil and arg["tracked"] == true) then
        trackedSpecifier = "tracked=\"true\""
    else
        trackedSpecifier = "tracked=\"false\""
    end

    arg["viewport"] = generateFisheyeViewport(
        arg["fov"],
        arg["quality"],
        arg["tilt"],
        arg["background"],
        arg["crop"],
        arg["offset"],
        trackedSpecifier
    )

    return sgct.makeConfig(generateSingleWindowConfig(arg))
end



function sgct.config.cube(arg)
    function getCubeWindow(location, res, size, trackedSpecifier)
        local pos
        local lowerLeft
        local upperLeft
        local upperRight
        if location == 'left' then
            pos = { 0, size[2] }
            lowerLeft =     { -1, -1,  1 }
            upperLeft =     { -1,  1,  1 }
            upperRight =    { -1,  1, -1 }
        elseif location == 'right' then
            pos = { 2 * size[1], size[2] }
            lowerLeft =     {  1, -1, -1 }
            upperLeft =     {  1,  1, -1 }
            upperRight =    {  1,  1,  1 }
        elseif location == 'up' then
            pos = { size[1], 0 }
            lowerLeft =     {  1,  1, -1 }
            upperLeft =     {  1,  1,  1 }
            upperRight =    { -1,  1,  1 }
        elseif location == 'down' then
            pos = { size[1], 2 * size[2] }
            lowerLeft =     { -1, -1,  1 }
            upperLeft =     { -1, -1, -1 }
            upperRight =    {  1, -1, -1 }
        elseif location == 'back' then
            pos = { 2 * size[1], 2 * size[2] }
            lowerLeft =     {  1, -1,  1 }
            upperLeft =     {  1,  1,  1 }
            upperRight =    { -1,  1,  1 }
        elseif location == 'front' then
            pos = { size[1], size[2] }
            lowerLeft =     { -1, -1, -1 }
            upperLeft =     { -1,  1, -1 }
            upperRight =    {  1,  1, -1 }
        end

        arg = {}
        arg["msaa"] = 8
        arg["border"] = false
        arg["name"] = "OpenSpace_" .. location
        arg["tags"] = { "Spout" }
        arg["windowSize"] = size
        arg["windowPos"] = pos
        arg["res"] = { res, res }
        arg["viewport"] = generateSingleViewport(lowerLeft, upperLeft, upperRight, trackedSpecifier)

        return generateWindow(arg)
    end

    function getControlWindow(down, up, left, right, trackedSpecifier)
        arg = {}
        arg["viewport"] = generateSingleViewportFOV(down, up, left, right, trackedSpecifier)
        return generateWindow(arg)
    end


    res = 1024
    size = { 640, 360 }
    
    arg["scene"] = generateScene(arg)
    arg["settings"] = generateSettings(arg)
    if (arg["tracked"] ~= nil and arg["tracked"] == true) then
        trackedSpecifier = "tracked=\"true\""
    else
        trackedSpecifier = "tracked=\"false\""
    end

    arg["window"] = getControlWindow(16.875, 16.875, 30.0, 30.0, trackedSpecifier) ..
        getCubeWindow('front', res, size, trackedSpecifier) ..
        getCubeWindow('back', res, size, trackedSpecifier) ..
        getCubeWindow('left', res, size, trackedSpecifier) ..
        getCubeWindow('right', res, size, trackedSpecifier) ..
        getCubeWindow('up', res, size, trackedSpecifier) ..
        getCubeWindow('down', res, size, trackedSpecifier)

    arg["user"] = generateUser(arg)
    arg["capture"] = generateCapture(arg)

    return sgct.makeConfig(generateCluster(arg))
end
