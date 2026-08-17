registerFunction(
  {
    Name = "addSmallBodyObject",
    Arguments = {
      { "object_search_string", "String" }
    },
    Documentation = [[
      Creates a new scene graph node from the small body object by querying the JPL Small
      Body Database for the provided object. If the object exists, its Keplerian elements
      are retrieved and an orbital path for that object is created. If the search returns
      no result, an error is logged
    ]],
    Function = function(object_search_string)
      local Input = "https://ssd-api.jpl.nasa.gov/sbdb.api?sstr=" .. object_search_string .. "&full-prec=true"
      local Output = openspace.absPath("${TEMPORARY}/module-space-sbdb.json")
      local WaitForCompletion = true
      local OverrideExistingFile = true
      local success =
        openspace.downloadFile(Input, Output, WaitForCompletion, OverrideExistingFile)

      if not success then
        error("Could not find small body object '" .. object_search_string .. "'")
      end

      local json = openspace.loadJson(Output)

      local object = json.object
      openspace.printInfo("JSON")
      openspace.printInfo(json)
      openspace.printInfo("object")
      openspace.printInfo(object)
      if object == nil then
        error("Malformed small body object '" .. object_search_string .. "'")
      end

      if not json.orbit then
        error("Malformed small body orbit '" .. object_search_string .. "'")
      end

      local elements = json.orbit.elements
      if elements == nil then
        error("Malformed small body element '" .. object_search_string .. "'")
      end

      local name = object.fullname or object.shortname or object_search_string
      local shortname = object.shortname or object_search_string

      local res = {}

      for k, v in pairs(json.orbit.elements) do
        res[v.name] = v.value
      end

      assert(res.e)
      local eccentricity = res.e
      assert(res.a)
      local semi_major_axis = res.a
      assert(res.i)
      local inclination = res.i
      assert(res.om)
      local longitude_ascending_node = res.om
      assert(res.w)
      local argument_perihelion = res.w
      assert(res.ma)
      local mean_anomaly = res.ma
      assert(res.per)
      local sidereal_orbital_period = res.per

      local AU = 1.496e+8

      local Translation = {
        Type = "KeplerTranslation",
        Eccentricity = tonumber(eccentricity),
        SemiMajorAxis = tonumber(semi_major_axis) * AU,
        Inclination = tonumber(inclination),
        AscendingNode = tonumber(longitude_ascending_node),
        ArgumentOfPeriapsis = tonumber(argument_perihelion),
        MeanAnomaly = tonumber(mean_anomaly),
        Epoch = json.orbit.epoch .. "JD",
        Period = tonumber(sidereal_orbital_period) * openspace.time.secondsPerDay()
      }

      local Trail = {
        Identifier = openspace.makeIdentifier(shortname) .. "Trail",
        Parent = "SunEclipJ2000",
        Renderable = {
          Type = "RenderableTrailOrbit",
          Translation = Translation,
          Color = { 0.2, 0.8, 0.45 },
          Period = tonumber(sidereal_orbital_period),
          Resolution = 1000,
          Fade = 1.25
        },
        GUI = {
          Name = name .. " Trail",
          Path = "/Solar System/SBDB",
          Focusable = false,
          Description = "More information at https://ssd.jpl.nasa.gov/tools/sbdb_lookup.html#/?sstr=" .. object_search_string
        }
      }

      local Position = {
        Identifier = openspace.makeIdentifier(shortname) .. "Position",
        Parent = "SunEclipJ2000",
        Transform = {
          Translation = Translation
        },
        GUI = {
          Name = name .. " Position",
          Path = "/Solar System/SBDB",
          Description = "More information at https://ssd.jpl.nasa.gov/tools/sbdb_lookup.html#/?sstr=" .. object_search_string
        }
      }

      openspace.addSceneGraphNode(Trail)
      openspace.addSceneGraphNode(Position)
    end
  }
)
