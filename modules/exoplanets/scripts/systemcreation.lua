openspace.exoplanets.documentation = {
  {
    Name = "addExoplanetSystem",
    Arguments = {
      { "starName", "String" }
    },
    Documentation = [[
      Add a specific exoplanet system to the scene, based on a star name.

      Note that the formatting of the name must match the one in the dataset. That is, it
      must match the name as given in the [NASA Exoplanet Archive](https://exoplanetarchive.ipac.caltech.edu/).

      \\param starName The name of the star
    ]]
  },
  {
    Name = "addExoplanetSystems",
    Arguments = {
      { "listOfStarNames", "String[]" }
    },
    Documentation = [[
      Add multiple exoplanet systems to the scene, based on a list of names.

      Note that the formatting of the name must match the one in the dataset. That is,
      they must match the names as given in the [NASA Exoplanet Archive](https://exoplanetarchive.ipac.caltech.edu/).

      \\param listOfStarNames A list of star names for which to create the exoplanet systems
    ]]
  },
  {
    Name = "loadExoplanetsFromCsv",
    Arguments = {
      { "csvFile", "String" }
    },
    Documentation = [[
      Load a set of exoplanets based on custom data, in the form of a CSV file, and add
      them to the rendering. Can be used to load custom datasets, or more recent planets
      than what are included in the internal data file that is released with OpenSpace.

      The format and column names in the CSV should be the same as the ones provided by
      the [NASA Exoplanet Archive](https://exoplanetarchive.ipac.caltech.edu/).

      We recommend downloading the file from the [Exoplanet Archive's Composite data table](https://exoplanetarchive.ipac.caltech.edu/cgi-bin/TblView/nph-tblView?app=ExoTbls&config=PSCompPars),
      where multiple sources are combined into one row per planet.

      Please remember to include all columns in the file download, as missing data columns
      may lead to an incomplete visualization.

      Also, avoid loading too large files of planets, as each added system will affect the
      rendering performance.

      \\param csvFile A path to a .csv file that contains the data for the exoplanets
    ]]
  }
}

openspace.exoplanets.addExoplanetSystem = function (starName)
  local data = openspace.exoplanets.systemData(starName)
  addExoplanetSystem(data)
end

openspace.exoplanets.addExoplanetSystems = function (listOfStarNames)
  for _,starName in pairs(listOflistOfStarNamesNames) do
    local data = openspace.exoplanets.systemData(starName)
    addExoplanetSystem(data)
  end
end

openspace.exoplanets.loadExoplanetsFromCsv = function (csvFile)
  local dataList = openspace.exoplanets.loadSystemDataFromCsv(csvFile)
  for _,data in pairs(dataList) do
    addExoplanetSystem(data)
  end
end

-----------------------------------------------------------------------------------
-- Some Settings and things that will be used for the creation
-----------------------------------------------------------------------------------

local ExoplanetsGuiPath = "/Milky Way/Exoplanets/Exoplanet Systems/";

-- @TODO (2024-10-11, emmbr) We should add a way of getting constants like this from
-- OpenSpace instead (for example though the Lua API) so we don't have to redefine them
-- everywhere we need a value for e.g. the Earth radius
local SolarRadius = 6.95700E8
local EarthRadius = 6371E3
local JupiterRadius = 7.1492E7
local AstronomicalUnit = 149597871E3

-- Constants for different categories of sizes of planets (in Earth radii)
-- Source: https://www.nasa.gov/image-article/sizes-of-known-exoplanets/
local MaxRadius = {
  Terrestrial = 1.25,
  SuperEarth = 2.0,
  NeptuneLike = 6.0
}

-- Planets will be colored based on their size (from thresholds above), and
-- described based on the planet types
local PlanetType = {
  Terrestrial = {
    Description = string.format(
      "Terrestrial planets (R < %.2f Earth radii)",
      MaxRadius.Terrestrial
    ),
    Color = { 0.32, 0.2, 0.1 },
    ColorName = "Brown"
  },
  SuperEarth = {
    Description = string.format(
      "Super-Earths (%.0f < R < %.0f Earth radii)",
      MaxRadius.Terrestrial, MaxRadius.SuperEarth
    ),
    Color = { 1.0, 0.76, 0.65 },
    ColorName = "Beige"
  },
  NeptuneLike = {
    Description = string.format(
      "Neptune-like planets (%.0f < R < %.0f Earth radii)",
      MaxRadius.SuperEarth, MaxRadius.NeptuneLike
    ),
    Color = { 0.22, 0.49, 0.50 },
    ColorName = "Blue"
  },
  GasGiant = {
    Description = string.format(
      "Gas giants or larger planets (R > %.0f Earth radii)",
      MaxRadius.NeptuneLike
    ),
    Color = { 0.55, 0.34, 0.39 },
    ColorName = "Wine red"
  }
}

local SizeColorLayerDescription = string.format(
  [[This layer gives a fixed color to the planet surface based on the
    planet radius. The planets are split into four categories based on
    their radius (in Earth radii). 1) %s are %s, 2) %s are %s, 3)
    %s are %s, and 4) %s are %s.]],
  PlanetType.Terrestrial.Description, PlanetType.Terrestrial.ColorName,
  PlanetType.SuperEarth.Description, PlanetType.SuperEarth.ColorName,
  PlanetType.NeptuneLike.Description, PlanetType.NeptuneLike.ColorName,
  PlanetType.GasGiant.Description, PlanetType.GasGiant.ColorName
);

function planetTypeKey(radiusInMeter)
  if radiusInMeter < MaxRadius.Terrestrial * EarthRadius then
    return "Terrestrial"
  elseif radiusInMeter < MaxRadius.SuperEarth * EarthRadius then
    return "SuperEarth"
  elseif radiusInMeter < MaxRadius.NeptuneLike * EarthRadius then
    return "NeptuneLike"
  else
    return "GasGiant"
  end
end

function hasValue(v)
  return v ~= nil
end

-----------------------------------------------------------------------------------
-- This is the function that adds the scene graph nodes for each exoplanet system.
-- Edit this to change the visuals of the created exoplanet systems
-----------------------------------------------------------------------------------
function addExoplanetSystem(data)
  if openspace.isEmpty(data) then
    -- No data was found
    return
  end

  local starIdentifier = data.SystemId
  local guiPath = ExoplanetsGuiPath .. data.StarName

  if openspace.hasSceneGraphNode(starIdentifier) then
    openspace.printError(
      "Adding of exoplanet system '" .. data.StarName .. "' failed. " ..
      "The system has already been added"
    )
    return
  end

  --------------------------------------------------------------------
  -- Star Globe
  --------------------------------------------------------------------
  local starTexture = openspace.propertyValue("Modules.Exoplanets.StarTexture")
  local starNoDataTexture = openspace.propertyValue("Modules.Exoplanets.NoDataTexture")

  local colorLayers = {}
  if hasValue(data.StarColor) and hasValue(data.StarRadius) then
    -- If a star color was computed, there was enough data to visualize the star
    colorLayers = {
      {
        Identifier = "StarColor",
        Type = "SolidColor",
        Color = data.StarColor,
        BlendMode = "Normal",
        Enabled = true
      },
      {
        Identifier = "StarTexture",
        FilePath = openspace.absPath(starTexture),
        BlendMode = "Color",
        Enabled = true
      }
    }
  else
    colorLayers = {
      {
        Identifier = "NoDataStarTexture",
        FilePath = openspace.absPath(starNoDataTexture),
        BlendMode = "Color",
        Enabled = true
      }
    }
  end

  local starSizeAndTempInfo = function (data)
    if hasValue(data.StarRadius) and hasValue(data.StarTeff) then
      return string.format(
        "It has a size of %.2f solar radii and an effective temperature of %.0f Kelvin",
        data.StarRadius / SolarRadius, data.StarTeff
      )
    elseif hasValue(data.StarTeff) then
      return string.format(
        "Its size is uknown, but it has an effective temperature of %.0f Kelvin",
        data.StarTeff
      )
    elseif hasValue(data.StarRadius) then
      return string.format(
        "It has a size of %.2f solar radii, but its temperature is unknown",
        data.StarRadius
      )
    else
      return "Both its size and temperature is unknown"
    end
  end

  local Star = {
    Identifier = starIdentifier,
    Parent = "SolarSystemBarycenter",
    Transform = {
      Rotation = {
        Type = "StaticRotation",
        Rotation = data.SystemRotation
      },
      Translation = {
        Type = "StaticTranslation",
        Position = data.Position
      }
    },
    Renderable = {
      Type = "RenderableGlobe",
      -- If there is not a value for the radius, render a globe with a default radius,
      -- to allow us to navigate to something. Note that it can't be too small, due to
      -- precision issues at this distance
      Radii = openspace.ternary(
        hasValue(data.StarRadius),
        data.StarRadius,
        0.1 * SolarRadius
      ),
      PerformShading = false,
      Layers = {
        ColorLayers = colorLayers
      }
    },
    Tag = { "exoplanet_system" },
    GUI = {
      Name = data.StarName .. " (Star)",
      Path = guiPath,
      Description = string.format(
        [[The star %s is the host star of an exoplanet system with %d known %s that %s
          enough data to be visualized. %s. The system is located at a distance of %.0f
          light-years from Earth.]],
        data.StarName,
        data.NumPlanets,
        openspace.ternary(data.NumPlanets > 1, "planets", "planet"),
        openspace.ternary(data.NumPlanets > 1, "have", "has"),
        starSizeAndTempInfo(data),
        data.Distance
      )
    }
  }
  openspace.addSceneGraphNode(Star)

  --------------------------------------------------------------------
  -- Star Label
  --------------------------------------------------------------------
  local StarLabel = {
    Identifier = starIdentifier .. "_Label",
    Parent = starIdentifier,
    Renderable = {
      Type = "RenderableLabel",
      Enabled = false,
      Text = data.StarName,
      FontSize = 70.0,
      Size = 14.17,
      MinMaxSize = { 1, 50 },
      EnableFading = true,
      FadeUnit = "pc",
      FadeDistances = { 1.33, 15.0 },
      FadeWidths = { 1.0, 20.0 }
    },
    Tag = { "exoplanet_system_labels" },
    GUI = {
      Name = data.StarName .. " Label",
      Path = guiPath,
      Description = string.format(
        "A label for the exoplanet host star %s.", data.StarName
      )
    }
  }
  openspace.addSceneGraphNode(StarLabel)

  --------------------------------------------------------------------
  -- Star Glare
  --------------------------------------------------------------------
  local starGlareTexture = openspace.propertyValue("Modules.Exoplanets.StarGlareTexture")

  if hasValue(data.StarColor) and hasValue(data.StarRadius) then
    -- This is a little magic to make the size of the glare dependent on the
    -- size and the temperature of the star. It's kind of based on the fact that
    -- the luminosity of a star is proportional to: (radius^2)*(temperature^4)
    -- Maybe a better option would be to compute the size based on the absolute
    -- magnitude or star luminosity, but for now this looks good enough.
    local size = 59.0 * data.StarRadius
    if hasValue(data.StarTeff) then
      local SunTeff = 5780.90;
      local RelativeTeff = (data.StarTeff / SunTeff)
      size = size * RelativeTeff * RelativeTeff;
    end

    local StarGlare = {
      Identifier = starIdentifier .. "_Glare",
      Parent = starIdentifier,
      Renderable = {
        Type = "RenderablePlaneImageLocal",
        Size = size,
        Origin = "Center",
        Billboard = true,
        Texture = openspace.absPath(starGlareTexture),
        BlendMode = "Additive",
        Opacity = 0.65,
        MultiplyColor = data.StarColor
      },
      GUI = {
        Name = data.StarName .. " Glare",
        Path = guiPath,
        Description = string.format(
          "A glare effect for the star %s. %s",
          data.StarName,
          openspace.ternary(
            hasValue(data.StarTeff),
            [[The size of the glare has been computed based on data of the star's
              temperature, in a way that's relative to the visualization for our Sun.]],
            "The size of the glare is not data-based."
          )
        )
      }
    }
    openspace.addSceneGraphNode(StarGlare)
  end

  --------------------------------------------------------------------
  -- 1 AU Comparison Circle
  --------------------------------------------------------------------
  local showCircle = openspace.propertyValue("Modules.Exoplanets.ShowComparisonCircle")
  local circleColor = openspace.propertyValue("Modules.Exoplanets.ComparisonCircleColor")

  local Circle = {
    Identifier = starIdentifier .. "_1AU_Circle",
    Parent = starIdentifier,
    Renderable = {
      Type = "RenderableRadialGrid",
      Enabled = showCircle,
      Radii = { 0.0, 1.0 },
      GridSegments = { 1, 1 }, -- 1 segment in each direction, makes the grid a ring
      Color = circleColor,
      CircleSegments = 64,
      LineWidth = 2.0
    },
    Transform = {
      Rotation = {
        Type = "StaticRotation",
        Rotation = data.MeanOrbitRotation
      },
      Scale = {
        Type = "StaticScale",
        Scale = AstronomicalUnit
      }
    },
    Tag = { "exoplanet_1au_ring" }, -- Used in GUI
    GUI = {
      Name = "1 AU Size Comparison Circle",
      Path = guiPath,
      Description = [[A circle with a radius of 1 Astronomical Unit. That is, its
        size corresponds to the size of Earth's orbit.]]
    }
  }
  openspace.addSceneGraphNode(Circle)

  --------------------------------------------------------------------
  -- Habitable Zone
  --------------------------------------------------------------------
  local showZone = openspace.propertyValue("Modules.Exoplanets.ShowHabitableZone")
  local zoneOpacity = openspace.propertyValue("Modules.Exoplanets.HabitableZoneOpacity")
  local zoneTexture = openspace.propertyValue("Modules.Exoplanets.HabitableZoneTexture")
  local useOptimisticBounds = openspace.propertyValue(
    "Modules.Exoplanets.UseOptimisticZone"
  )

  if hasValue(data.StarTeff) and hasValue(data.StarLuminosity) then
    local HabitableZoneDisc = {
      Identifier = starIdentifier .. "_HZ_Disc",
      Parent = starIdentifier,
      Renderable = {
        Type = "RenderableHabitableZone",
        Enabled = showZone,
        Texture = openspace.absPath(zoneTexture),
        Luminosity = data.StarLuminosity,
        EffectiveTemperature = data.StarTeff,
        Optimistic = useOptimisticBounds,
        Opacity = zoneOpacity
      },
      Transform = {
        Rotation = {
          Type = "StaticRotation",
          Rotation = data.MeanOrbitRotation
        }
      },
      Tag = { "exoplanet_habitable_zone" }, -- Used in GUI
      GUI = {
        Name = data.StarName .. " Habitable Zone",
        Path = guiPath,
        Description = [[
          The habitable zone is the region around a star in which an Earth-like planet
          can potentially have liquid water on its surface. The inner boundary is where
          the greenhouse gases in the atmosphere would trap any incoming infrared
          radiation, leading to the planet surface becoming so hot that water boils away.
          The outer boundary is where the greenhouse effect would not be able to maintain
          surface temperature above freezing anywhere on the planet.
        ]]
      }
    }
    openspace.addSceneGraphNode(HabitableZoneDisc)
  end

  --------------------------------------------------------------------
  -- Planets
  --------------------------------------------------------------------
  local defaultPlanetTexture = openspace.propertyValue(
    "Modules.Exoplanets.PlanetDefaultTexture"
  )

  local addExoplanet = function(id, planetData)
    -- This translation will be used for both the trail and globe
    local PlanetKeplerTranslation = {
      Type = "KeplerTranslation",
      Eccentricity = planetData.Eccentricity,
      SemiMajorAxis = 0.001 * planetData.SemiMajorAxis, -- km
      Inclination = planetData.Inclination,
      AscendingNode = planetData.AscendingNode,
      ArgumentOfPeriapsis = planetData.ArgumentOfPeriapsis,
      MeanAnomaly = 0.0,
      Epoch = planetData.Epoch,
      Period = planetData.Period * openspace.time.secondsPerDay()
    }

    --------------------------------------------------------------------
    -- Planet Globe (if we know how big it is)
    --------------------------------------------------------------------
    local ambientIntensity = 0.5 -- High to show the color from size more clearly

    local planetSizeInfo = function (data)
      if hasValue(data.Radius) then
        return string.format(
          "%.2f Earth radii, %.2f Jupiter radii",
          planetData.Radius / EarthRadius,
          planetData.Radius / JupiterRadius
        )
      else
        return "unknown"
      end
    end

    if hasValue(planetData.Radius) then
      local planetTypeKey = planetTypeKey(planetData.Radius)
      local planetTypeData = PlanetType[planetTypeKey]

      local planetColorLayers = {
        {
          Identifier = "ColorFromSize",
          Name = "Color From Size Classification",
          Type = "SolidColor",
          Color = planetTypeData.Color,
          Enabled = true,
          Description = SizeColorLayerDescription
        }
      }

      -- If a default texture was provided, use it. Also, reduce the ambient intensity
      if not openspace.isEmpty(defaultPlanetTexture) then
        local PlanetTextureLayer = {
          Identifier = "PlanetTexture",
          Name = "Planet Texture",
          FilePath = openspace.absPath(defaultPlanetTexture),
          Enabled = true
        }
        table.insert(planetColorLayers, PlanetTextureLayer)
        ambientIntensity = 0.15
      end

      local Planet = {
        Identifier = id,
        Parent = starIdentifier,
        Transform = {
          Translation = PlanetKeplerTranslation
        },
        Renderable = {
          Type = "RenderableGlobe",
          Radii = planetData.Radius,
          PerformShading = true,
          Layers = {
            ColorLayers = planetColorLayers
          },
          LightSourceNode = starIdentifier,
          AmbientIntensity = ambientIntensity
        },
        Tag = { "exoplanet_planet" },
        GUI = {
          Name = planetData.Name,
          Path = guiPath,
          Description = string.format(
            [[The exoplanet %s falls into the category of %s. Some key facts:
              Radius: %s.
              Orbit Period: %.1f (Earth) days.
              Orbit Semi-major axis: %.2f (AU).
              Orbit Eccentricity: %.2f. %s]],
            planetData.Name,
            planetTypeData.Description,
            planetSizeInfo(planetData),
            planetData.Period,
            planetData.SemiMajorAxis / AstronomicalUnit,
            planetData.Eccentricity,
            openspace.ternary(
              planetData.HasUsedDefaultValues,
              [[OBS! Default values have been used to visualize the orbit (for example for
                inclination, eccentricity, or argument of periastron), and hence the data
                specified for the orbit might not be reliable.]],
              ""
            )
          )
        }
      }
      openspace.addSceneGraphNode(Planet)
    end

    --------------------------------------------------------------------
    -- Planet Orbit
    --------------------------------------------------------------------
    local trailResolution = 1000.0
    -- Increase the resolution of very eccentric orbits
    local EccentricityThreshold = 0.85
    if planetData.Eccentricity > EccentricityThreshold then
      trailResolution = trailResolution * 2.0
    end

    local orbitDescription = string.format(
      "The orbit trail of the exoplanet %s.", planetData.Name
    )
    if planetData.HasUsedDefaultValues then
      orbitDescription = orbitDescription .. " " .. [[
        OBS! Default values have been used to visualize the orbit (for example for
        inclination, eccentricity, or argument of periastron). The shape or orientation
        of the orbit might hence not be completely accurate.
      ]]
    end

    local Orbit = {
      Identifier = id .. "_Trail",
      Parent = starIdentifier,
      Renderable = {
        Type = "RenderableTrailOrbit",
        Period = planetData.Period,
        Resolution = trailResolution,
        Translation = PlanetKeplerTranslation,
        Color = { 1.0, 1.0, 1.0 }
      },
      Tag = { "exoplanet_trail" },
      GUI = {
        Name = planetData.Name .. " Trail",
        Path = guiPath,
        Description = orbitDescription
      }
    }
    openspace.addSceneGraphNode(Orbit)

    --------------------------------------------------------------------
    -- Planet Orbit Uncertainty
    --------------------------------------------------------------------
    local showUncertaintyDisc = openspace.propertyValue("Modules.Exoplanets.ShowOrbitUncertainty")
    local discTexture = openspace.propertyValue("Modules.Exoplanets.OrbitDiscTexture")

    if hasValue(planetData.SemiMajorAxisUncertainty) then
      local OrbitDisc = {
        Identifier = id .. "_Disc",
        Parent = starIdentifier,
        Transform = {
          Rotation = {
            Type = "StaticRotation",
            Rotation = planetData.OrbitPlaneRotationMatrix
          }
        },
        Renderable = {
          Type = "RenderableOrbitDisc",
          Enabled = showUncertaintyDisc,
          Texture = openspace.absPath(discTexture),
          Size = planetData.SemiMajorAxis,
          Eccentricity = planetData.Eccentricity,
          Offset = planetData.SemiMajorAxisUncertainty,
          Opacity = 0.25
        },
        Tag = { "exoplanet_uncertainty_disc" }, -- Used in GUI
        GUI = {
          Name = planetData.Name .. " Disc",
          Path = guiPath,
          Description = [[
            The width of this disc around the planet's orbit marks the uncertainty of the
            orbit (based on the uncertainty of the semi-major axis, and the eccentricity
            of the orbit). The wider the disc, the more uncertain the orbit is.
          ]]
        }
      }
      openspace.addSceneGraphNode(OrbitDisc)
    end
  end

  -- Add all the planets that have sufficient data
  for planetId,planetData in pairs(data.Planets) do
    addExoplanet(planetId, planetData)
  end
end
