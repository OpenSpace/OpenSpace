openspace.globebrowsing.documentation = {
  {
    Name = "setNodePosition",
    Arguments = { nodeIdentifer = "String", globeIdentifier = "String", latitude = "Number", longitude = "Number", altitude = "Number" },
    Documentation =
        "Sets the position of a SceneGraphNode that has GlobeTranslation/GlobeRotations. " ..
        "Usage: openspace.globebrowsing.setNodePosition(" ..
        "\"Scale_StatueOfLiberty\", \"Earth\", 40.000, -117.5, optionalAltitude)"
  },
  {
    Name = "setNodePositionFromCamera",
    Arguments = { nodeIdentifer = "String", useAltitude = "Number" },
    Documentation =
        "Sets the position of a SceneGraphNode that has GlobeTranslation/GlobeRotations" ..
        " to match the camera. Only uses camera position not rotation. If useAltitude" ..
        " is true, then the position will also be updated to the camera's altitude." ..
        "Usage: openspace.globebrowsing.setNodePositionFromCamera(" ..
        "\"Scale_StatueOfLiberty\", optionalUseAltitude)"
  },
  {
    Name = "getSunriseTime",
    Arguments = { time = "String"},
    Documentation =
        "Returns the next sunrise time after the supplied date " ..
        "Usage: openspace.globebrowsing.getSunriseTime(" ..
        "\"2023-04-16T12:36:28.773\")"
  },
  {
    Name = "getSunsetTime",
    Arguments = { time = "String"},
    Documentation =
        "Returns the next sunset time after the supplied date " ..
        "Usage: openspace.globebrowsing.getSunsetTime(" ..
        "\"2023-04-16T12:36:28.773\")"
  },
  {
    Name = "getNoonTime",
    Arguments = { time = "String"},
    Documentation =
        "Returns the next noon time after the supplied date " ..
        "Usage: openspace.globebrowsing.getNoonTime(" ..
        "\"2023-04-16T12:36:28.773\")"
  }

}

openspace.globebrowsing.setNodePosition = function (node_identifer, globe_identifier, lat, lon, altitude)
    openspace.setParent(node_identifer, globe_identifier)
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Globe', globe_identifier);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Longitude', lon);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Globe', globe_identifier);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Longitude', lon);
    if (altitude) then
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', altitude);
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', altitude);
    end
end

openspace.globebrowsing.setNodePositionFromCamera = function (node_identifer, use_altitude)
    local lat, lon, alt = openspace.globebrowsing.getGeoPositionForCamera();
    local camera = openspace.navigation.getNavigationState();
    openspace.setParent(node_identifer, camera.Anchor)
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Globe', camera.Anchor);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Longitude', lon);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Globe', camera.Anchor);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Longitude', lon);
    if (use_altitude) then
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', alt);
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', alt);
    end
end

openspace.globebrowsing.getSunriseTime = function (date)
  local lat, lon, alt = openspace.globebrowsing.getGeoPositionForCamera();
  local angle = noaa_sun_position(lat, lon, date)[1]
  local newTime = date;
  local counter = 0;
  while ( (angle < 128) or (angle > 129) ) do
    newTime = openspace.time.advancedTime(newTime, "1m")
    angle = noaa_sun_position(lat, lon, newTime)[1]
    counter = counter + 1
    if (counter == 1441) then break end
  end
  if (counter > 1440) then
    openspace.printWarning('No sunrise time found')
  end
    return newTime;
end

openspace.globebrowsing.getSunsetTime = function (date)
  local lat, lon, alt = openspace.globebrowsing.getGeoPositionForCamera();
  local angle = noaa_sun_position(lat, lon, date)[1]
  local newTime = date;
  local counter = 0;
  while ( (angle < 353) or (angle > 354) ) do
    newTime = openspace.time.advancedTime(newTime, "1m")
    angle = noaa_sun_position(lat, lon, newTime)[1]
    counter = counter + 1
    if (counter == 1441) then break end
  end
  if (counter > 1440) then
    openspace.printWarning('No sunset time found')
  end
  return newTime;
end

openspace.globebrowsing.getNoonTime = function (date)
  local lat, lon, alt = openspace.globebrowsing.getGeoPositionForCamera();
  local angle = noaa_sun_position(lat, lon, date)[1]
  local newTime = date;
  local counter = 0;
  while ( (angle < 257) or (angle > 258) ) do
    newTime = openspace.time.advancedTime(newTime, "1m")
    angle = noaa_sun_position(lat, lon, newTime)[1]
    counter = counter + 1
    if (counter == 1441) then break end
  end
  if (counter > 1440) then
    openspace.printWarning('No noon time found')
  end
  return newTime;
end

function noaa_sun_position(lat, long, datetime, tzoffset)
  -- https://gml.noaa.gov/grad/solcalc/solareqns.PDF
  -- modified from https://stackoverflow.com/a/75785223
  local pattern = "(%d+)-(%d+)-(%d+)T(%d+):(%d+):(%d+)"
  local timeToConvert = datetime
  local y, m, d, h, min, s = timeToConvert:match(pattern)
  local convertedTimestamp = os.time({year = y, month = m, day = d, hour = h, min = min, sec = s})

  datetime = os.date("!*t", convertedTimestamp)
  tzoffset = tzoffset or -5
  local datetime= os.date("*t",os.time(datetime) - 5 * 60 * 60)
  local day_of_year = os.date("%j", os.time(datetime))
  local fract_year = (2 * math.pi / 365) * ( day_of_year - 1 + (os.date("*t").hour - 5 - 12) / 24)

  local eqtime = 229.18*
                      (0.000075+
                      0.001868*math.cos(fract_year)
                  - 0.032077 * math.sin(fract_year)
                  - 0.014615 * math.cos(2*fract_year)
                  - 0.040849 * math.sin(2*fract_year))
  local decl = 0.006918
          - 0.399912*math.cos(fract_year )
          + 0.070257*math.sin(fract_year )
          - 0.006758*math.cos(2*fract_year )
          + 0.000907*math.sin(2*fract_year )
          -0.002697*math.cos(3*fract_year )
          + 0.00148*math.sin (3*fract_year )
  local time_offset = eqtime + 4 * long - 60 * -5
  local tst = datetime.hour*60 + datetime.min  + datetime.sec/60 + time_offset
  local ha = (tst / 4) -180
  local cos_zenith =  math.sin(math.rad(lat))*math.sin(decl) + math.cos(math.rad(lat))* math.cos(decl) *math.cos(math.rad(ha))
  local azimuth = math.atan(-math.cos(math.rad(decl)) * math.sin(math.rad(ha)), math.sin(math.rad(decl)) * math.cos(math.rad(lat)) - math.cos(math.rad(decl)) * math.sin(math.rad(lat)) * math.cos(math.rad(ha)))
  return {((math.deg(azimuth) > 0 and math.deg(azimuth)) or ( math.deg(azimuth) + 360 )) , 90-math.deg(math.acos(cos_zenith)) , ha, decl}
end


