--[[  OpenSpace keybinding script ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()
helper.setDeltaTimeKeys({
    1, 5, 10, 20, 40, 60, 120, 360, 720, 1440,
    2880, 5760, 11520, 23040, 46080, 92160, 184320, 368640, 737280, 1474560,
    2949120, 5898240, 11796480, 23592960, 47185920, 94371840, 188743680, 377487360
})

openspace.bindKey("q", helper.renderable.toggle('SunMarker'))
openspace.bindKey("e", helper.renderable.toggle('EarthMarker'))
openspace.bindKey("x", helper.renderable.toggle('Constellation Bounds'))

--openspace.bindKey("c", "openspace.parallel.setAddress('130.236.142.51');openspace.parallel.setPassword('newhorizons-20150714');openspace.parallel.connect();")

--openspace.bindKey("h", "openspace.iswa.setBaseUrl('https://iswa-demo-server.herokuapp.com/')");
openspace.bindKey("g", "openspace.iswa.setBaseUrl('http://128.183.168.116:3000/')");
openspace.bindKey("l", "openspace.iswa.setBaseUrl('http://localhost:3000/')");

openspace.bindKey("v", "openspace.time.setTime('2015-03-15T02:00:00.00')");

openspace.bindKeyLocal("h", "openspace.parallel.setAddress('127.0.0.2');openspace.parallel.setPort('25001');openspace.parallel.setPassword('test');openspace.parallel.connect();openspace.parallel.requestHostship('test');")
openspace.bindKeyLocal("c", "openspace.parallel.setAddress('127.0.0.3');openspace.parallel.setPort('25001');openspace.parallel.setPassword('test');openspace.parallel.connect();")
openspace.bindKeyLocal("d", "openspace.parallel.setAddress('127.0.0.4');openspace.parallel.setPort('25001');openspace.parallel.setPassword('test');openspace.parallel.connect();")