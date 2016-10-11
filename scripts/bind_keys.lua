--[[  OpenSpace keybinding script ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()
helper.setDeltaTimeKeys({
--  1			2		3		4		5		6		7		8		9		0
--------------------------------------------------------------------------------------------------------------------------
--  1s            2s 		5s		10s		30s		1m  		2m   		5m   		10m  		30 min
    1, 		2, 		5, 		10, 		30, 		60, 		120, 		300, 		600, 		1800,

--  1h    		2h    	3h     	6h     	12h    	1d     	2d      	4d      	1w      	2w
    3600, 		7200, 	10800, 	21600, 	43200, 	86400, 	172800, 	345600, 	604800, 	1209600,

--  1mo      	2mo		3mo      	6mo       	1yr       	2y        	5y         	10y		20y		50y
    2592000, 	5184000,	7776000, 	15552000, 	31536000, 	63072000, 	157680000, 	315360000, 	630720000,	1576800000
})
--  OBS: One month (1mo) is approximated by 30 days.


openspace.bindKey("q", helper.renderable.toggle('SunMarker'))
openspace.bindKey("e", helper.renderable.toggle('EarthMarker'))
openspace.bindKey("x", helper.renderable.toggle('Constellation Bounds'))

--openspace.bindKey("c", "openspace.parallel.setAddress('130.236.142.51');openspace.parallel.setPassword('newhorizons-20150714');openspace.parallel.connect();")

--openspace.bindKey("h", "openspace.iswa.setBaseUrl('https://iswa-demo-server.herokuapp.com/')");
openspace.bindKey("g", "openspace.iswa.setBaseUrl('http://128.183.168.116:3000/')");
openspace.bindKey("l", "openspace.iswa.setBaseUrl('http://localhost:3000/')");

openspace.bindKey("v", "openspace.time.setTime('2015-03-15T02:00:00.00')");

openspace.bindKeyLocal("h", "openspace.parallel.setAddress('127.0.0.1');openspace.parallel.setPort('25001');openspace.parallel.setPassword('test');openspace.parallel.connect();openspace.parallel.requestHostship('test');")
openspace.bindKeyLocal("c", "openspace.parallel.setAddress('127.0.0.1');openspace.parallel.setPort('25001');openspace.parallel.setPassword('test');openspace.parallel.connect();")
