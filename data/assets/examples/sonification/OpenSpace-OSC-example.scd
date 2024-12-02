NetAddr.langPort;
NetAddr.localAddr;


(

// Camera sonification
OSCdef.new(
	\Camera,
	{
		arg msg;

		// msg[0] = OSC Label
		// msg[1] = The current X position of the camera in the world,
		//          specified in relation to the Solar System barycenter
		// msg[2] = The Y position of the camera in the world
		// msg[3] = The Z position of the camera in the world
		// msg[4] = The angle in radians for the current quaternion rotation of the camera (w)
		// msg[5] = The X component of the unit rotation axis in the current quaternion rotation
		//          of the camera
		// msg[6] = The Y component of the rotation axis in the quaternion rotation
		// msg[7] = The Z component of the rotation axis in the quaternion rotation
		// msg[8] = The current speed of the camera,
		//          specified in the selected distance unit per second
		// msg[9] = The selected distance unit for the speed of the camera, as a string with the
		//          first letter capitalised

		("Camera: " + msg).postln;
	},
	'/Camera'
);

// Focus sonification
OSCdef.new(
	\Focus,
	{
		arg msg;

		// msg[0] = OSC Label
		// msg[1] = The name of the currently set focus node in OpenSpace as a string.
		//          For example, Earth or ISS.

		("Focus: " + msg).postln;
	},
	'/Focus'
);

// Surround mode sonification
OSCdef.new(
	\Mode,
	{
		arg msg;

		// msg[0] = OSC Label
		// msg[1] = Angle Calculation Mode. Determines what type of angle the other
		//          Sonifications are sending. More documentaiton on how these angles work
		//          can be found on the general OpenSpace documentation page
		//          https://docs.openspaceproject.com/en/latest/
		//          0 -> Horizontal, the angle is horizontal to the screen and any additional
		//                           elevatoin angle is up/down the screen.
		//          1 -> Circular, the angle is a circular azimuth angle. Similar to
		//                         polar coordinates. In the case of an added elevation
		//                         angle, this would be the radius angle.
		// msg[2] = Whether the elevation angles have been calculated or not.
		//          In case of false, the elevation angles will always be set to 0.0.

		("Mode: " + msg).postln;
	},
	'/Mode'
);

// Time sonification
OSCdef.new(
	\Time,
	{
		arg msg;

		// msg[0] = OSC Label
		// msg[1] = Speed of simulation time,
		//          specified in the selected time unit in the simulation per real-life second
		// msg[2] = The selected time unit for the speed of simulation time,
		//          as a string with the first letter capitalized
		// msg[3] = Current simulation time specified in J2000 seconds.
		//          J2000 seconds are the number of seconds past the J2000 epoch
		//          (January 1, 2000 12:00:00 TT)

		("Time: " + msg).postln;
	},
	'/Time'
);

// Nodes sonificaitons, general map:
// msg[0] = OSC Label (The identifier for each node)
// msg[1] = The distance from the camera to the node,
//          specified in the selected distance unit
// msg[2] = The horizontal angle in radians to the node on the master screen,
//          with the current surround mode taken into account
// msg[3] = The vertical/elevation angle in radians to the node on the master screen,
//          with the current angle calculation mode taken into account
// msg[4] = The selected distance unit for the distance to the camera, as a string with the
//          first letter capitalised

// ISS node
OSCdef.new(
	\ISS,
	{
		arg msg;

		// Follow the general nodes sonificaiton data map above

		("ISS: " + msg).postln;
	},
	'/ISS'
);

// Tiangong node
OSCdef.new(
	\Tiangong,
	{
		arg msg;

		// Follow the general nodes sonificaiton data map above

		("Tiangong: " + msg).postln;
	},
	'/Tiangong'
);


// Planet related sonifications:
// Compare planets
OSCdef.new(
	\Compare,
	{
		arg msg;

		// msg[0] = OSC Label
		// msg[1] = The first planet to be compared
		//          0 -> None selected
		//          1 -> Mercury
		//          2 -> Venus
		//          3 -> Earth
		//          4 -> Mars
		//          5 -> Jupiter
		//          6 -> Saturn
		//          7 -> Uranus
		//          8 -> Neptune
		// msg[2] = The second planet to be compared, see list above (will never be the same
		//          as the first)
		// msg[3] = List of GUI settings for the comparison, which aspects of the sonification
		//          should be turned on or off
		//     msg[3][0] = Size/day, 0: off, 1: on
		//     msg[3][1] = Gravity
		//     msg[3][2] = Temperature
		//     msg[3][3] = Atmosphere
		//     msg[3][4] = Moons
		//     msg[3][5] = Rings

		("Compare: " + msg).postln;

	},
	'/Compare'
);

// Planets overview
OSCdef.new(
	\Overview,
	{
		arg msg;

		// msg[0] = OSC Label
		// msg[1] = List of planets for the overview, which planets of the overview sonification
		//          should be turned on or off
		//     msg[1][0] = Mercury, 0: off, 1: on
		//     msg[1][1] = Venus
		//     msg[1][2] = Earth
		//     msg[1][3] = Mars
		//     msg[1][4] = Jupiter
		//     msg[1][5] = Saturn
		//     msg[1][5] = Uranus
		//     msg[1][6] = Neptune

		("Overview: " + msg).postln;
	},
	'/Overview'
);

// Planets sonifications, general map:
// msg[0] = OSC Label (The name of the planet)
// msg[1] = The distance from the camera to the planet in kilometers
// msg[2] = The horizontal angle in radians to the planet on the master screen,
//          with the current angle calculation mode taken into account
// msg[3] = The vertical/elevation angle in radians to the planet on the master screen,
//          with the current angle calculation mode taken into account
// msg[4] = List of GUI settings for the planet, what features of the sonification should be
//          turned on or off. If the setting does not exist for a planet, the value is always 0
//     msg[4][0] = Size/day, 0: off, 1: on
//     msg[4][1] = Gravity
//     msg[4][2] = Temperature
//     msg[4][3] = Atmosphere
//     msg[4][4] = Moons
//     msg[4][5] = Rings
// msg[5] = The distance from the camera to the first moon in kilometers
// msg[6] = Horizontal angle in radians from the planet to the first moon,
//          with the current angle calculation mode taken into account
// msg[7] = Vertical/Elevation angle in radians from the planet to the first moon,
//          with the current angle calculation mode taken into account
// msg[8] = The distance from the camera to the second moon in kilometers
// msg[9] = Horizontal angle in radians from the planet to the second moon
// msg[10] = Vertical/Elevation angle in radians from the planet to the second moon
// msg[...] = Data continues in the same pattern for as many moons there are for the planet

// Mercury
OSCdef.new(
	\Mercury,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Mercury does not have the GUI settings:
		//    - Atmosphere
		//    - Moons
		//    - Rings
		// Mercury have no moon data since it does not have any moon

		("Mercury: " + msg).postln;
	},
	'/Mercury'
);

// Venus
OSCdef.new(
	\Venus,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Venus does not have the GUI settings:
		//    - Moons
		//    - Rings
		// Venus have no moon data since it does not have any moon

		("Venus: " + msg).postln;
	},
	'/Venus'
);

// Earth
OSCdef.new(
	\Earth,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Earth does not have the GUI settings:
		//    - Rings
		// Earth have moon data for one moon:
		//    - The Moon

		("Earth: " + msg).postln;
	},
	'/Earth'
);

// Mars
OSCdef.new(
	\Mars,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Mars does not have the GUI settings:
		//    - Rings
		// Mars have moon data for 2 moons in the following order:
		//    - Phobos
		//    - Deimos

		("Mars: " + msg).postln;
	},
	'/Mars'
);

// Jupiter
OSCdef.new(
	\Jupiter,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Jupiter does not have the GUI settings:
		//    - Rings (there are rings in real life, but not in OpenSpace)
		// Jupiter have moon data for 4 moons in the following order:
		//    - Io
		//    - Europa
		//    - Ganymede
		//    - Callisto

		("Jupiter: " + msg).postln;
	},
	'/Jupiter'
);

// Saturn
OSCdef.new(
	\Saturn,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Saturn have moon data for 8 moons in the following order:
		//    - Dione
		//    - Enceladus
		//    - Hyperion
		//    - Iapetus
		//    - Mimas
		//    - Rhea
		//    - Tethys
		//    - Titan

		("Saturn: " + msg).postln;
	},
	'/Saturn'
);

// Uranus
OSCdef.new(
	\Uranus,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Uranus does not have the GUI settings:
		//    - Rings (there are rings in real life, but not in OpenSpace)
		// Uranus have moon data for 5 moons in the following order:
		//    - Ariel
		//    - Miranda
		//    - Oberon
		//    - Titania
		//    - Umbriel

		("Uranus: " + msg).postln;
	},
	'/Uranus'
);

// Neptune
OSCdef.new(
	\Neptune,
	{
		arg msg;

		// Follow the general planets sonificaiton data map above
		// Neptune does not have the GUI settings:
		//    - Rings (there are rings in real life, but not in OpenSpace)
		// Neptune have moon data for 2 moons in the following order:
		//    - Triton
		//    - Nereid

		("Neptune: " + msg).postln;
	},
	'/Neptune'
);

)
