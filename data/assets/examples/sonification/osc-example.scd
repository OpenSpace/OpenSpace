/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

Platform.userExtensionDir
NetAddr.langPort;
NetAddr.localAddr;

/*****************************************************************************************
 * This is a SuperCollider file that needs to be run in the SuperCollider application    *
 * that can be downloaded here: https://supercollider.github.io/                         *
 *                                                                                       *
 * This is an example file that shows all the messages that the Telemetry module in      *
 * OpenSpace can send over Open Sound Control (OSC). SuperCollider can then listen to    *
 * these OSC messages and create sounds based on their content (a sonification). This    *
 * example only listens to the messages and displays their content in the log to the     *
 * right in the SuperCollider application. For a more extensive example that shows how   *
 * to create sounds in SuperCollider based on the data from OpenSpace, see the           *
 * planets-sonification.scd file located in data\assets\modules\telemetry\sonification.  *
 *                                                                                       *
 * To run a SuperCollider file, click any line within the parentheses below, such as the *
 * marked line. When you have selected a line, press the CTRL and ENTER keys on your     *
 * keyboard at the same time, and SuperCollider will run the code. You will see a        *
 * message appear in the log to the right, the message should be:                        *
 * "-> OSCdef(Neptune, /Neptune, nil, nil, nil)". At this point, SuperCollider is ready  *
 * to receive messages from OpenSpace. For a guide on how to enable and use the          *
 * Telemetry module inside OpenSpace, see this documentation page:                       *
 * https://docs.openspaceproject.com/latest/creating-data-assets/modules/telemetry/      *
 * index.html                                                                            *
 ****************************************************************************************/

(

// To run this example, click this line and press CTRL + ENTER on the keyboard
// When finished, press CTRL + . on you keyboard simultaneously to stop the sonification

// Camera sonification
OSCdef.new(
	\Camera,
	{
		arg msg;

		// msg[0] = OSC Label.
		// msg[1] = The x position of the camera in the world.
		// msg[2] = The y position of the camera in the world.
		// msg[3] = The z position of the camera in the world.
		// msg[4] = The w component of the quaternion rotation of the camera.
		// msg[5] = The x component of the quaternion rotation of the camera.
		// msg[6] = The y component of the quaternion rotation of the camera.
		// msg[7] = The z component of the quaternion rotation of the camera.
		// msg[8] = The movement speed of the camera, in the distance unit per second
		//          specified by the next item.
		// msg[9] = The distance unit for the movement speed of the camera, as a string in
		//          singular form with the first letter capitalized. For example,
		//          "Kilometer".
		//
		// Note that the first seven items describe the position and orientation of the
		// camera. The first three items specify the x, y, and z positions of the camera
		// in the world, in relation to the solar system barycenter. The next four items
		// are the quaternion rotation of the camera in the order of w, x, y, and z.

		("Camera: " + msg).postln;
	},
	'/Camera'
);

// Focus sonification
OSCdef.new(
	\Focus,
	{
		arg msg;

		// msg[0] = OSC Label.
		// msg[1] = The identifier of the new focus in OpenSpace, as a string.

		("Focus: " + msg).postln;
	},
	'/Focus'
);

// Surround mode sonification
OSCdef.new(
	\Mode,
	{
		arg msg;

		// msg[0] = OSC Label.
		// msg[1] = The first item is an integer value that specifies what method was
		//          used to calculate the angles. If the value is 0, then the method
		//          used was the Horizontal angle calculation mode. In the case where
		//          the value is 1, then the Circular angle calculation mode was used.
		// msg[2] = The second value is an integer value of either 0 or 1 that
		//          determines if the additional elevation angle is used or not. If the
		//          value is 1, the additional elevation angle is calculated. Otherwise,
		//          if 0, the elevation angle is always set to 0.0.

		("Mode: " + msg).postln;
	},
	'/Mode'
);

// Time sonification
OSCdef.new(
	\Time,
	{
		arg msg;

		// msg[0] = OSC Label.
		// msg[1] = The speed of the simulation time, specified in the selected time unit
		//          in the simulation, per real-life second. For example, 10 simulated
		//          seconds per real-life second means that the simulation goes 10 times
		//          faster than real-life.
		// msg[2] = The selected time unit for the speed of simulation time, as a string
		//          in singular form with the first letter capitalized. For example,
		//          "Day".
		// msg[3] = The current simulation time in OpenSpace specified in J2000 seconds,
		//          that is, the number of seconds past the J2000 epoch
		//          (i.e. January 1, 2000 12:00:00 TT).

		("Time: " + msg).postln;
	},
	'/Time'
);

// Nodes sonification, data map:
// msg[0] = OSC Label (The identifier for each node).
// msg[1] = The distance from the camera to the node, in the distance unit specified in
//          the last item.
// msg[2] = The horizontal angle to the node, in radians, with the current angle
//          calculation mode taken into account. For more information, see
//          https://docs.openspaceproject.com/latest/creating-data-assets/modules/
//          telemetry/angle-information.html.
// msg[3] = The elevation angle to the node, in radians, with the current angle
//          calculation mode taken into account. Again, see
//          https://docs.openspaceproject.com/latest/creating-data-assets/modules/
//          telemetry/angle-information.html for details.
// msg[4] = The unit for the distance to the camera, as a string in singular form with
//          the first letter capitalized. For example, "Meter".

// ISS node
OSCdef.new(
	\ISS,
	{
		arg msg;

		// Follow the nodes sonification data map above

		("ISS: " + msg).postln;
	},
	'/ISS'
);

// Tiangong node
OSCdef.new(
	\Tiangong,
	{
		arg msg;

		// Follow the nodes sonification data map above

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

		// msg[0] = OSC Label.
		// msg[1] = The index of the first planet to be compared. See the list below on
		//          how to convert the index to a planet name:
		//            0 -> None selected
		//            1 -> Mercury
		//            2 -> Venus
		//            3 -> Earth
		//            4 -> Mars
		//            5 -> Jupiter
		//            6 -> Saturn
		//            7 -> Uranus
		//            8 -> Neptune
		// msg[2] = The index of the second planet to be compared (will never be the same
		//          as the first).
		// msg[3] = List of user interface settings for the comparison, which determines
		//          which aspects of the sonification should be turned on or off. A value
		//          of 0 means that a setting is turned off, and 1 means that it is turned
		//          on. The order of the settings can be seen in the list below:
		//            msg[3][0] = Size/day. 1 -> Size/day is turned on.
		//                                  0 -> Size/day is turned off.
		//            msg[3][1] = Gravity
		//            msg[3][2] = Temperature
		//            msg[3][3] = Atmosphere
		//            msg[3][4] = Moons
		//            msg[3][5] = Rings

		("Compare: " + msg).postln;

	},
	'/Compare'
);

// Planets overview
OSCdef.new(
	\Overview,
	{
		arg msg;

		// msg[0] = OSC Label.
		// msg[1] = List of user interface settings for the planets overview. This
		//          determines which planets are part of the sonification or not. A value
		//          of 0 means that the planet is turned off, and a 1 means that it is
		//          turned on. The order of the settings can be seen in the list below:
		//            msg[1][0] = Mercury. 1 -> Mercury is turned on.
		//                                 0 -> Mercury is turned off.
		//            msg[1][1] = Venus
		//            msg[1][2] = Earth
		//            msg[1][3] = Mars
		//            msg[1][4] = Jupiter
		//            msg[1][5] = Saturn
		//            msg[1][6] = Uranus
		//            msg[1][7] = Neptune

		("Overview: " + msg).postln;
	},
	'/Overview'
);

// Planets sonifications, data map:
// msg[0] = OSC Label (The name of the planet).
// msg[1] = The distance from the camera to the planet in kilometers.
// msg[2] = The horizontal angle in radians to the planet, with the current angle
//          calculation mode taken into account. For more information see
//          https://docs.openspaceproject.com/latest/creating-data-assets/modules/
//          telemetry/angle-information.html.
// msg[3] = The elevation angle in radians to the planet, with the current angle
//          calculation mode taken into account. Again, see
//          https://docs.openspaceproject.com/latest/creating-data-assets/modules/
//          telemetry/angle-information.html for details.
// msg[4] = List of user interface settings for the planet sonification, which aspects
//          of the sonification should be turned on or off. A value of 0 means that it
//          is turned off, and a 1 means that it is turned on. The order of the
//          settings can be seen in the table below. If the setting does not exist for
//          a planet, the value is always 0.
//            msg[4][0] = Size/day. 1 -> Size/day is turned on.
//                                  0 -> Size/day is turned off.
//            msg[4][1] = Gravity
//            msg[4][2] = Temperature
//            msg[4][3] = Atmosphere
//            msg[4][4] = Moons
//            msg[4][5] = Rings
// msg[5] = (optional) The distance from the camera to the first moon in kilometers.
// msg[6] = (optional) The horizontal angle in radians to the first moon.
// msg[7] = (optional) The elevation angle in radians to the first moon.
// msg[8] = (optional) The distance from the camera to the second moon in kilometers.
// msg[9] = (optional) The horizontal angle in radians to the second moon.
// msg[10] = (optional) The elevation angle in radians to the second moon.
// msg[...] = The data then continues in the same pattern for each of the planet's
//            moons, with three values per moon. The moons are given in order of
//            distance from the planet (closest first, farthest last) as specified in
//            the planets.asset file in the data\assets\modules\telemetry\sonification
//            folder.

// Mercury
OSCdef.new(
	\Mercury,
	{
		arg msg;

		// Follow the planets sonification data map above
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

		// Follow the planets sonification data map above
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

		// Follow the planets sonification data map above
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

		// Follow the planets sonification data map above
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

		// Follow the planets sonification data map above
		// Jupiter does not have the GUI settings:
		//    - Rings (there are rings in real-life, but not in OpenSpace)
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

		// Follow the planets sonification data map above
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

		// Follow the planets sonification data map above
		// Uranus does not have the GUI settings:
		//    - Rings (there are rings in real-life, but not in OpenSpace)
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

		// Follow the planets sonification data map above
		// Neptune does not have the GUI settings:
		//    - Rings (there are rings in real-life, but not in OpenSpace)
		// Neptune have moon data for 2 moons in the following order:
		//    - Triton
		//    - Nereid

		("Neptune: " + msg).postln;
	},
	'/Neptune'
);

)
