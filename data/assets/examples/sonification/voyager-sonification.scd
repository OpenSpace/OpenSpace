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
 * This is an example file for the Voyager 1 and 2 space probes. This sonification will  *
 * play and loop the Greetings to the Universe audio clip (one of the recordings         *
 * included on the Golden Records aboard both probes) when the camera is close to either *
 * of the probes. For a more extensive example that shows how to create sounds in        *
 * SuperCollider based on the data from OpenSpace, see the planets-sonification.scd file *
 * located in data\assets\modules\telemetry\sonification. This sonification was made by  *
 * Elias Elmquist.                                                                       *
 *                                                                                       *
 * To run a SuperCollider file, click any line within the parentheses below, such as the *
 * marked line. When you have selected a line, press the CTRL and ENTER keys on your     *
 * keyboard at the same time, and SuperCollider will run the code. You will see several  *
 * messages appear in the log to the right, the last message should be:                  *
 * "---------- Sonification is ready ----------". At this point, SuperCollider is ready  *
 * to receive messages from OpenSpace and produce the sonification. For a guide on how   *
 * to enable and use the Telemetry module inside OpenSpace, see the documentation page:  *
 * https://docs.openspaceproject.com/latest/creating-data-assets/modules/telemetry/      *
 * index.html                                                                            *
 ****************************************************************************************/

(

// To run this example, click this line and press CTRL + ENTER on the keyboard
// When finished, press CTRL + . on you keyboard simultaneously to stop the sonification

s.quit;
o = Server.default.options;

// TODO Update this to use the angle calculation setting in OpenSpace
// Set the surround setting
// 0: Stereo (Binaurual)
// 1: VisC Dome
// 2: Hayden
~setup = 0;

if((~setup == 0),
	{~numSpeakerChannels = 2;}
);

if((~setup == 1),
	{o.outDevice_("ASIO : Focusrite USB ASIO");
		~numSpeakerChannels = 8;
	}
);

if((~setup == 2),
	{o.outDevice_("ASIO : ASIO MADIface USB");
		~numSpeakerChannels = 32;
	}
);

o.numOutputBusChannels = ~numSpeakerChannels;

//3D
~ampMode = 1;
~focusType = 1;
~lockedFocus = 0;
~comparisonMode = 1;

// Setup which nodes to listen to in OpenSpace
~oscLabel0 = \Voyager_1;
~oscLabel1 = \Voyager_2;

s.reboot;
s.waitForBoot{
	// Load the Greetings to the Universe audio clip (located next to this file)
	var filePath = thisProcess.nowExecutingPath.dirname;
	var loadSamples = {
		~b1 = Buffer.read(s, filePath +/+ "Greetings_from_Earth_mono.wav");
		// To load another audio clip, copy the line above, change the variable name
		// and the path to the audio file
	};

	var loadSynths = {
		// Create a sound function to play and loop the audio clip
		SynthDef(
			\audiofile,
			{
				arg out=0, gate=1, mamp=5, amp=0, baseAmp=1, buf, loop=0, rate=1, da=0, atk=1, dcay=0, suslvl=1, rel=1, mute=1, trig=0;
				var sig=0, env;

				env = EnvGen.kr(
					Env.adsr(atk,dcay,suslvl,rel),
					gate,
					doneAction:da
				);

				sig = PlayBuf.ar(1, buf, rate:rate, loop:loop, trigger:trig);
				sig = mamp * sig;
				sig = mute.lag(5) * (baseAmp*amp) * sig * env;

				Out.ar(out, sig);
			}
		).add;


		// Setup Ambisonics
		~order = 2; // set this to the order you want
		~numChannels = ((~order + 1)**2).asInteger;

		// binaural decoder (~numChannels -> 2) - reads from 'bus' and sums into 'out'
		SynthDef.new(\binauralDecoder, { | bus, out = 0 |
			Out.ar(out, VSTPlugin.ar(In.ar(bus, ~numChannels), 2));
		}).add;

		SynthDef.new(\allraDecoder, { | bus, out = 0 |
			Out.ar(out, VSTPlugin.ar(In.ar(bus, ~numChannels), ~numSpeakerChannels));
		}).add;

		// stereo encoder (2 -> ~numChannels) - replaces stereo signal with ambisonics signal
		SynthDef.new(\stereoEncoder, { | bus = 0 |
			ReplaceOut.ar(bus, VSTPlugin.ar(In.ar(bus, 2), ~numChannels));
		}).add;

		// ambisonics insert FX Surround (replaces input with output)
		SynthDef.new(\ambiFX, { | bus = 0, bypass |
			ReplaceOut.ar(bus, VSTPlugin.ar(In.ar(bus, ~numChannels), ~numChannels, bypass));
		}).add;

		// helper Synth (throws audio from ambi bus to ambi master bus)
		SynthDef.new(\ambiThrow, { | from, to |
			Out.ar(to, In.ar(from, ~numChannels));
		}).add;
	};

	var initiateAmbisonic = {
		// bus + group
		~ambiMasterBus = Bus.audio(s, ~numChannels);
		~ambiMasterGroup = Group.new;

		if((~setup == 0),
			// binaural decoder (writes to master output)
			{~decoder = VSTPluginController(Synth(\binauralDecoder, [\bus, ~ambiMasterBus, \out, 0], target: ~ambiMasterGroup, addAction: \addToTail)).open("BinauralDecoder");},
			// AllRA decoder
			{~decoder = VSTPluginController(Synth(\allraDecoder, [\bus, ~ambiMasterBus, \out, 0], target: ~ambiMasterGroup, addAction: \addToTail)).open("SimpleDecoder");}
		);

		// ambisonics insert FX (replaces input with output)
		SynthDef.new(\ambiFX, { | bus = 0, bypass |
			ReplaceOut.ar(bus, VSTPlugin.ar(In.ar(bus, 2), ~numChannels, bypass));
		}).add;

		// a group for ambisonic master effects
		~ambiMasterFXGroup = Group.before(~decoder.synth);
	};

	var initiateSynths = {
		// create ambisonic busses
		~soundBus = Array.newClear(7);
		~numBus = ~soundBus.size;
		~ambiBus = Array.newClear(~numBus);
		~ambiGroup = Array.newClear(~numBus);
		~encoder = Array.newClear(~numBus);
		~sounds = Array.newClear(~numBus);

		// First Audio clip
		~ambiBus[0] = Bus.audio(s, ~numChannels);
		~ambiGroup[0] = Group.before(~ambiMasterGroup);
		~sounds[0] = Synth.new(\audiofile, [\out, ~ambiBus[0], \buf, ~b1], ~ambiGroup[0], addAction: \addToHead);
		~encoder[0] = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBus[0]], target: ~ambiGroup[0], addAction: \addToTail));
		~encoder[0].open("StereoEncoder", action: { |self| self.set(6, 0.5) }); // 6 -> azimuth
		Synth(\ambiThrow, [\from, ~ambiBus[0], \to, ~ambiMasterBus], target: ~ambiGroup[0], addAction: \addToTail);

		// Second Audio clip (if needed)
		/*~ambiBus[1] = Bus.audio(s, ~numChannels);
		~ambiGroup[1] = Group.before(~ambiMasterGroup);
		~sounds[1] = Synth.new(\audiofile, [\out, ~ambiBus[1], \buf, ~b2], ~ambiGroup[1], addAction: \addToHead);
		~encoder[1] = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBus[1]], target: ~ambiGroup[1], addAction: \addToTail));
		~encoder[1].open("StereoEncoder", action: { |self| self.set(6, 0.6) }); // 6 -> azimuth
		Synth(\ambiThrow, [\from, ~ambiBus[1], \to, ~ambiMasterBus], target: ~ambiGroup[1], addAction: \addToTail);

		//Audio mixing of the sounds (if needed)
		~sounds[0].set(\baseAmp, 0.3);
		~sounds[1].set(\baseAmp, 0.3);*/

		// Debugging spatial position
		//~encoder[0].editor;

		~sounds[0].set(\baseAmp, 0.3);

		// add an ambisonic master FX
		~ambiReverb = VSTPluginController(Synth(\ambiFX, [\bus, ~ambiMasterBus, \out, 0],
			target: ~ambiMasterFXGroup)).open("FdnReverb", action: { |self| self.set(1, 0.2)  });

	};

	var loadDecoder = {
		if((~setup == 1),
			{
				~decoder.iemPluginOSC("/SimpleDecoder/loadFile", thisProcess.nowExecutingPath.dirname +/+ "AmbiDecoders"  +/+ "DomenVisC_5th.json");
				0.1.wait;
				~decoder.iemPluginOSC("/SimpleDecoder/swMode", 1);
				0.1.wait;
				~decoder.iemPluginOSC("/SimpleDecoder/swChannel", 4);
				0.1.wait;
				~decoder.iemPluginOSC("/SimpleDecoder/lowPassGain", -12);
			}
		);

		if((~setup == 2),
			{
				~decoder.iemPluginOSC("/SimpleDecoder/loadFile", thisProcess.nowExecutingPath.dirname +/+ "AmbiDecoders"  +/+ "HaydenIEM_5th_0deg_80top_rot180deg.json");
				0.1.wait;
				~decoder.iemPluginOSC("/SimpleDecoder/swMode", 1);
				0.1.wait;
				~decoder.iemPluginOSC("/SimpleDecoder/swChannel", 32);
				0.1.wait;
			}
		);
	};

	// Set which OSC labels to listen to
	var oscDef = {


		//Horizontal mode
		~modeElSign = 1;
		~modeElPhase = 0.5;

		//Circular mode (switch manually)
		//~modeElSign = -1;
		//~modeElPhase = 0.75;

		//Size of hearable area
		~distanceLimit = 0.5;

		//Ambient amp
		~ambientAmp = 1.0;

		OSCdef.new(\Mode,
			{
				arg msg;

				//msg[1] == 0 ->  Horizontal
				//msg[1] == 1 ->  Circular

				//msg[2] == 0 ->  Elevation on
				//msg[2] == 1 ->  Elevation off

				("SurroundMode:" + msg[1]).postln;

				//Horizontal
				if(((msg[1]==0)),
					{~modeElSign = 1;
						~modeElPhase = 0.5;},
					{}
				);

				//Circular
				if(((msg[1]==1)),
					{~modeElSign = -1;
						~modeElPhase = 0.75;},
					{}
				);

				("~modeElSign:" + ~modeElSign).postln;
				("~modeElPhase:" + ~modeElPhase).postln;

		},'/Mode');

		OSCdef.new(
			~oscLabel0,
			{
				arg msg;
				var name, distance, invDistance, azimuth, amp, value, elevation;

				//msg[1]: label distance
				//msg[2]: label azimuth
				//msg[3]: label elevation

				// DEBUG: Uncomment this line to get console output of all messages from OpenSpace
				//(~oscLabel0 ++ ": " ++ msg).postln;

				name = msg[0].asString;

				distance = msg[1]/1000000;

				//invDistance = ~distanceLimit-distance;

				azimuth = (msg[2]/(2*pi))+0.5;
				elevation = ~modeElSign*(msg[3]/(2*pi))+~modeElPhase;

				if((~ampMode == 1),
					{
						if((distance < ~distanceLimit),
							{amp = LinLin.kr(distance, 0, ~distanceLimit, 0, 1);

							},
							{amp = ~ambientAmp;}
						);
					}
				);

				(~oscLabel0 ++ "Amp: " ++ amp).postln;
				~sounds[0].set(\amp, amp);
				~encoder[0].do(_.set(6, azimuth));
				~encoder[0].do(_.set(7, elevation));
			},
			~oscLabel0
		);

		OSCdef.new(
			~oscLabel1,
			{
				arg msg;
				var name, distance, invDistance, azimuth, amp, value, elevation;

				//msg[1]: label distance
				//msg[2]: label azimuth
				//msg[3]: label elevation

				// DEBUG: Uncomment this line to get console output of all messages from OpenSpace
				//(~oscLabel1 ++ ": " ++ msg).postln;

				name = msg[0].asString;

				distance = msg[1]/1000000;

				//invDistance = ~distanceLimit-distance;

				azimuth = (msg[2]/(2*pi))+0.5;
				elevation = ~modeElSign*(msg[3]/(2*pi))+~modeElPhase;

				if((~ampMode == 1),
					{
						if((distance < ~distanceLimit),
							{amp = LinLin.kr(distance, 0, ~distanceLimit, 0, 1);

							},
							{amp = ~ambientAmp;}
						);
					}
				);

				(~oscLabel1 ++ "Amp: " ++ amp).postln;
				~sounds[0].set(\amp, amp);
				~encoder[0].do(_.set(6, azimuth));
				~encoder[0].do(_.set(7, elevation));
			},
			~oscLabel1
		);

	};

	// Start routine
	~startRoutine = Routine({
		0.5.wait;
		loadSamples.value;
		"--Loaded the data--".postln;
		0.2.wait;
		loadSynths.value;
		"--Loaded the synths--".postln;
		0.3.wait;
		initiateAmbisonic.value;
		"--Loaded the ambisonics--".postln;
		0.3.wait;
		initiateSynths.value;
		"--Initiated the synths--".postln;
		0.2.wait;
		loadDecoder.value;
		"--Loaded the decoder--".postln;
		0.2.wait;
		oscDef.value;
		"--Loaded the OSCdefs--".postln;
		"---------- Sonification is ready ----------".postln;
	});

	~startRoutine.play(AppClock);
}

)
