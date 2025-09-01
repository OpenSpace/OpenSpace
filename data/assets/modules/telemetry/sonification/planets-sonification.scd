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
* This is a sonification of the planets in the solar system, for more details, see the  *
* http://dx.doi.org/10.21785/icad2021.018. This sonification was made by Elias          *
* Elmquist.
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

//Requirements:
// * VST plugin: https://git.iem.at/pd/vstplugin/-/releases
// * IEM Plug-in Suite: https://plugins.iem.at/download/
// * IEM OSC Plug-in: https://git.iem.at/ressi/iempluginosc

// To run this sonification, click this line and press CTRL + ENTER on the keyboard
// When finished, press CTRL + . on you keyboard simultaneously to stop the sonification

s.quit;
o = Server.default.options;
o.numOutputBusChannels = 2;
o.numOutputBusChannels;
o.memSize = 32*8192;

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

s.reboot;
s.waitForBoot{

	var loadData = {

		var mul = 1, orbitscale= 0.0, gravityscale=2, tempscale=174, tempscale2=2, tempscale3=99;

		~time = 1;

		//Mass (10^24 kg), C2-C5
		~massMercury = 523.25*mul;//C5
		~massVenus =  329.63*mul;//E4
		~massEarth =  261.63*mul;//C4
		~massMars =  392.00*mul;//G4

		~massJupiter = 65.406*mul; //C2
		~massSaturn = 82.41*mul; //E2
		~massUranus = 130.81*mul; //C3
		~massNeptune =  110.00*mul; //A2

		//Moons, not scaled
		~massMoon = 659.25; //E5

		~massPhobos = 783.99; //G5
		~massDeimos = 1046.50; //C6

		~massIo = 659.25; //E5, similar to moon
		~massEuropa = 783.99; //G5
		~massGanymede =  440.00; //A4, largest moon of Jupiter
		~massCallisto = 523.25; //C5, similar mass to mercury

		~massMimas =  1046.50; //C6
		~massEnceladus = 880.00; //A5
		~massTethys =  783.99; //G5
		~massDione = 659.25; //E5,
		~massRhea = 523.25; //C5,
		~massTitan = 440.00; //A4, largest of Saturn
		~massHyperion = 1318.51; //E6
		~massIapetus = 587.33; //D5

		~massAriel = 880.00; //A5
		~massMiranda = 1046.50; //C6
		~massOberon =  659.25; //E5
		~massTitania = 523.25; //C5
		~massUmbriel = 783.99; //G5

		~massTriton = 440.00; //A4

		//Diameter
		~diameterMercury = 4879;
		~diameterVenus = 12104;
		~diameterEarth = 12756;
		~diameterMars = 6792;

		~diameterJupiter = 142984;
		~diameterSaturn = 120536;
		~diameterUranus = 51118;
		~diameterNeptune = 49528;

		//orbital period, Earth ratio, (1 day/second)
		~orbitMercury = (1/88)*orbitscale;
		~orbitVenus = (1/224.7)*orbitscale;
		~orbitEarth = (1/365.2)*orbitscale;
		~orbitMars = (1/687)*orbitscale;

		~orbitJupiter = (1/4331)*orbitscale;
		~orbitSaturn = (1/10747)*orbitscale;
		~orbitUranus = (1/30589)*orbitscale;
		~orbitNeptune = (1/59800)*orbitscale;

		~orbitMoon = 1/27.3;

		~orbitPhobos = 1/0.31891;
		~orbitDeimos = 1/1.26244;

		~orbitIo = 1/1.8;
		~orbitEuropa = 1/3.6;
		~orbitGanymede = 1/7.2;
		~orbitCallisto = 1/16.7;

		~orbitMimas = 1/0.9424218;
		~orbitEnceladus = 1/1.370218;
		~orbitTethys =  1/1.887802;
		~orbitDione = 1/2.736915;
		~orbitRhea = 1/4.517500;
		~orbitTitan = 1/15.945421;
		~orbitHyperion = 1/21.276609;
		~orbitIapetus = 1/79.330183;

		~orbitAriel = 1/2.520379;
		~orbitMiranda = 1/1.413479;
		~orbitOberon = 1/13.463234;
		~orbitTitania = 1/8.705867;
		~orbitUmbriel = 1/4.144176;

		~orbitTriton = 1/5.876854;

		//Length of day, ratio earth (1/rotationRatio),(24hrs/sec)
		~invrotationMercury = 1/175.9;
		~invrotationVenus = 1/116.8; //-, retrograde rotation
		~invrotationEarth = 1;
		~invrotationMars = 1/1.03;

		~invrotationJupiter = 1/0.414;
		~invrotationSaturn = 1/0.444;
		~invrotationUranus = 1/0.718; //-, retrograde rotation
		~invrotationNeptune = 1/0.671;

		//Gravity (g), relation to Earth
		~gravityMercury = 0.378/gravityscale;
		~gravityVenus = 0.907/gravityscale;
		~gravityEarth = 1/gravityscale;
		~gravityMars = 0.377/gravityscale;

		~gravityJupiter = 2.36/gravityscale;
		~gravitySaturn = 0.916/gravityscale;
		~gravityUranus = 0.889/gravityscale;
		~gravityNeptune = 1.12/gravityscale;

		//Avg temperature
		~tempMercury = (167+tempscale)*tempscale2;
		~tempVenus = (464+tempscale)*tempscale2;
		~tempEarth = (15+tempscale)*tempscale2;
		~tempMars = (-65+tempscale)*tempscale2;

		//Lowest temperature
		~lowtempMercury = 100-tempscale3;
		~lowtempVenus = 735-tempscale3;
		~lowtempEarth = 185-tempscale3;
		~lowtempMars = 120-tempscale3;

		//Highest temperature
		~hightempMercury = 700-tempscale3;
		~hightempVenus = 735-tempscale3;
		~hightempEarth = 331-tempscale3;
		~hightempMars = 293-tempscale3;

		//Gas giants only have Average temperature
		~lowtempJupiter = ~hightempJupiter = 163-tempscale3;
		~lowtempSaturn = ~hightempSaturn = 133-tempscale3;
		~lowtempUranus = ~hightempUranus = 100-tempscale3; //78
		~lowtempNeptune = ~hightempNeptune = 100-tempscale3; //73

	};


	var loadSynthDefs = {

		var filePath = thisProcess.nowExecutingPath.dirname;

		~order = 2; // set this to the ambisonics order you want
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

		// Set the lag time for the distance data
		~poslagTime = 0.1;

		SynthDef(\surroundmod, {
			arg out=0,gate=0, amp=0.3, amp2=1.0, freq=200, rate=1, time=1, pan=0, type=0, isMagnetic=0;
			var sig, sig2, globalAmp, env, resfreq, lfo;

			env = EnvGen.kr(
				Env.adsr(5.0,0.0,1.0,5.0),
				gate,
				doneAction:0);

			sig = Select.ar(type,[
				LFSaw.ar(Vibrato.kr(freq/2,rate*time,0.004),0,0.4),
				LFPulse.ar(Vibrato.kr(freq,rate*time,0.004),0,0.4)]);

			//Sweeping resonant low-pass filter for length of day
			resfreq = SinOsc.kr(rate*time) * 200 + 400;
			sig = RLPF.ar(sig, resfreq,0.5);

			//Phaser sweep if planet has a magnetic field
			lfo = LinLin.ar(Vibrato.ar(0.4,0.04,1.0,0,0,0.2,0.2), 0, 1, 600, 5000);
			sig = Select.ar(isMagnetic,
				[sig, sig + HPF.ar(BPF.ar(sig, lfo, 0.1)*2,freq*3)]);

			globalAmp = Lag.kr(amp2, ~poslagTime);
			sig = sig * env * amp * globalAmp;

			Out.ar(out, sig);
		}).add;

		SynthDef.new(\gravity, {
			arg out=0,gate=0, amp=0.65, amp2=1.0, freq=200, rate=1, pan=0, gravity=1, type=0, time=1;
			var sig, globalAmp, env, b, sf, gFreq;

			env = EnvGen.kr(
				Env.adsr(5.0,0.0,1.0,5.0),
				gate,
				doneAction:0);

			gFreq = Select.kr(type,[
				freq;,
				freq;]);

			//Physical model of a ball
			sf = LFPulse.ar(rate*time,0.0);
			b = TBall.ar(sf, gravity,0.03,0.01);
			sig = Ringz.ar(b * 20, gFreq, 1.0);

			globalAmp = Lag.kr(amp2, ~poslagTime);
			sig = sig * env * amp * globalAmp;

			Out.ar(out, sig);
		}).add;

		SynthDef.new(\wind, {
			arg out=0,gate=0, amp=0.4, amp2=1.0, rate=0.45, pan=0, type=0, gust=1, atmos=60;
			var sig, globalAmp, env, lfo;

			env = EnvGen.kr(
				Env.adsr(5.0,0.0,1.0,5.0),
				gate,
				doneAction:0);

			//Select signal depending on planet type
			sig = Select.ar(type,[
				PinkNoise.ar(amp);,
				BrownNoise.ar(amp);]);

			//Sweeping low-pass filter on noise to create wind sound
			sig = LPF.ar(sig, Vibrato.kr(1000*gust,rate,rate,rateVariation:0.7,depthVariation:0.3));
			sig = LPF.ar(sig, Vibrato.kr(5000,rate,rate,rateVariation:0.7,depthVariation:0.3).lag(0.5));

			//High-pass filter for thinner atmosphere
			sig = HPF.ar(sig, atmos);

			globalAmp = Lag.kr(amp2, ~poslagTime);
			sig = sig * env * amp * globalAmp;

			Out.ar(out, sig);
		}).add;

		SynthDef.new(\fry, {
			arg out=0,gate=0, amp=0.8, amp2=1.0, freq=440, rate=1, time=1, pan=0, type=0, lowtemp=20, hightemp=200;
			var sig, globalAmp, env, newrate, newrate2, newrate3, grate;

			env = EnvGen.kr(
				Env.adsr(5.0,0.0,1.0,5.0),
				gate,
				doneAction:0);

			//Set temperature range and length of day.
			newrate = SinOsc.kr(rate*time).range(lowtemp,hightemp);

			newrate2 = SinOsc.kr(rate*time).range(lowtemp/2000,hightemp/2000);

			newrate3 = SinOsc.kr(rate*time).range((lowtemp*10)+20,(hightemp*10)+20);

			//Grating sound
			sig = Dust.ar(newrate, 0.20);

			//Select Low-pass filter (depending on day/night)
			//depending on if the temperature is dynamic or not
			sig = Select.ar(type,[
				LPF.ar(sig,SinOsc.kr(rate*time).range(4000,13000));,
				LPF.ar(sig,12000);]);

			sig = sig + BPF.ar(Crackle.ar(2.0),newrate3,3,newrate2);

			globalAmp = Lag.kr(amp2, ~poslagTime);
			sig = sig * env * amp * globalAmp;

			Out.ar(out, sig);
		}).add;

		SynthDef(\rings, {
			arg out=0, gate=0, amp=0.2, amp2=1.0, freq=400, rate=1, pan=0, type=0, dev=1.04;
			var sig, globalAmp, temp, env;

			env = EnvGen.kr(
				Env.adsr(5.0,0.0,1.0,5.0),
				gate,
				doneAction:0);

			sig = 0;
			3.do{
				arg count;
				temp = SinOsc.ar(
					freq *
					(count + 1) *
					LFNoise1.kr({Rand(0.05,0.2)}!3).range(dev.reciprocal,dev)
				);
				temp = temp * LFNoise1.kr({Rand(0.5,8)}!7).exprange(0.01,1);
				sig = sig + temp;
			};
			sig = LPF.ar(sig,400);
			sig = sig * 0.5;

			globalAmp = Lag.kr(amp2, ~poslagTime);
			sig = sig * env * amp * globalAmp;

			Out.ar(out, sig);
		}).add;

		SynthDef(\percSolar, {
			arg freq=400, rate=1, pan=0, out=0, amp=0.2, amp2=1.0, gate=0, phase=0, time=1;
			var sig, globalAmp, env, t_gate=0;

			t_gate = gate * LFPulse.kr(rate*time,phase);
			env = EnvGen.kr(
				Env.perc(0.01,1/(rate*time)),
				t_gate,
				doneAction:0);

			sig = BrownNoise.ar(1);
			sig = (BPF.ar(sig, freq*2, 0.004) * 50);

			globalAmp = Lag.kr(amp2, ~poslagTime);
			sig = sig * env * amp * globalAmp;
			Out.ar(out, sig);
		}).add;

		SynthDef(\percMoon, {
			arg out=0,freq=400, rate=1, pan=0, amp=0.3, ampMoon=1.0, gate=0, phase=0, time=1, pangate=0;
			var sig, globalAmp, env, t_gate=0;

			t_gate = gate * LFPulse.kr(rate*time,phase);

			env = EnvGen.kr(
				Env.perc(0.01,1/(rate*time)),
				t_gate,
				doneAction:0);

			sig = BrownNoise.ar(1);
			sig = (BPF.ar(sig, freq, 0.005) * 20);

			globalAmp = Lag.kr(ampMoon, ~poslagTime);
			sig = sig * env * amp * globalAmp;
			Out.ar(out, sig);
		}).add;


		SynthDef(\sinamppoof, {
			arg freq=400, rate=1, pan=0, out=0, amp=0.6, amp2=1.0, gate=0, time=1;
			var sig, globalAmp, sig2, env, env2, t_gate=0,oscillator1, oscillator2, resfreq, sinlvl;

			env = EnvGen.kr(
				Env.adsr(0.01,0.0,0.3,0.1),
				gate,
				doneAction:0);
			env2 = EnvGen.kr(
				Env.adsr(0.05,0.5,0.3,0.5),
				gate,
				doneAction:0);

			sinlvl = SinOsc.kr(rate*time).range(0,1);

			sig = BrownNoise.ar(1);
			sig2 = (BPF.ar(sig, freq*1, 0.01) * 5) * (1-sinlvl);
			sig = (BPF.ar(sig, freq*2, 0.005) * 20)* sinlvl;

			globalAmp = Lag.kr(amp2, ~poslagTime);
			sig = sig * env;
			sig2 = sig2 * env2;
			sig = (sig + sig2)/2 * amp * globalAmp;
			Out.ar(out, sig);
		}).add;
	};

	// create ambisonic master section
	var loadAmbiGroups = {

		// bus + group
		~ambiMasterBus = Bus.audio(s, ~numChannels);
		~ambiMasterGroup = Group.new;

		if((~setup == 0),
			// binaural decoder (writes to master output)
			{~decoder = VSTPluginController(Synth(\binauralDecoder, [\bus, ~ambiMasterBus, \out, 0], target: ~ambiMasterGroup, addAction: \addToTail)).open("BinauralDecoder");},
			// AllRA decoder
			{~decoder = VSTPluginController(Synth(\allraDecoder, [\bus, ~ambiMasterBus, \out, 0], target: ~ambiMasterGroup, addAction: \addToTail)).open("SimpleDecoder");}
		);

		// a group for ambisonic master effects
		~ambiMasterFXGroup = Group.before(~decoder.synth);

		//Groups
		~solarGrp = Group.new;
		~innerSolarGrp = Group.new(~solarGrp);
		~outerSolarGrp = Group.new(~solarGrp);

		~planetaryGrp = Group.new;
		~innerPlanetaryGrp = Group.new(~planetaryGrp);
		~outerPlanetaryGrp = Group.new(~planetaryGrp);

		~mercuryGrp = Group.new(~innerPlanetaryGrp);
		~venusGrp = Group.new(~innerPlanetaryGrp);
		~earthGrp = Group.new(~innerPlanetaryGrp);
		~marsGrp = Group.new(~innerPlanetaryGrp);
		~jupiterGrp = Group.new(~outerPlanetaryGrp);
		~saturnGrp = Group.new(~outerPlanetaryGrp);
		~uranusGrp = Group.new(~outerPlanetaryGrp);
		~neptuneGrp = Group.new(~outerPlanetaryGrp);

		//~fxGrp = Group.after(~solarGrp);
		~solarGrp = Group.before(~ambiMasterGroup);

	};

	var initiateSynths = {

		~moonMode = \percMoon;

		//Mercury
		~ambiBusMercury = Bus.audio(s, ~numChannels);

		~mercuryP = Synth.new(\surroundmod, [
			\amp, 0.2,
			\type, 0,
			\freq, ~massMercury,
			\rate, ~invrotationMercury,
			\isMagnetic, 1,
			\out, ~ambiBusMercury],  ~mercuryGrp, addAction: \addToHead);

		~mercuryG = Synth.new(\gravity, [
			\amp, 0.5,
			\type, 0,
			\freq, ~massMercury,
			\rate, ~gravityMercury/2,
			\gravity, ~gravityMercury,
			\out, ~ambiBusMercury],  ~mercuryGrp, addAction: \addToHead);

		~mercuryT = Synth.new(\fry, [
			\rate, ~invrotationMercury,
			\type, 0,
			\lowtemp, ~lowtempMercury,
			\hightemp, ~hightempMercury,
			\out, ~ambiBusMercury],  ~mercuryGrp, addAction: \addToHead);

		~encoderMercury = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusEarth], target: ~mercuryGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusMercury, \to, ~ambiMasterBus], target: ~mercuryGrp, addAction: \addToTail);

		//Venus
		~ambiBusVenus = Bus.audio(s, ~numChannels);

		~venusP = Synth.new(\surroundmod, [
			\amp, 0.2,
			\type, 0,
			\freq, ~massVenus,
			\rate, ~invrotationVenus,
			\isMagnetic, 0,
			\out, ~ambiBusVenus],  ~venusGrp, addAction: \addToHead);

		~venusG = Synth.new(\gravity, [
			\type, 0,
			\amp, 0.8,
			\freq, ~massVenus,
			\rate, ~gravityVenus/4,
			\gravity, ~gravityVenus,
			\out, ~ambiBusVenus],  ~venusGrp, addAction: \addToHead);

		~venusW = Synth.new(\wind, [
			\amp, 0.5,
			\type, 1,
			\rate, 0.6,
			\gust, 2,
			\out, ~ambiBusVenus],  ~venusGrp, addAction: \addToHead);

		~venusT = Synth.new(\fry, [
			\type, 1,
			\rate, ~invrotationVenus,
			\lowtemp, ~lowtempVenus ,
			\hightemp, ~hightempVenus,
			\out, ~ambiBusVenus],  ~venusGrp, addAction: \addToHead);

		~encoderVenus = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusVenus], target: ~venusGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusVenus, \to, ~ambiMasterBus], target: ~venusGrp, addAction: \addToTail);


		//Earth
		~ambiBusEarth = Bus.audio(s, ~numChannels);

		~earthP = Synth.new(\surroundmod, [
			\type, 0,
			\freq, ~massEarth,
			\rate, ~invrotationEarth,
			\isMagnetic, 1,
			\out, ~ambiBusEarth],  ~earthGrp, addAction: \addToHead);

		~earthG = Synth.new(\gravity, [
			\type, 0,
			\freq, ~massEarth,
			\rate, ~invrotationEarth/4,
			\gravity, ~gravityEarth,
			\out, ~ambiBusEarth], ~earthGrp, addAction: \addToHead);

		~earthW = Synth.new(\wind, [
			\amp, 0.7,
			\type, 0,
			\rate, 0.3,
			\gust, 1,
			\out, ~ambiBusEarth], ~earthGrp, addAction: \addToHead);

		~earthT = Synth.new(\fry, [
			\type, 0,
			\rate, ~invrotationEarth,
			\lowtemp, ~lowtempEarth ,
			\hightemp, ~hightempEarth,
			\out, ~ambiBusEarth], ~earthGrp, addAction: \addToHead);

		~encoderEarth = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusEarth], target: ~earthGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusEarth, \to, ~ambiMasterBus], target: ~earthGrp, addAction: \addToTail);

		//Moon
		~ambiBusMoon = Bus.audio(s, ~numChannels);

		~moonP = Synth.new(~moonMode, [
			\freq, ~massMoon,
			\rate, ~orbitMoon,
			\out, ~ambiBusMoon], ~earthGrp, addAction: \addToHead);

		~encoderMoon = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusMoon], target: ~earthGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusMoon, \to, ~ambiMasterBus], target: ~earthGrp, addAction: \addToTail);

		//Mars
		~ambiBusMars = Bus.audio(s, ~numChannels);

		~marsP = Synth.new(\surroundmod, [
			\type, 0,
			\freq, ~massMars,
			\rate, ~invrotationMars,
			\isMagnetic, 0,
			\out, ~ambiBusMars],  ~marsGrp, addAction: \addToHead);

		~marsG = Synth.new(\gravity, [
			\type, 0,
			\freq, ~massMars,
			\rate, ~invrotationMars/5,
			\gravity, ~gravityMars,
			\out, ~ambiBusMars],  ~marsGrp, addAction: \addToHead);

		~marsW = Synth.new(\wind, [
			\amp, 0.6,
			\type, 0,
			\rate, 0.1,
			\gust, 2,
			\atmos, 400,
			\out, ~ambiBusMars],  ~marsGrp, addAction: \addToHead);

		~marsT = Synth.new(\fry, [
			\type, 0,
			\rate, ~invrotationMars,
			\lowtemp, ~lowtempMars,
			\hightemp, ~hightempMars,
			\out, ~ambiBusMars],  ~marsGrp, addAction: \addToHead);

		~encoderMars = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusMars], target: ~marsGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusMars, \to, ~ambiMasterBus], target: ~marsGrp, addAction: \addToTail);

		//Phobos
		~ambiBusPhobos = Bus.audio(s, ~numChannels);

		~phobosP = Synth.new(~moonMode, [
			\freq, ~massPhobos,
			\rate, ~orbitPhobos,
			\out, ~ambiBusPhobos],  ~marsGrp, addAction: \addToHead);

		~encoderPhobos = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusPhobos], target: ~marsGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusPhobos, \to, ~ambiMasterBus], target: ~marsGrp, addAction: \addToTail);

		//Deimos
		~ambiBusDeimos = Bus.audio(s, ~numChannels);

		~deimosP = Synth.new(~moonMode, [
			\freq, ~massDeimos,
			\rate, ~orbitDeimos,
			\out, ~ambiBusDeimos],  ~marsGrp, addAction: \addToHead);

		~encoderDeimos = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusDeimos], target: ~marsGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusDeimos, \to, ~ambiMasterBus], target: ~marsGrp, addAction: \addToTail);


		//Jupiter
		~ambiBusJupiter = Bus.audio(s, ~numChannels);

		~jupiterP = Synth.new(\surroundmod, [
			\amp, 0.5,
			\type, 1,
			\freq, ~massJupiter,
			\rate, ~invrotationJupiter,
			\isMagnetic, 1,
			\out, ~ambiBusJupiter],  ~jupiterGrp, addAction: \addToHead);

		~jupiterG = Synth.new(\gravity, [
			\amp, 0.8,
			\type, 1,
			\freq, ~massJupiter,
			\rate, ~invrotationJupiter/4,
			\gravity, ~gravityJupiter,
			\out, ~ambiBusJupiter],  ~jupiterGrp, addAction: \addToHead);

		~jupiterW = Synth.new(\wind, [
			\amp, 0.5,
			\type, 1,
			\rate, 0.5,
			\gust, 0.5,
			\out, ~ambiBusJupiter],  ~jupiterGrp, addAction: \addToHead);

		~jupiterT = Synth.new(\fry, [
			\type, 1,
			\rate, ~invrotationJupiter,
			\lowtemp, ~lowtempJupiter,
			\hightemp, ~hightempJupiter,
			\out, ~ambiBusJupiter],  ~jupiterGrp, addAction: \addToHead);

		~encoderJupiter = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusJupiter], target: ~jupiterGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusJupiter, \to, ~ambiMasterBus], target: ~jupiterGrp, addAction: \addToTail);

		//Io
		~ambiBusIo = Bus.audio(s, ~numChannels);

		~ioP = Synth.new(~moonMode, [
			\freq, ~massIo,
			\rate, ~orbitIo,
			\out, ~ambiBusIo],  ~jupiterGrp, addAction: \addToHead);

		~encoderIo = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusIo], target: ~jupiterGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusIo, \to, ~ambiMasterBus], target: ~jupiterGrp, addAction: \addToTail);

		//Europa
		~ambiBusEuropa = Bus.audio(s, ~numChannels);

		~europaP = Synth.new(~moonMode, [
			\freq, ~massEuropa,
			\rate, ~orbitEuropa,
			\out, ~ambiBusEuropa],  ~jupiterGrp, addAction: \addToHead);

		~encoderEuropa = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusEuropa], target: ~jupiterGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusEuropa, \to, ~ambiMasterBus], target: ~jupiterGrp, addAction: \addToTail);

		//Ganymede
		~ambiBusGanymede = Bus.audio(s, ~numChannels);

		~ganymedeP = Synth.new(~moonMode, [
			\freq, ~massGanymede,
			\rate, ~orbitGanymede,
			\out, ~ambiBusGanymede],  ~jupiterGrp, addAction: \addToHead);

		~encoderGanymede = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusGanymede], target: ~jupiterGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusGanymede, \to, ~ambiMasterBus], target: ~jupiterGrp, addAction: \addToTail);

		//Callisto
		~ambiBusCallisto = Bus.audio(s, ~numChannels);

		~callistoP = Synth.new(~moonMode, [
			\freq, ~massCallisto,
			\rate, ~orbitCallisto,
			\out, ~ambiBusCallisto],  ~jupiterGrp, addAction: \addToHead);

		~encoderCallisto = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusCallisto], target: ~jupiterGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusCallisto, \to, ~ambiMasterBus], target: ~jupiterGrp, addAction: \addToTail);

		//Saturn
		~ambiBusSaturn = Bus.audio(s, ~numChannels);


		~saturnP = Synth.new(\surroundmod, [
			\amp, 0.4,
			\type, 1,
			\freq, ~massSaturn,
			\rate, ~invrotationSaturn,
			\isMagnetic, 1,
			\out, ~ambiBusSaturn],  ~saturnGrp, addAction: \addToHead);

		~saturnG = Synth.new(\gravity, [
			\type, 1,
			\freq, ~massSaturn,
			\rate, ~invrotationSaturn/4,
			\gravity, ~gravitySaturn,
			\out, ~ambiBusSaturn],  ~saturnGrp, addAction: \addToHead);

		~saturnW = Synth.new(\wind, [
			\amp, 0.5,
			\type, 1,
			\rate, 0.6,
			\gust, 1.0,
			\out, ~ambiBusSaturn],  ~saturnGrp, addAction: \addToHead);

		~saturnT = Synth.new(\fry, [
			\type, 1,
			\rate, ~invrotationSaturn,
			\lowtemp, ~lowtempSaturn,
			\hightemp, ~hightempSaturn,
			\out, ~ambiBusSaturn],  ~saturnGrp, addAction: \addToHead);

		~saturnR = Synth.new(\rings, [
			\freq, ~massSaturn*4,
			\rate, ~invrotationSaturn,
			\out, ~ambiBusSaturn],  ~saturnGrp, addAction: \addToHead);

		~encoderSaturn = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusSaturn], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusSaturn, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Mimas
		~ambiBusMimas = Bus.audio(s, ~numChannels);

		~mimasP = Synth.new(~moonMode, [
			\freq, ~massMimas,
			\rate, ~orbitMimas,
			\out, ~ambiBusMimas],  ~saturnGrp, addAction: \addToHead);

		~encoderMimas = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusMimas], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusMimas, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Enceladus
		~ambiBusEnceladus = Bus.audio(s, ~numChannels);

		~enceladusP = Synth.new(~moonMode, [
			\freq, ~massEnceladus,
			\rate, ~orbitEnceladus,
			\out, ~ambiBusEnceladus],  ~saturnGrp, addAction: \addToHead);

		~encoderEnceladus = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusEnceladus], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusEnceladus, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Tethys
		~ambiBusTethys = Bus.audio(s, ~numChannels);

		~tethysP = Synth.new(~moonMode, [
			\freq, ~massTethys,
			\rate, ~invrotationSaturn/8,
			\phase, 0.375,
			\out, ~ambiBusTethys],  ~saturnGrp, addAction: \addToHead);

		~encoderTethys = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusTethys], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusTethys, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Dione
		~ambiBusDione = Bus.audio(s, ~numChannels);

		~dioneP = Synth.new(~moonMode, [
			\freq, ~massDione,
			\rate, ~orbitDione,
			\out, ~ambiBusDione],  ~saturnGrp, addAction: \addToHead);

		~encoderDione = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusDione], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusDione, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Rhea
		~ambiBusRhea = Bus.audio(s, ~numChannels);

		~rheaP = Synth.new(~moonMode, [
			\freq, ~massRhea,
			\rate, ~orbitRhea,
			\out, ~ambiBusRhea],  ~saturnGrp, addAction: \addToHead);

		~encoderRhea = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusRhea], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusRhea, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Titan
		~ambiBusTitan = Bus.audio(s, ~numChannels);

		~titanP = Synth.new(~moonMode, [
			\freq, ~massTitan,
			\rate, ~orbitTitan,
			\out, ~ambiBusTitan],  ~saturnGrp, addAction: \addToHead);

		~encoderTitan = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusTitan], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusTitan, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Hyperion
		~ambiBusHyperion = Bus.audio(s, ~numChannels);

		~hyperionP = Synth.new(~moonMode, [
			\freq, ~massHyperion,
			\rate, ~orbitHyperion,
			\out, ~ambiBusHyperion],  ~saturnGrp, addAction: \addToHead);

		~encoderHyperion = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusHyperion], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusHyperion, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);

		//Iapetus
		~ambiBusIapetus = Bus.audio(s, ~numChannels);

		~iapetusP = Synth.new(~moonMode, [
			\freq, ~massIapetus,
			\rate, ~orbitIapetus,
			\out, ~ambiBusIapetus],  ~saturnGrp, addAction: \addToHead);

		~encoderIapetus = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusIapetus], target: ~saturnGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusIapetus, \to, ~ambiMasterBus], target: ~saturnGrp, addAction: \addToTail);


		//Uranus
		~ambiBusUranus = Bus.audio(s, ~numChannels);

		~uranusP = Synth.new(\surroundmod, [
			\amp, 0.4,
			\type, 1,
			\freq, ~massUranus,
			\rate, ~invrotationUranus,
			\isMagnetic, 1,
			\out, ~ambiBusUranus],  ~uranusGrp, addAction: \addToHead);

		~uranusG = Synth.new(\gravity, [
			\type, 1,
			\freq, ~massUranus,
			\rate, ~invrotationUranus/4,
			\gravity, ~gravityUranus,
			\out, ~ambiBusUranus],  ~uranusGrp, addAction: \addToHead);

		~uranusW = Synth.new(\wind, [
			\amp, 0.5,
			\type, 1,
			\rate, 0.5,
			\gust, 0.7,
			\out, ~ambiBusUranus],  ~uranusGrp, addAction: \addToHead);
		~uranusT = Synth.new(\fry, [
			\type, 1,
			\rate, ~invrotationUranus,
			\lowtemp, ~lowtempUranus,
			\hightemp, ~hightempUranus,
			\out, ~ambiBusUranus],  ~uranusGrp, addAction: \addToHead);

		~encoderUranus = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusUranus], target: ~uranusGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusUranus, \to, ~ambiMasterBus], target: ~uranusGrp, addAction: \addToTail);

		//Ariel
		~ambiBusAriel = Bus.audio(s, ~numChannels);

		~arielP = Synth.new(~moonMode, [
			\freq, ~massAriel,
			\rate, ~orbitAriel,
			\out, ~ambiBusAriel],  ~uranusGrp, addAction: \addToHead);

		~encoderAriel = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusAriel], target: ~uranusGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusAriel, \to, ~ambiMasterBus], target: ~uranusGrp, addAction: \addToTail);

		//Miranda
		~ambiBusMiranda = Bus.audio(s, ~numChannels);

		~mirandaP = Synth.new(~moonMode, [
			\freq, ~massMiranda,
			\rate, ~orbitMiranda,
			\out, ~ambiBusMiranda],  ~uranusGrp, addAction: \addToHead);

		~encoderMiranda = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusMiranda], target: ~uranusGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusMiranda, \to, ~ambiMasterBus], target: ~uranusGrp, addAction: \addToTail);

		//Oberon
		~ambiBusOberon = Bus.audio(s, ~numChannels);

		~oberonP = Synth.new(~moonMode, [
			\freq, ~massOberon,
			\rate, ~orbitOberon,
			\out, ~ambiBusOberon],  ~uranusGrp, addAction: \addToHead);

		~encoderOberon = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusOberon], target: ~uranusGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusOberon, \to, ~ambiMasterBus], target: ~uranusGrp, addAction: \addToTail);

		//Titania
		~ambiBusTitania = Bus.audio(s, ~numChannels);

		~titaniaP = Synth.new(~moonMode, [
			\freq, ~massTitania,
			\rate, ~orbitTitania,
			\out, ~ambiBusTitania],  ~uranusGrp, addAction: \addToHead);

		~encoderTitania = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusTitania], target: ~uranusGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusTitania, \to, ~ambiMasterBus], target: ~uranusGrp, addAction: \addToTail);

		//Umbriel
		~ambiBusUmbriel = Bus.audio(s, ~numChannels);

		~umbrielP = Synth.new(~moonMode, [
			\freq, ~massUmbriel,
			\rate, ~orbitUmbriel,
			\out, ~ambiBusUmbriel],  ~uranusGrp, addAction: \addToHead);

		~encoderUmbriel = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusUmbriel], target: ~uranusGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusUmbriel, \to, ~ambiMasterBus], target: ~uranusGrp, addAction: \addToTail);

		//Neptune
		~ambiBusNeptune = Bus.audio(s, ~numChannels);

		~neptuneP = Synth.new(\surroundmod, [
			\amp, 0.4,
			\type, 1,
			\freq, ~massNeptune,
			\rate, ~invrotationNeptune,
			\isMagnetic, 1,
			\out, ~ambiBusNeptune],  ~neptuneGrp, addAction: \addToHead);

		~neptuneG = Synth.new(\gravity, [
			\type, 1,
			\freq, ~massNeptune,
			\rate, ~invrotationNeptune/4,
			\gravity, ~gravityNeptune,
			\out, ~ambiBusNeptune],  ~neptuneGrp, addAction: \addToHead);

		~neptuneW = Synth.new(\wind, [
			\amp, 0.5,
			\type, 1,
			\rate, 0.6,
			\gust, 2,
			\out, ~ambiBusNeptune],  ~neptuneGrp, addAction: \addToHead);

		~neptuneT = Synth.new(\fry, [
			\rate, ~invrotationNeptune,
			\lowtemp, ~lowtempNeptune,
			\hightemp, ~hightempNeptune,
			\out, ~ambiBusNeptune],  ~neptuneGrp, addAction: \addToHead);

		~encoderNeptune = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusNeptune], target: ~neptuneGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusNeptune, \to, ~ambiMasterBus], target: ~neptuneGrp, addAction: \addToTail);

		//Triton
		~ambiBusTriton = Bus.audio(s, ~numChannels);

		~tritonP = Synth.new(~moonMode, [
			\freq, ~massTriton,
			\rate, ~orbitTriton,
			\out, ~ambiBusTriton],  ~neptuneGrp, addAction: \addToHead);

		~encoderTriton = VSTPluginController(Synth(\stereoEncoder, [\bus, ~ambiBusTriton], target: ~neptuneGrp, addAction: \addToTail)).open("StereoEncoder");
		Synth(\ambiThrow, [\from, ~ambiBusTriton, \to, ~ambiMasterBus], target: ~neptuneGrp, addAction: \addToTail);


		//SOLAR SYSTEM VIEW

		~solarMode = \percSolar;
		//~solarMode = \sinamppoof;

		//Mercury Solar
		~mercuryPoof = Synth.new(\sinamppoof, [
			\freq, ~massMercury,
			\rate, ~invrotationMercury,
			\out, ~ambiBusMercury], ~mercuryGrp, addAction: \addToHead);

		//Venus Solar
		~venusPoof = Synth.new(\sinamppoof, [
			\freq, ~massVenus,
			\rate, ~invrotationVenus,
			\out, ~ambiBusVenus], ~venusGrp, addAction: \addToHead);

		//Earth Solar
		~earthPoof = Synth.new(~solarMode, [
			\freq, ~massEarth,
			\rate, ~invrotationEarth,
			\out, ~ambiBusEarth], ~earthGrp, addAction: \addToHead);

		//Mars Solar
		~marsPoof = Synth.new(~solarMode, [
			\freq, ~massMars,
			\rate, ~invrotationMars,
			\out, ~ambiBusMars], ~marsGrp, addAction: \addToHead);

		//Jupiter Solar
		~jupiterPoof = Synth.new(~solarMode, [
			\freq, ~massJupiter,
			\rate, ~invrotationJupiter,
			\out, ~ambiBusJupiter], ~jupiterGrp, addAction: \addToHead);

		//Saturn Solar
		~saturnPoof = Synth.new(~solarMode, [
			\freq, ~massSaturn,
			\rate, ~invrotationSaturn,
			\out, ~ambiBusSaturn], ~saturnGrp, addAction: \addToHead);

		//Uranus Solar
		~uranusPoof = Synth.new(~solarMode, [
			\freq, ~massUranus,
			\rate, ~invrotationUranus,
			\out, ~ambiBusUranus], ~uranusGrp, addAction: \addToHead);

		//Neptune Solar
		~neptunePoof = Synth.new(~solarMode, [
			\freq, ~massNeptune,
			\rate, ~invrotationNeptune,
			\out, ~ambiBusNeptune], ~neptuneGrp, addAction: \addToHead);

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

	var loadOSC = {

		~compareGate = 0;
		~minDist = 3;

		//Horizontal mode
		~modeElSign = 1;
		~modeElPhase = 0.5;

		//Circular mode (change manually)
		// ~modeElSign = -1;
		//~modeElPhase = 0.75;

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

		OSCdef.new(\Time,
			{
				arg msg;

				//("time:" + msg[1]).postln;

				~planetaryGrp.set(\time, abs(msg[1]));
				~solarGrp.set(\time, abs(msg[1]));

		},'/Time');


		OSCdef.new(\mercury,
			{
				arg msg;
				var amp = (-5.4e-7*1.1*msg[1]).exp*1.05;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~mercuryP.set(\gate, 1);},
						{~mercuryP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~mercuryG.set(\gate, 1);},
						{~mercuryG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~mercuryT.set(\gate, 1);},
						{~mercuryT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~mercuryW.set(\gate, 1);},
						{~mercuryW.set(\gate, 0);});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-5.4e-7*1.1*msg[1]).exp*1.05});

					~mercuryGrp.set(\amp2, amp);},
					{~mercuryGrp.set(\amp2, 1.0);});

				~encoderMercury.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderMercury.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

		},'/Mercury');

		OSCdef.new(\venus,
			{
				arg msg;
				var amp = (-5.4e-7*1.1*msg[1]).exp*1.05;
				var azimuth, elevation;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~venusP.set(\gate, 1);},
						{~venusP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~venusG.set(\gate, 1);},
						{~venusG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~venusT.set(\gate, 1);},
						{~venusT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~venusW.set(\gate, 1);},
						{~venusW.set(\gate, 0);});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-5.4e-7*1.1*msg[1]).exp*1.05});

					~venusGrp.set(\amp2, amp);},
					{~venusGrp.set(\amp2, 1.0);});

				~encoderVenus.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderVenus.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

		},'/Venus');

		OSCdef.new(\earth,
			{
				arg msg;

				var ampMoon;
				var amp = (-5.4e-7*1.1*msg[1]).exp*1.05;
				var azimuth, elevation;

				("Earth:" + msg).postln;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~earthP.set(\gate, 1);},
						{~earthP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~earthG.set(\gate, 1);},
						{~earthG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~earthT.set(\gate, 1);},
						{~earthT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~earthW.set(\gate, 1);},
						{~earthW.set(\gate, 0);});

					if(msg[4][4]==1,
						{~moonP.set(\gate, 1);},
						{~moonP.set(\gate, 0);});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-5.4e-7*1.1*msg[1]).exp*1.05});

					~earthGrp.set(\amp2, amp);},
					{~earthGrp.set(\amp2, 1.0);});

				ampMoon = -4*(amp - 0.5).pow(2)+1;
				~earthGrp.set(\ampMoon, ampMoon);

				~encoderEarth.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderEarth.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

				~encoderMoon.do(_.set(6, (msg[6]/(2*pi))+0.5));
				~encoderMoon.do(_.set(7, ~modeElSign*(msg[7]/(2*pi))+~modeElPhase));

		},'/Earth');

		OSCdef.new(\mars,
			{
				arg msg;
				var ampMoon;
				var amp = (-9.5e-6*0.5*msg[1]).exp*1.1;
				var azimuth, elevation;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~marsP.set(\gate, 1);},
						{~marsP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~marsG.set(\gate, 1);},
						{~marsG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~marsT.set(\gate, 1);},
						{~marsT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~marsW.set(\gate, 1);},
						{~marsW.set(\gate, 0);});

					if(msg[4][4]==1,
						{~phobosP.set(\gate, 1);
							~deimosP.set(\gate, 1);
						},
						{~phobosP.set(\gate, 0);
							~deimosP.set(\gate, 0);});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-9.5e-6*0.5*msg[1]).exp*1.1});

					if(0>amp,
						{amp = 0.0},
						{amp = (-9.5e-6*0.5*msg[1]).exp*1.1});

					~marsGrp.set(\amp2, amp);},
					{~marsGrp.set(\amp2, 1.0);});

				ampMoon = -4*(amp - 0.5).pow(2)+1;
				~marsGrp.set(\ampMoon, ampMoon);

				~encoderMars.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderMars.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

				~encoderPhobos.do(_.set(6, (msg[6]/(2*pi))+0.5));
				~encoderPhobos.do(_.set(7, ~modeElSign*(msg[7]/(2*pi))+~modeElPhase));

				~encoderDeimos.do(_.set(6, (msg[9]/(2*pi))+0.5));
				~encoderDeimos.do(_.set(7, ~modeElSign*(msg[10]/(2*pi))+~modeElPhase));

		},'/Mars');

		OSCdef.new(\jupiter,
			{
				arg msg;
				var ampMoon;
				var amp = (-1.2e-7*0.9*msg[1]).exp*1.05;

				("Jupiter:" + msg).postln;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~jupiterP.set(\gate, 1);},
						{~jupiterP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~jupiterG.set(\gate, 1);},
						{~jupiterG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~jupiterT.set(\gate, 1);},
						{~jupiterT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~jupiterW.set(\gate, 1);},
						{~jupiterW.set(\gate, 0);});

					if(msg[4][4]==1,
						{~ioP.set(\gate, 1);
							~europaP.set(\gate, 1);
							~ganymedeP.set(\gate, 1);
							~callistoP.set(\gate, 1);
						},
						{~ioP.set(\gate, 0);
							~europaP.set(\gate, 0);
							~ganymedeP.set(\gate, 0);
							~callistoP.set(\gate, 0);});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-1.2e-7*0.9*msg[1]).exp*1.05});

					if(0>amp,
						{amp = 0.0},
						{amp = (-1.2e-7*0.9*msg[1]).exp*1.05});

					~jupiterGrp.set(\amp2, amp);},
					{~jupiterGrp.set(\amp2, 1.0);});

				ampMoon = -4*(amp - 0.5).pow(2)+1;
				~jupiterGrp.set(\ampMoon, ampMoon);

				~encoderJupiter.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderJupiter.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

				~encoderIo.do(_.set(6, (msg[6]/(2*pi))+0.5));
				~encoderIo.do(_.set(7, ~modeElSign*(msg[7]/(2*pi))+~modeElPhase));

				~encoderEuropa.do(_.set(6, (msg[9]/(2*pi))+0.5));
				~encoderEuropa.do(_.set(7, ~modeElSign*(msg[10]/(2*pi))+~modeElPhase));

				~encoderGanymede.do(_.set(6, (msg[12]/(2*pi))+0.5));
				~encoderGanymede.do(_.set(7, ~modeElSign*(msg[13]/(2*pi))+~modeElPhase));

				~encoderCallisto.do(_.set(6, (msg[15]/(2*pi))+0.5));
				~encoderCallisto.do(_.set(7, ~modeElSign*(msg[16]/(2*pi))+~modeElPhase));

		},'/Jupiter');


		OSCdef.new(\saturn,
			{
				arg msg;
				var ampMoon;
				var amp = (-1.2e-7*0.9*msg[1]).exp*1.05;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~saturnP.set(\gate, 1);},
						{~saturnP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~saturnG.set(\gate, 1);},
						{~saturnG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~saturnT.set(\gate, 1);},
						{~saturnT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~saturnW.set(\gate, 1);},
						{~saturnW.set(\gate, 0);});

					if(msg[4][4]==1,
						{~mimasP.set(\gate, 1);
							~enceladusP.set(\gate, 1);
							~tethysP.set(\gate, 1);
							~dioneP.set(\gate, 1);
							~rheaP.set(\gate, 1);
							~titanP.set(\gate, 1);
							~hyperionP.set(\gate, 1);
							~iapetusP.set(\gate, 1);
						},
						{~mimasP.set(\gate, 0);
							~enceladusP.set(\gate, 0);
							~tethysP.set(\gate, 0);
							~dioneP.set(\gate, 0);
							~rheaP.set(\gate, 0);
							~titanP.set(\gate, 0);
							~hyperionP.set(\gate, 0);
							~iapetusP.set(\gate, 0);
					});

					if(msg[4][5]==1,
						{~saturnR.set(\gate, 1);},
						{~saturnR.set(\gate, 0);});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-1.2e-7*0.9*msg[1]).exp*1.05});

					~saturnGrp.set(\amp2, amp);},
					{~saturnGrp.set(\amp2, 1.0);});

				ampMoon = -4*(amp - 0.5).pow(2)+1;
				~saturnGrp.set(\ampMoon, ampMoon);

				~encoderSaturn.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderSaturn.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

				~encoderDione.do(_.set(6, (msg[6]/(2*pi))+0.5));
				~encoderDione.do(_.set(7, ~modeElSign*(msg[7]/(2*pi))+~modeElPhase));

				~encoderEnceladus.do(_.set(6, (msg[9]/(2*pi))+0.5));
				~encoderEnceladus.do(_.set(7, ~modeElSign*(msg[10]/(2*pi))+~modeElPhase));

				~encoderHyperion.do(_.set(6, (msg[12]/(2*pi))+0.5));
				~encoderHyperion.do(_.set(7, ~modeElSign*(msg[13]/(2*pi))+~modeElPhase));

				~encoderIapetus.do(_.set(6, (msg[15]/(2*pi))+0.5));
				~encoderIapetus.do(_.set(7, ~modeElSign*(msg[16]/(2*pi))+~modeElPhase));

				~encoderMimas.do(_.set(6, (msg[18]/(2*pi))+0.5));
				~encoderMimas.do(_.set(7, ~modeElSign*(msg[19]/(2*pi))+~modeElPhase));

				~encoderRhea.do(_.set(6, (msg[21]/(2*pi))+0.5));
				~encoderRhea.do(_.set(7, ~modeElSign*(msg[22]/(2*pi))+~modeElPhase));

				~encoderTethys.do(_.set(6, (msg[24]/(2*pi))+0.5));
				~encoderTethys.do(_.set(7, ~modeElSign*(msg[25]/(2*pi))+~modeElPhase));

				~encoderTitan.do(_.set(6, (msg[27]/(2*pi))+0.5));
				~encoderTitan.do(_.set(7, ~modeElSign*(msg[28]/(2*pi))+~modeElPhase));

		},'/Saturn');




		OSCdef.new(\uranus,
			{
				arg msg;
				var ampMoon;
				var amp = (-6.7e-7*0.7*msg[1]).exp*1.1;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~uranusP.set(\gate, 1);},
						{~uranusP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~uranusG.set(\gate, 1);},
						{~uranusG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~uranusT.set(\gate, 1);},
						{~uranusT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~uranusW.set(\gate, 1);},
						{~uranusW.set(\gate, 0);});

					if(msg[4][4]==1,
						{~arielP.set(\gate, 1);
							~mirandaP.set(\gate, 1);
							~oberonP.set(\gate, 1);
							~titaniaP.set(\gate, 1);
							~umbrielP.set(\gate, 1);
						},
						{~arielP.set(\gate, 0);
							~mirandaP.set(\gate, 0);
							~oberonP.set(\gate, 0);
							~titaniaP.set(\gate, 0);
							~umbrielP.set(\gate, 0);
					});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-6.7e-7*0.7*msg[1]).exp*1.1});

					~uranusGrp.set(\amp2, amp);},
					{~uranusGrp.set(\amp2, 1.0);});

				ampMoon = -4*(amp - 0.5).pow(2)+1;
				~uranusGrp.set(\ampMoon, ampMoon);

				~encoderUranus.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderUranus.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

				~encoderAriel.do(_.set(6, (msg[6]/(2*pi))+0.5));
				~encoderAriel.do(_.set(7, ~modeElSign*(msg[7]/(2*pi))+~modeElPhase));

				~encoderMiranda.do(_.set(6, (msg[9]/(2*pi))+0.5));
				~encoderMiranda.do(_.set(7, ~modeElSign*(msg[10]/(2*pi))+~modeElPhase));

				~encoderOberon.do(_.set(6, (msg[12]/(2*pi))+0.5));
				~encoderOberon.do(_.set(7, ~modeElSign*(msg[13]/(2*pi))+~modeElPhase));

				~encoderTitania.do(_.set(6, (msg[15]/(2*pi))+0.5));
				~encoderTitania.do(_.set(7, ~modeElSign*(msg[16]/(2*pi))+~modeElPhase));

				~encoderUmbriel.do(_.set(6, (msg[18]/(2*pi))+0.5));
				~encoderUmbriel.do(_.set(7, ~modeElSign*(msg[19]/(2*pi))+~modeElPhase));

		},'/Uranus');

		OSCdef.new(\neptune,
			{
				arg msg;
				var ampMoon;
				var amp = (-7.7e-7*0.8*msg[1]).exp*1.15;

				if(~compareGate == 0,
					{if(msg[4][0]==1,
						{~neptuneP.set(\gate, 1);},
						{~neptuneP.set(\gate, 0);});

					if(msg[4][1]==1,
						{~neptuneG.set(\gate, 1);},
						{~neptuneG.set(\gate, 0);});

					if(msg[4][2]==1,
						{~neptuneT.set(\gate, 1);},
						{~neptuneT.set(\gate, 0);});

					if(msg[4][3]==1,
						{~neptuneW.set(\gate, 1);},
						{~neptuneW.set(\gate, 0);});

					if(msg[4][4]==1,
						{~tritonP.set(\gate, 1);},
						{~tritonP.set(\gate, 0);});

					if(amp>1.0,
						{amp = 1.0},
						{amp = (-7.7e-7*0.8*msg[1]).exp*1.15});

					~neptuneGrp.set(\amp2, amp);},
					{~neptuneGrp.set(\amp2, 1.0);});

				ampMoon = -4*(amp - 0.5).pow(2)+1;
				~neptuneGrp.set(\ampMoon, ampMoon);

				~encoderNeptune.do(_.set(6, (msg[2]/(2*pi))+0.5));
				~encoderNeptune.do(_.set(7, ~modeElSign*(msg[3]/(2*pi))+~modeElPhase));

				~encoderTriton.do(_.set(6, (msg[6]/(2*pi))+0.5));
				~encoderTriton.do(_.set(7, ~modeElSign*(msg[7]/(2*pi))+~modeElPhase));

		},'/Neptune');

		OSCdef.new(\Overview,
			{
				arg msg;

				//msg[1] = array of gates
				//msg[1][0] = all gate
				//msg[1][1] = mercury
				//msg[1][2] = venus
				//msg[1][3] = earth
				//msg[1][4] = mars
				//...

				("Solar:" + msg).postln;

				if((msg[1][0] != 0 or: msg[1][1] != 0 or: msg[1][2] != 0 or: msg[1][3] != 0 or: msg[1][4] != 0 or: msg[1][5] != 0 or: msg[1][6] != 0 or: msg[1][7] != 0),
					{~compareGate=1;},
					{~compareGate=0;});

				("~compareGate:" + ~compareGate).postln;

				if(msg[1][0] == 1,
					{~mercuryPoof.set(\gate, 1);},
					{~mercuryPoof.set(\gate, 0);});

				if(msg[1][1] == 1,
					{~venusPoof.set(\gate, 1);},
					{~venusPoof.set(\gate, 0);});

				if(msg[1][2] == 1,
					{~earthPoof.set(\gate, 1);},
					{~earthPoof.set(\gate, 0);});

				if(msg[1][3] == 1,
					{~marsPoof.set(\gate, 1);},
					{~marsPoof.set(\gate, 0);});

				if(msg[1][4] == 1,
					{~jupiterPoof.set(\gate, 1);},
					{~jupiterPoof.set(\gate, 0);});

				if(msg[1][5] == 1,
					{~saturnPoof.set(\gate, 1);},
					{~saturnPoof.set(\gate, 0);});

				if(msg[1][6] == 1,
					{~uranusPoof.set(\gate, 1);},
					{~uranusPoof.set(\gate, 0);});

				if(msg[1][7] == 1,
					{~neptunePoof.set(\gate, 1);},
					{~neptunePoof.set(\gate, 0);});

		},'/Overview');

		OSCdef.new(\compare,
			{
				arg msg;
				("compare settings:" + msg[3]).postln;

				//msg[1] = first planet to compare
				//msg[2] = second planet to compare
				//msg[3] = [
				//msg[3][0] = size/day
				//msg[3][1] = gravity
				//msg[3][2] = temperature
				//msg[3][3] = atmosphere
				//msg[3][4] = moons
				//msg[3][5] = rings

				("Compare:" + msg).postln;


				("First: " + msg[1]).postln;
				("Second: " + msg[2]).postln;

				if(msg[1] != 0 or: msg[2] != 0,
					{~compareGate=1;},
					{~compareGate=0;});


				if(msg[1] == 1 or: {msg[2] == 1},
					{if(msg[3][0]==1,
						{~mercuryP.set(\gate, 1);},
						{~mercuryP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~mercuryG.set(\gate, 1);},
						{~mercuryG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~mercuryT.set(\gate, 1);},
						{~mercuryT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~mercuryW.set(\gate, 1);},
						{~mercuryW.set(\gate, 0);});},

					{~mercuryGrp.set(\gate, 0);});

				if(msg[1] == 2 or: {msg[2] == 2},
					{if(msg[3][0]==1,
						{~venusP.set(\gate, 1);},
						{~venusP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~venusG.set(\gate, 1);},
						{~venusG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~venusT.set(\gate, 1);},
						{~venusT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~venusW.set(\gate, 1);},
						{~venusW.set(\gate, 0);});},

					{~venusGrp.set(\gate, 0);});

				if(msg[1] == 3 or: {msg[2] == 3},
					{if(msg[3][0]==1,
						{~earthP.set(\gate, 1);},
						{~earthP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~earthG.set(\gate, 1);},
						{~earthG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~earthT.set(\gate, 1);},
						{~earthT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~earthW.set(\gate, 1);},
						{~earthW.set(\gate, 0);});

					if(msg[3][4]==1,
						{~moonP.set(\gate, 1);},
						{~moonP.set(\gate, 0);});},

					{~earthGrp.set(\gate, 0);});

				if(msg[1] == 4 or: {msg[2] == 4},
					{if(msg[3][0]==1,
						{~marsP.set(\gate, 1);},
						{~marsP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~marsG.set(\gate, 1);},
						{~marsG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~marsT.set(\gate, 1);},
						{~marsT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~marsW.set(\gate, 1);},
						{~marsW.set(\gate, 0);});

					if(msg[3][4]==1,
						{~phobosP.set(\gate, 1);
							~deimosP.set(\gate, 1);
						},
						{~phobosP.set(\gate, 0);
							~deimosP.set(\gate, 0);});},

					{~marsGrp.set(\gate, 0);});

				if(msg[1] == 5 or: {msg[2] == 5},
					{if(msg[3][0]==1,
						{~jupiterP.set(\gate, 1);},
						{~jupiterP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~jupiterG.set(\gate, 1);},
						{~jupiterG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~jupiterT.set(\gate, 1);},
						{~jupiterT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~jupiterW.set(\gate, 1);},
						{~jupiterW.set(\gate, 0);});

					if(msg[3][4]==1,
						{~ioP.set(\gate, 1);
							~europaP.set(\gate, 1);
							~ganymedeP.set(\gate, 1);
							~callistoP.set(\gate, 1);
						},
						{~ioP.set(\gate, 0);
							~europaP.set(\gate, 0);
							~ganymedeP.set(\gate, 0);
							~callistoP.set(\gate, 0);});},

					{~jupiterGrp.set(\gate, 0);});


				if(msg[1] == 6 or: {msg[2] == 6},
					{if(msg[3][0]==1,
						{~saturnP.set(\gate, 1);},
						{~saturnP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~saturnG.set(\gate, 1);},
						{~saturnG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~saturnT.set(\gate, 1);},
						{~saturnT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~saturnW.set(\gate, 1);},
						{~saturnW.set(\gate, 0);});

					if(msg[3][4]==1,
						{~mimasP.set(\gate, 1);
							~enceladusP.set(\gate, 1);
							~tethysP.set(\gate, 1);
							~dioneP.set(\gate, 1);
							~rheaP.set(\gate, 1);
							~titanP.set(\gate, 1);
							~hyperionP.set(\gate, 1);
							~iapetusP.set(\gate, 1);
						},
						{~mimasP.set(\gate, 0);
							~enceladusP.set(\gate, 0);
							~tethysP.set(\gate, 0);
							~dioneP.set(\gate, 0);
							~rheaP.set(\gate, 0);
							~titanP.set(\gate, 0);
							~hyperionP.set(\gate, 0);
							~iapetusP.set(\gate, 0);
					});

					if(msg[3][5]==1,
						{~saturnR.set(\gate, 1);},
						{~saturnR.set(\gate, 0);});},
					{~saturnGrp.set(\gate, 0);});


				if(msg[1] == 7 or: {msg[2] == 7},
					{if(msg[3][0]==1,
						{~uranusP.set(\gate, 1);},
						{~uranusP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~uranusG.set(\gate, 1);},
						{~uranusG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~uranusT.set(\gate, 1);},
						{~uranusT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~uranusW.set(\gate, 1);},
						{~uranusW.set(\gate, 0);});

					if(msg[3][4]==1,
						{~arielP.set(\gate, 1);
							~mirandaP.set(\gate, 1);
							~oberonP.set(\gate, 1);
							~titaniaP.set(\gate, 1);
							~umbrielP.set(\gate, 1);
						},
						{~arielP.set(\gate, 0);
							~mirandaP.set(\gate, 0);
							~oberonP.set(\gate, 0);
							~titaniaP.set(\gate, 0);
							~umbrielP.set(\gate, 0);
					});},
					{~uranusGrp.set(\gate, 0);});

				if(msg[1] == 8 or: {msg[2] == 8},
					{if(msg[3][0]==1,
						{~neptuneP.set(\gate, 1);},
						{~neptuneP.set(\gate, 0);});

					if(msg[3][1]==1,
						{~neptuneG.set(\gate, 1);},
						{~neptuneG.set(\gate, 0);});

					if(msg[3][2]==1,
						{~neptuneT.set(\gate, 1);},
						{~neptuneT.set(\gate, 0);});

					if(msg[3][3]==1,
						{~neptuneW.set(\gate, 1);},
						{~neptuneW.set(\gate, 0);});

					if(msg[3][4]==1,
						{~tritonP.set(\gate, 1);},
						{~tritonP.set(\gate, 0);});},
					{~neptuneGrp.set(\gate, 0);});

		},'/Compare');

	};

	// Start routine
	~startRoutine = Routine({

		loadData.value;
		"--Loaded the data--".postln;
		0.1.wait;
		loadSynthDefs.value;
		"--Loaded the synths--".postln;
		0.2.wait;
		loadAmbiGroups.value;
		"--Loaded the ambisonics--".postln;
		0.1.wait;
		initiateSynths.value;
		"--Initiated the synths--".postln;
		0.1.wait;
		loadDecoder.value;
		"--Loaded the decoder--".postln;
		0.1.wait;
		loadOSC.value;
		"--Loaded the OSCdefs--".postln;

		"---------- Sonification is ready ----------".postln;

	});

	~startRoutine.play(AppClock);

}
)