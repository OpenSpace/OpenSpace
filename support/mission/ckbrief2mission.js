var USAGE_TEXT = '\n\
USAGE:\n\
node spice2mission.js <input directory> [,<ckbrief arguments...>] \n\
\n\
PURPOSE:\n\
This script takes a root directory containing SPICE files (*.bc) \n\
and prints lua code representing the contents for a .mission-file\n\
to std out. The directory structure is used to create the hierarchy\n\
of mission phases.\
';

const exec = require('child_process').exec;

var KEY_START = 'start';
var KEY_END = 'end';
var FILE_START = 'Summary for: ';
var TIME_START = 'Begin ET: ';
var SPACES = '                                                                   ';

var inputDir = process.argv[2];
var additionalArguments = process.argv.splice(3);


run();

function run(){
	if(process.argv.length == 2){
		console.log('Error: expected at least one (1) argument');
		console.log(USAGE_TEXT);
		return;
	}

	execute('find ' + inputDir + ' -type f -name *.bc*', (files) => {
		files = files.split('\n').join(' ');
		//console.log(files);
		var cmd = "ckbrief.exe " + additionalArguments.join(" ") + " " + files + '-g';
		execute(cmd, handleCkbriefOutput);
	});
}

function onSuccess(cb){
	return (error, stdout, stderr) => {
		if(error) return console.error('exec error: ' + error);
		if(stderr) return console.error(stderr);
		cb(stdout);
	}
}

function execute(cmd, cb){
	//console.log('executing: ' + cmd);
	exec(cmd, onSuccess(cb));
}

/**
* Handles the output from the shell command
* $ find <inputDit> -type f -name *.bc | xargs ckbrief.exe <additional arguments...> -g
*/
function handleCkbriefOutput(output){
	var mission = {};
	var currentLeaf = null;
	var lines = output.split('\n');
	for (var i = 0; i < lines.length; i++) {
		var line = lines[i].trim();
		if(line.startsWith(FILE_START)){
			var pathToFile = line.substr(FILE_START.length);
			//console.log(currentLeaf);
			currentLeaf = addPathToTree(mission, pathToFile);
		}
		else if(line.startsWith(TIME_START)){
			var startTime = line.substr(TIME_START.length + 0, TIME_START.length + 14);
			var endTime = line.substr(TIME_START.length + 34).trim();
			if(currentLeaf[KEY_START] === undefined) {
				currentLeaf[KEY_START] = startTime;
			}
			currentLeaf[KEY_END] = endTime;
		}
	}
	//console.log(JSON.stringify(mission, null, 2));

	var luaStr = toLuaTable(mission, 0);
	console.log(luaStr);
}

function toLuaTable(mission, d){
	if(d === undefined) d = 0;
	if(mission === undefined) return '';
	//if(d > 2) return '';
	var str = d ? "" : "return ";

	var indent = SPACES.substr(0, d*2);
	for (var phaseName in mission) {
		if (mission.hasOwnProperty(phaseName)) {
			var phase = mission[phaseName];	
			str += '\n' + indent + '{ Name = "' + phaseName + '", ';
			if(phase[KEY_START] !== undefined && phase[KEY_END] !== undefined){
				str += 'TimeRange = { Start = "' + phase[KEY_START] + '", End = "' + phase[KEY_END] + '" }';
			}
			else {
				str += 'Phases = {' + toLuaTable(phase, d + 1) + '\n' + indent + '}';
			}
			str += '},';
		}
	}

	// strip out last ','.
	return str.substr(0, str.length-1);
}

function addPathToTree(obj, components){
	if(typeof components === 'string') {
		components = components.split('/');
	}
	
	var o = obj;
	var key;
	for (var i = 0; i < components.length; i++) {
		key = components[i];
		if(o[key] === undefined) o[key] = {};
		o = o[key];
	}

	return o; // returning the leaf component
}