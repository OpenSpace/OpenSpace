var fs = require('fs');
const exec = require('child_process').exec;

var REQUIRED_ARGUMENTS = 2;
var PATH_TO_OPENSPACE = process.argv[2];
var NUM_NODES = +process.argv[3];

var PATH_TO_GENERATED_CONF = __dirname + '/generated_sgct_config.xml';

// Optional params
var FIRM_SYNC = false;
var WINDOW_SIZE = {x: 640, y: 360};
var WINDOW_COLUMNS = 2;

// Parse optional arguments
function argIndexOf(param){
	return process.argv.indexOf(param, REQUIRED_ARGUMENTS+2);
}

var paramIndex = -1;
if( (paramIndex = argIndexOf("--firmsync")) != -1){
	FIRM_SYNC = true;
}
if( (paramIndex = argIndexOf("-size")) != -1){
	WINDOW_SIZE = { x: +process.argv[paramIndex + 1], y: +process.argv[paramIndex + 2] };
}
if( (paramIndex = argIndexOf("-cols")) != -1){
	WINDOW_COLUMNS = +process.argv[paramIndex + 1];
}


run();

function run(){
	if(process.argv.length < REQUIRED_ARGUMENTS + 2){
		console.log("Expected at least " + REQUIRED_ARGUMENTS + " arguments:");
		console.log("<path/to/openspace-binary> <# nodes to generate>");
		return;
	}
	if(!PATH_TO_OPENSPACE) return new Error("bad path to openspace!");
	if(!NUM_NODES) return new Error("bad number of nodes!");

	var s = generateConfigSrcForN_nodes();

	fs.writeFile(PATH_TO_GENERATED_CONF, s, function(err) {
	    if(err) {
	        return console.log(err);
	    }

	    console.log("SGCT config generated!");
	    execChildProcesses();
	}); 
}

function execChildProcesses(){
	for (var i = 0; i < NUM_NODES; i++) {
		var cmd = PATH_TO_OPENSPACE;
		cmd += " -sgct " + PATH_TO_GENERATED_CONF
		cmd += " -local " + i;
		if(i > 0){
			cmd += " --slave";
		}

		console.log(cmd);
		exec(cmd, function(err, stdout, stderr){
  			if (err) {
    			console.error(err);
    			return;		
    		}
    		console.log(stdout);
    		console.error(stderr);
    	});
	}	
}


function generateConfigSrcForN_nodes(){
	var s = "";
	s += '\
<?xml version="1.0" ?>\n\
<Cluster masterAddress="127.0.0.1" firmSync="' + FIRM_SYNC + '">';
	
	for (var i = 0; i < NUM_NODES; i++) {
		s += generateNode(i);
	}
	
	s += '\n\
	<User eyeSeparation="0.065"> \n\
		<Pos x="0.0" y="0.0" z="4.0" /> \n\
	</User>\n\
</Cluster>';
	
	return s;
}

function generateNode(i){
	var x = 10 + (i%WINDOW_COLUMNS) * (WINDOW_SIZE.x + 15);;
	var y = 30 + Math.floor(i/WINDOW_COLUMNS) * (WINDOW_SIZE.y + 40);
	return '\n\
	<Node address="127.0.0.' + (i+1) + '" port="2040' + (i+1) + '" swapLock="false">\n\
		<Window fullScreen="false">\n\
			<Pos x="'+ x +'" y="' + y + '" />\n\
			<!-- 16:9 aspect ratio -->\n\
			<Size x="' + WINDOW_SIZE.x + '" y="' + WINDOW_SIZE.y + '" />\n\
			<Viewport>\n\
				<Pos x="0.0" y="0.0" />\n\
				<Size x="1.0" y="1.0" />\n\
				<Viewplane>\n\
					<!-- Lower left -->\n\
					<Pos x="-1.778" y="-1.0" z="0.0" />\n\
					<!-- Upper left -->\n\
					<Pos x="-1.778" y="1.0" z="0.0" />\n\
					<!-- Upper right -->\n\
					<Pos x="1.778" y="1.0" z="0.0" />\n\
				</Viewplane>\n\
			</Viewport>\n\
		</Window>\n\
	</Node>';
}
