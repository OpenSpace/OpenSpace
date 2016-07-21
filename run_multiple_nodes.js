var fs = require('fs');
const exec = require('child_process').exec;


var path = process.argv[2];
var confPath = process.argv[3];
var nNodes = +process.argv[4];


run();

function run(){
	if(process.argv.length !== 5){
		console.log("Error! Expected 3 arguments:");
		console.log("<path/to/openspace-binary> <path/to/output-sgct-config> <# nodes to generate>");
		return;
	}
	if(!path) return new Error("bad path!");
	if(!nNodes) return new Error("bad nNodes!");
	if(!confPath) return new Error("bad confPath!");

	var s = generateConfigSrcForN_nodes();

	fs.writeFile(confPath, s, function(err) {
	    if(err) {
	        return console.log(err);
	    }

	    console.log("SGCT config generated!");
	    execChildProcesses();
	}); 
}

function execChildProcesses(){
	for (var i = 0; i < nNodes; i++) {

		var cmd = path + " -local " + i;
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
<Cluster masterAddress="127.0.0.1" firmSync="true">';
	
	for (var i = 0; i < nNodes; i++) {
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
	var x = i > 0 ? ((640 + i * 578) % 1920) : 10;
	var y = i > 0 ? ((300 + i * 258) % 1080) : 30;
	return '\n\
	<Node address="127.0.0.' + (i+1) + '" port="2040' + (i+1) + '" swapLock="true">\n\
		<Window fullScreen="false">\n\
			<Pos x="'+ x +'" y="' + y + '" />\n\
			<!-- 16:9 aspect ratio -->\n\
			<Size x="640" y="360" />\n\
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
