const express = require('express');
const net = require('net');

const app = express();
app.use(express.static("../web/dist"));

const options = {
	port: 8000,
	host: 'localhost'
}

const server = app.listen(8080);
console.log('Serving OpenSpace GUI');

const connection = net.createConnection(options, () => {
	console.log('Connected to OpenSpace')
});

connection.on('end', () => {
	console.log('Lost conneciton to OpenSpace - Exiting.');
	server.close();
	process.exit();
});

connection.on('error', () => {
	console.log('Could not connect to OpenSpace - Exiting.');
	server.close();
	process.exit();
});

// connect to OpenSpace process.
// Whenever the contact is lost, kill app.