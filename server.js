const express = require('express');
const bodyParser = require('body-parser');
const events = require('events');
const childProcess = require('child_process');
const tmp = require('tmp');
const fs = require('fs-extra');
const EventEmitter = require('events').EventEmitter;
const util = require('util');
const path = require('path');

// const multer = require('multer');

const app = express();

// really annoyed that express doesn't do this by default
app.use(bodyParser.json()); // to support JSON-encoded bodies
app.use(bodyParser.urlencoded({ // to support URL-encoded bodies
  extended: true
})); 
// tell express to print requests in the console as they come in
app.use(require('morgan')('tiny'));
// for uploading files
// app.use(multer({ dest: './saved_data/uploads/'}));

// serve static files
app.use(express.static(__dirname + '/public'));

// tell express how to handle requests
app.get('/', (req, res) => {
	res.sendFile('public/index.html');
});

const emptyElmDir = path.join(path.dirname(process.argv[1]), 'empty-elm-dir');
const hackedMake = path.join(path.dirname(process.argv[1]), 'hacked-compiler', 'elm-make');

app.post('/compile_elm', (req, res) => {

  tmp.dir((err, tmpDir, cleanupCallback) => {
    console.log('dir:', tmpDir);
    fs.copy(emptyElmDir, tmpDir, () => {
      const code = req.body.code;
      console.log('CODE:', code);
      fs.writeFile(path.join(tmpDir, 'Main.elm'), code, () => {
        const cmd = `${hackedMake} Main.elm --output out.json`;
        console.log(cmd);
        childProcess.exec(cmd, { cwd: tmpDir }, (error, stdout, stderr) => {
          if (error && error.code !== 0) {
            res.status(400);
            res.send(stdout + stderr);
            console.log('ERRORS:', stdout);
          } else {
            res.status(200);
            res.sendFile(path.join(tmpDir, 'out.json'));
            console.log('SUCCESS');
          }
        });
      });
    });

  });

});

app.listen(4000, () => {
  console.log('listening on http://localhost:4000/');
});
