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

  tmp.dir((err, tmpPath, cleanupCallback) => {
    console.log('dir:', tmpPath);
    fs.copy(emptyElmDir, tmpPath, () => {
      const code = req.body.code;
      fs.writeFile(path.join(emptyElmDir, 'Main.elm'), code, () => {
        const cmd = `${hackedMake} Main.elm --output out.json --report json`;
        console.log(cmd);
        childProcess.exec(cmd, { cwd: emptyElmDir }, (error, stdout, stderr) => {
          if (error && error.code !== 0) {
            res.status(400);
            res.set('Content-Type', 'application/json');
            res.send(stdout);
            console.log('ERRORS:', stdout);
          } else {
            res.status(200);
            res.sendFile(path.join(emptyElmDir, 'out.json'));
            console.log('SUCCESS');
          }
        });
      });
    });

  });

});


app.post('/run_python', function(req, res) {

  tmp.dir(function(err, path, cleanupCallback) {
    if (err) throw err;

    console.log("Dir: ", path);
    // ugh, that callback christmas tree... should have written in Go
    const code_path = path + '/code.py'
    const log_call_path = path + '/log_call.py'
    fs.copy('src/Runtime/log_call.py', log_call_path, function (err) {
      if (err) return console.error(err)
      
      const code = req.body.code;

      console.log('CODE:', code);

      fs.writeFile(code_path, code, function(err) {
        if(err) throw err;
        
        const stdout_messages = [];
        const stderr_messages = [];

        const python = child_process.spawn('python', [code_path]);
        // TODO: implement f*#@$ing framed protocol
        const f_buf = new FramedBuffer();
        python.stdout.on('data', function(line) {
          console.log('python out:', line);
          f_buf.push(line);
        });
        python.stderr.setEncoding('utf8');
        python.stderr.on('data', function(data) {
          const line = data.trim();
          console.log('python err:', line);
          stderr_messages.push(line);
        });
        f_buf.on('message', function(msg) {
          stdout_messages.push(JSON.parse(msg));
        });

        python.on('close', function(code, signal) {
          console.log('exit code: ', code);

          if(code == 0) {
            res.status(200);
            res.send(stdout_messages);
          } else {
            res.status(500);
            res.send(stderr_messages);
          }

          console.log('contents:', fs.readdirSync(path));

          fs.removeSync(path)

        });

      });

    });

  });

});

// TODO: this must be in a package somewhere...
function FramedBuffer() {
  this.buffer = '';
  this.state = 'beginning';
  this.length_left = 0;
  return this;
}
util.inherits(FramedBuffer, EventEmitter);

FramedBuffer.prototype.push = function(buf) {
  if(buf.length == 0) {
    return;
  }
  if(this.state === 'beginning') {
    const length = buf.readInt32LE();
    this.length_left = length;
    buf = buf.slice(4);
    this.state = 'reading';
  }
  const chunk_length = Math.min(this.length_left, buf.length);
  const this_chunk = buf.toString('utf8', 0, chunk_length);
  this.buffer += this_chunk;
  this.length_left -= chunk_length;
  if(this.length_left === 0) {
    this.state = 'beginning';
    this.emit('message', this.buffer);
    this.buffer = '';
    this.push(buf.slice(chunk_length));
  }
}

// start it up
app.listen(4000, () => {
	console.log('listening on http://localhost:4000/');
});
