// simple http server that returns a JSON object

var http = require('http');

let a = 0;

http.createServer(function (req, res) {
    if (req.url !== '/') {
        a++;
        console.log(a);
        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ message: 'Not found' }));
        return;
    }
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify(a > 10));
}).listen(3000);
