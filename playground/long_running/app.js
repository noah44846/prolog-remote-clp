// simple http server that returns a JSON object

var http = require('http');

var value = 0;

http.createServer((req, res) => {
    if (req.url === '/solve') {
        setTimeout(() => { value = 42; }, 5000);
        res.writeHead(202);
        res.end();
        return;
    } else if (req.url === '/status') {
        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify(value > 0));
        return;
    } else if (req.url === '/value') {
        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify(value));
        return;
    }
}).listen(3000);