var Socket = require('net').Socket

var sock = new Socket()

sock.connect({
	port: 5222,
	host: "127.0.0.1",
})

sock.on('connect', function(){
	console.log("connected")
	sock.write("<?xml version=\"1.0\"?>")
	sock.write('<stream:stream xmlns:stream="http://etherx.jabber.org/streams" version="1.0" xmlns="jabber:client" to="localhost" xml:lang="en" xmlns:xml="http://www.w3.org/XML/1998/namespace">')
	sock.write("<iq type='get'><query xmlns='jabber:iq:register_phone'><phone>+79817852735</phone></query></iq>")
  	sock.pipe(process.stdout)
	process.stdin.pipe(sock)
})


sock.on('end',console.log.bind(null,"end"))

