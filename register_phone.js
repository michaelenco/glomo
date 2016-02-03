var Socket = require('net').Socket

var sock = new Socket()

sock.connect({
	port: 5226,
	host: "front.glomo.im",
})

var phone = process.argv[2]

sock.on('connect', function(){
	console.log("connected")
	sock.write("<?xml version=\"1.0\"?>")
	sock.write('<stream:stream xmlns:stream="http://etherx.jabber.org/streams" version="1.0" xmlns="jabber:client" to="front.glomo.im" xml:lang="en" xmlns:xml="http://www.w3.org/XML/1998/namespace">')
	sock.write("<iq type='get'><query xmlns='jabber:iq:register_phone'><phone>"+phone+"</phone></query></iq>")
  	sock.pipe(process.stdout)
	process.stdin.pipe(sock)
})


sock.on('end',console.log.bind(null,"end"))

