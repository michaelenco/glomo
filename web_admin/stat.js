var req = new XMLHttpRequest()
req.open('GET', '/admin/api/stat', false)
req.send()
console.log(req.responseText)
