const { createServer } = require('http')
const log = console.log.bind("WEB-REPL => ")

const port = 2112
const host = "localhost"
const url  = `http://${host}:${port}`

const handler = (req, res) =>

createServer(handler).listen(port, () => log(`Listening on ${url}`))
