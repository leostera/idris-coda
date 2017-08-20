module Main

import Coda.Node.Core
import Coda.Node.HTTP

%default total

sampleHandler : HTTP.Handler
sampleHandler params = do
  req <- prop { ty = Ptr } params "req"
  res <- prop { ty = Ptr } params "res"

  httpMethod <- prop { ty = String } req "method"
  log ("sample handler called with method " ++ httpMethod)

  path   <- prop { ty = String } req "url"
  log ("sample handler called with path " ++ path)

  let result = "You visited: " ++ path
  method { a = String } { b = () } res "end" result

partial
main : JS_IO ()
main = do server <- createServer sampleHandler
          listen server (MkPort 2112)
