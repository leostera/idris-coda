module Node.HTTP

import Coda.Node.Core

%access public export
%default total

record Server where
  constructor MkServer
  server : Ptr

data Port : Type where
  MkPort : (n : Int) -> { auto p : Dec ( n > 0 = True ) } -> Port

Handler : Type
Handler = Ptr -> Ptr -> JS_IO ()

wrapHandler : Handler -> Ptr -> JS_IO ()
wrapHandler handler ptr = do
  req <- prop { ty = Ptr } ptr "req"
  res <- prop { ty = Ptr } ptr "res"
  handler req res

partial
createServer : Handler -> JS_IO Server
createServer handler = do
  http <- require "http"
  server <- eval { ty = (Ptr -> String -> (JsFn (Ptr -> JS_IO())) -> JS_IO Ptr) }
                 "%0[%1]( (req, res) => %2({req, res}))"
                 http "createServer" (MkJsFn (wrapHandler handler))
  pure $ MkServer server

listen : Server -> Port -> JS_IO ()
listen (MkServer server) (MkPort number) = do
  startedPtr <- method { a = Int } { b = Ptr } server "listen" number
  log ("Started server on http://localhost:" ++ (show number))

