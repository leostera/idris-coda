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
Handler = Ptr -> JS_IO ()

partial
createServer : Handler -> JS_IO Server
createServer handler = do
  http <- require "http"
  server <- eval { ty = (Ptr -> String -> (JsFn Handler) -> JS_IO Ptr) }
                 "%0[%1]( (req, res) => %2({req, res}))"
                 http "createServer" (MkJsFn handler)
  pure $ MkServer server

listen : Server -> Port -> JS_IO ()
listen (MkServer server) (MkPort number) = do
  startedPtr <- method { a = Int } { b = Ptr } server "listen" number
  log ("Started server on http://localhost" ++ (show number))

