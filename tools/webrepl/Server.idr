module Main

%default total

{-
  JS FFI Prelude-ish
-}

namespace JS

  ffi_js : Type -> Type
  ffi_js = FTy FFI_JS []

  eval : { ty : Type } ->
         { auto prf : (ffi_js ty) } ->
         ( s : String ) -> ty
  eval { ty } s = foreign FFI_JS s ty

  JsMethod : Type -> Type -> Type
  JsMethod a b = Ptr -> String -> a -> JS_IO b

  JsProp : Type -> Type
  JsProp a = Ptr -> String -> JS_IO a

  ||| General property accessing
  prop : { auto ty : Type } -> { auto prf : (ffi_js (JsProp ty)) } -> JsProp ty
  prop {ty} = eval { ty = (JsProp ty) } "%0[%1]"

  ||| General method application, if it worked.
  ||| @a method input type
  ||| @b method output type
  method : { auto a : Type } ->
           { auto b : Type } ->
           { auto prf : (ffi_js (JsMethod a b)) } -> JsMethod a b
  method {a} {b} = eval { ty = (JsMethod a b) } "%0[%1](%2)"

  log : String -> JS_IO ()
  log s = do console <- eval { ty = JS_IO Ptr } "console"
             method { a = String } { b = () } console "log" s

  require : String -> JS_IO Ptr
  require = eval { ty = (String -> JS_IO Ptr) } "require(%0)"

  ||| Sample fs module function
  readFileSync : Ptr -> String -> JS_IO String
  readFileSync fs fileName = do
    buffer   <- method { a = String } { b = Ptr } fs "readFileSync" fileName
    contents <- method { a = String } { b = String } buffer "toString" "UTF-8"
    pure contents

{-
  HTTP
-}

namespace HTTP

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

{-
  Actual program
-}

sampleHandler : HTTP.Handler
sampleHandler params = do
  req <- prop { ty = Ptr } params "req"
  res <- prop { ty = Ptr } params "res"

  httpMethod <- prop { ty = String } req "method"
  JS.log ("sample handler called with method " ++ httpMethod)

  path   <- prop { ty = String } req "url"
  JS.log ("sample handler called with path " ++ path)

  let result = "You visited: " ++ path
  method { a = String } { b = () } res "end" result

partial
main : JS_IO ()
main = do server <- createServer sampleHandler
          listen server (MkPort 2112)
