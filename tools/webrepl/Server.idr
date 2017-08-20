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
  prop : { ty : Type } -> { auto prf : (ffi_js (JsProp ty)) } -> JsProp ty
  prop {ty} = eval { ty = (JsProp ty) } "%0[%1]"

  ||| General method application, if it worked.
  ||| @a method input type
  ||| @b method output type
  method : { a : Type } ->
           { b : Type } ->
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
  Handler = () -> JS_IO ()

  partial
  createServer : Handler -> JS_IO Server
  createServer handler = do
    http <- require "http"
    server <- method { a = (JsFn Handler) } { b = Ptr } http "createServer" (MkJsFn handler)
    pure $ MkServer server

  listen : Server -> Port -> JS_IO ()
  listen (MkServer server) (MkPort number) = do
    startedPtr <- method { a = Int } { b = Ptr } server "listen" number
    log ("Started server on http://localhost" ++ (show number))

{-
  Actual program
-}

sampleHandler : HTTP.Handler
sampleHandler () = JS.log "sample handler called"

partial
main : JS_IO ()
main = do server <- createServer sampleHandler
          listen server (MkPort 2112)
