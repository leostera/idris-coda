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
  JsMethod a b = (Ptr -> String -> a -> JS_IO b)

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

  ||| General reference-by-name grabbing function
  ||| Passing in a name, it will return a pointer to that object if it exists
  ref : (name : String) -> JS_IO Ptr
  ref = eval { ty = (JS_IO Ptr) }

  --log : Ptr -> String -> JS_IO ()
  --log obj s = eval "%0[%1](%2)" (JsMethod String ()) obj "log" s

  --require : String -> JS_IO Ptr
  --require name = eval "require(%0)" (String -> JS_IO Ptr) name

{-
  HTTP
-}

-- record HTTPServer where
--   constructor MkHTTPServer
--   server : Ptr

-- data Port : Type where
--   MkPort : (n : Int) -> { auto p : Dec ( n > 0 = True ) } -> Port

-- Handler : Type
-- Handler = () -> JS_IO ()

-- createServer : Handler -> JS_IO HTTPServer
-- createServer handler = do
--   http <- require "http"
--   server <- js_method { ty = Handler } http "createServer" handler
--   pure $ MkHTTPServer server

-- listen : HTTPServer -> Port -> JS_IO ()
-- listen (MkHTTPServer server) (MkPort number) = do
--   start <- js_method { ty = Int } server "listen" number
--   log ("Started server on http://localhost" ++ (show number))

{-
  Actual program

main : JS_IO ()
main = do fs <- require "fs"
          console <- eval "console" (JS_IO Ptr)
          f_ok <- prop { ty = Int } fs "R_OK"
          log console "no way!"
          log console (show f_ok)

-}

-- main = do server <- createServer ?the_handler
--           listen server (MkPort 2112)


