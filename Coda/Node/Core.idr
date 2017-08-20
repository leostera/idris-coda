module Node.Core

%access public export
%default total

{-
  Proof and Type Function helpers
-}
ffi_js : Type -> Type
ffi_js = FTy FFI_JS []

JsMethod : Type -> Type -> Type
JsMethod a b = Ptr -> String -> a -> JS_IO b

JsProp : (a : Type) -> Type
JsProp a = Ptr -> String -> JS_IO a


||| The infamous `eval` function. As evil as they come, it'll let you escape
||| this fantasy land and access the node environment. Use with care.
|||
||| @s the raw string to be evaluated
eval : { ty : Type } -> { auto prf : (ffi_js ty) } -> ( s : String ) -> ty
eval { ty } s = foreign FFI_JS s ty

||| General property accessing, useful for quickly exatracting properties from
||| objects, be it primitives or pointers to other objects
|||
||| @ty the type of the value to be retrieved
prop : { auto ty : Type } -> { auto prf : (ffi_js (JsProp ty)) } -> JsProp ty
prop {ty} = eval { ty = (JsProp ty) } "%0[%1]"

||| General method application, unary thou.
|||
||| @a method input type
||| @b method output type
method : { auto a : Type } ->
         { auto b : Type } ->
         { auto prf : (ffi_js (JsMethod a b)) } -> JsMethod a b
method {a} {b} = eval { ty = (JsMethod a b) } "%0[%1](%2)"

||| Logging utility using the default global `console` object
|||
||| @s the string to log
log : (s : String) -> JS_IO ()
log s = do console <- eval { ty = JS_IO Ptr } "console"
           method { a = String } { b = () } console "log" s

||| Default require wrapper; relies on the globally available require function
||| to import packages as pointers.
|||
||| @name package name to require
require : (name : String) -> JS_IO Ptr
require = eval { ty = (String -> JS_IO Ptr) } "require(%0)"

