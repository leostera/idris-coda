module Node.Core

%access public export
%default total

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

