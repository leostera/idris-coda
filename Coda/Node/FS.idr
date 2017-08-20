module Node.FS

import Coda.Node.Core

%access public export
%default total

||| Sample fs module function
readFileSync : Ptr -> String -> JS_IO String
readFileSync fs fileName = do
  buffer   <- method { a = String } { b = Ptr } fs "readFileSync" fileName
  contents <- method { a = String } { b = String } buffer "toString" "UTF-8"
  pure contents
