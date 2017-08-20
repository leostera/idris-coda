module Main

import Coda.Node.Core
import Coda.Node.HTTP
import Coda.Node.FS

%default total

{-
  HTTP Helpers
-}

queryParams : Ptr -> JS_IO (List String)
queryParams req = Strings.split (== '?') <$> prop { ty = String } req "url"

end : Ptr -> String -> JS_IO ()
end res = method { a = String } { b = () } res "end"

{-
  Route Handlers
-}

handleIndex : Ptr -> Ptr -> JS_IO ()
handleIndex req res = do
  fs <- require "fs"
  indexFile <- readFileSync fs "./index.html"
  end res indexFile

handleRun : Ptr -> Ptr -> JS_IO ()
handleRun req res = do
  [_, query] <- queryParams req | _ => end res "Needs a query"
  log query
  end res query

handle404 : Ptr -> Ptr -> JS_IO ()
handle404 _ res = end res "Invalid route"

route : (String, String) -> Ptr -> Ptr -> JS_IO ()
route ("GET", "/")    = handleIndex
route ("GET", "/run") = handleRun
route _ = handle404

partial
router : HTTP.Handler
router req res = do
  verb <- prop { ty = String } req "method"
  [path, query] <- queryParams req
  log (verb ++ " " ++ path ++ " ? " ++ query)
  route (verb, path) req res

partial
main : JS_IO ()
main = do server <- createServer router
          listen server (MkPort 2112)
