module TAP

import System
import Data.Vect

%default total
%access private

comment : String -> IO ()
comment s = putStrLn ("# " ++ s)

stamp : IO () -> IO ()
stamp rest = do t <- time
                comment ("t " ++ (show t))
                rest

printResult : String -> Bool -> IO ()
printResult n True = putStrLn ("ok " ++ n)
printResult n False = putStrLn ("not ok " ++ n)

runTests : Nat -> Vect n (Lazy (IO Bool)) -> IO ()
runTests k [] = pure ()
runTests k (x :: xs) = x >>= printResult (show k) >>= \_ => runTests (S k) xs

export
plan : (desc : String) -> Vect n (Lazy (IO Bool)) -> IO ()
plan desc tests {n} = do putStrLn "TAP version 13"
                         comment desc
                         putStrLn ("1.." ++ show n)
                         runTests 1 tests
                         stamp $ comment "done"
