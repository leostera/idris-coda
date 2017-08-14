module Range

%access public export
%default total

||| Generic Type for Non-inclusive, ordered ranges
data Range : Type -> a -> a -> Type where
  MkRange : (Ord a) =>
            (min : a) ->
            (max : a) ->
            { auto prf : Dec( min < max = True ) } ->
            Range a min max

inRange : Range t a b -> (value : t) -> Type
inRange (MkRange a b) y = Dec (a < y = y < b)

interface Distance a b where
  distance : Range { a = a } t x y -> b
