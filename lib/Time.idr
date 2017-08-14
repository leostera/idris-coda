module Time

import Range

%access public export
%default total

{-
  Type Definitions
-}

data Second : Type where
  MkSec : (s : Nat) -> { auto prf : (Dec (0 <= s = s < 60) ) } -> Second

data Minute : Type where
  MkMin : (m : Nat) -> { auto prf : (Dec (0 <= m = m < 60) ) } -> Minute

data Hour : Type where
  MkHour : (h : Nat) -> { auto prf : (Dec (0 <= h = h < 24) ) } -> Hour

data Instant : Type where
  MkInstant : (h : Hour) -> (m : Minute) -> (s : Second) -> Instant

data Sign : Type where
  (+) : Sign
  (-) : Sign

data Offset : Sign -> Hour -> Type where
  MkOffset : (sign : Sign) -> (h : Hour) -> Offset sign h

data TimeZone : Offset sign h -> Type where
  MkTimeZone : (name : String) ->
               (offset : Offset sign h) ->
               TimeZone offset

data LocalTime : TimeZone offset -> Type where
  MkLocalTime : (tz : TimeZone offset) ->
                (i : Instant) ->
                LocalTime tz


{-
  Interface Implementations
-}

Eq Second where
  (==) (MkSec s) (MkSec k) = s == k

Ord Second where
  compare (MkSec s) (MkSec k) = compare s k

Eq Minute where
  (==) (MkMin s) (MkMin k) = s == k

Ord Minute where
  compare (MkMin s) (MkMin k) = compare s k

Eq Hour where
  (==) (MkHour s) (MkHour k) = s == k

Ord Hour where
  compare (MkHour s) (MkHour k) = compare s k

Eq Instant where
  (==) (MkInstant h m s) (MkInstant h' m' s') = h == h' && m == m' && s == s'

Ord Instant where
  compare (MkInstant h m s) (MkInstant h' m' s') =
    case compare h h' of
         EQ => case compare m m' of
                    EQ => compare s s'
                    r => r
         r => r

{-
  Utility Functions
-}

minuteToSeconds : Minute -> Integer
minuteToSeconds (MkMin m) = (toIntegerNat m) * 60

hourToSeconds : Hour -> Integer
hourToSeconds (MkHour h) = (toIntegerNat h) * 60 * 60

instantToSeconds : Instant -> Integer
instantToSeconds (MkInstant h m (MkSec s)) = (hourToSeconds h) +
                                             (minuteToSeconds m) +
                                             (toIntegerNat s)

Distance Instant Integer where
  distance (MkRange x y) = (instantToSeconds y) - (instantToSeconds x)
