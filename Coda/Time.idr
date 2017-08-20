module Time

import Coda.Range

%access public export
%default total

{-
  ISO 8601 Time Package

  Time Representation includes:

    Hour : Minute : Second (+/-) Offset

  Where (+/-) Offset is represented as a TimeZone

  And Hour : Minute : Second is represented as an Instant

  And the combination of both is represented as a LocalTime

-}

{-
  Type Definitions
-}

data Second : Type where
  MkSec : (s : Nat) -> { auto prf : (Dec (0 <= s = s < 60) ) } -> Second

%name Second s,s1,s2,s3

data Minute : Type where
  MkMin : (m : Nat) -> { auto prf : (Dec (0 <= m = m < 60) ) } -> Minute

%name Minute m,m1,m2,m3

data Hour : Type where
  MkHour : (h : Nat) -> { auto prf : (Dec (0 <= h = h < 24) ) } -> Hour

%name Hour h,m1,m2,m3

data Instant : Type where
  MkInstant : (h : Hour) -> (m : Minute) -> (s : Second) -> Instant

%name Instant i,i1,i2,i3

data Sign : Type where
  (+) : Sign
  (-) : Sign

%name Sign s,s1,s2,s3

data Offset : Type where
  MkOffset : (sign : Sign) -> (i : Instant) -> Offset

%name Offset off,off1,off2,off3

data TimeZone : Offset -> Type where
  MkTimeZone : (name : String) -> (offset : Offset) -> TimeZone offset

%name TimeZone tz,tz1,tz2,tz3

data LocalTime : TimeZone offset -> Type where
  MkLocalTime : (tz : TimeZone offset) -> (i : Instant) -> LocalTime tz

%name LocalTime lt,lt1,lt2,lt3

{-
  Standard Interface Implementations
-}

Eq Second where
  (==) (MkSec s) (MkSec s') = s == s'

Ord Second where
  compare (MkSec s) (MkSec s') = compare s s'

Show Second where
  show (MkSec s) = if s < 10 then "0"++show s else show s

Eq Minute where
  (==) (MkMin s) (MkMin s') = s == s'

Ord Minute where
  compare (MkMin s) (MkMin s') = compare s s'

Show Minute where
  show (MkMin m) = if m < 10 then "0"++show m else show m

Eq Hour where
  (==) (MkHour s) (MkHour s') = s == s'

Ord Hour where
  compare (MkHour s) (MkHour s') = compare s s'

Show Hour where
  show (MkHour h) = if h < 10 then "0"++show h else show h

Eq Instant where
  (==) (MkInstant h m s) (MkInstant h' m' s') = h == h' && m == m' && s == s'

Ord Instant where
  compare (MkInstant h m s) (MkInstant h' m' s') =
    case compare h h' of
         EQ => case compare m m' of
                    EQ => compare s s'
                    r => r
         r => r

Show Instant where
  show (MkInstant h m s) = "" ++ show h ++ ":" ++ show m ++ ":" ++ show s

Eq Sign where
  (==) (+) (+) = True
  (==) (-) (-) = True
  (==) _ _ = False

Ord Sign where
  compare (+) (-) = GT
  compare (-) (+) = LT
  compare (+) (+) = EQ
  compare (-) (-) = EQ

Show Sign where
  show (+) = "+"
  show (-) = "-"

Eq Offset where
  (==) (MkOffset sign i) (MkOffset sign' i') = sign == sign' && i == i'

Ord Offset where
  compare (MkOffset sign i) (MkOffset sign' i') =
    case compare sign sign' of
         EQ => compare i i'
         r => r

Show Offset where
  show (MkOffset sign i@(MkInstant h m _)) =
    if isZero i
       then "Z"
       else show sign ++ show h ++ ":" ++ show m
    where isZero : Instant -> Bool
          isZero (MkInstant (MkHour Z) (MkMin Z) (MkSec Z)) = True
          isZero _ = False

Eq (TimeZone offset) where
  (==) (MkTimeZone name _) (MkTimeZone name' _) = name == name'

Ord (TimeZone offset) where
  compare (MkTimeZone name _) (MkTimeZone name' _) = compare name name'

Show (TimeZone offset) where
  show (MkTimeZone name offset) = show name ++ show offset

Eq (LocalTime tz) where
  (==) (MkLocalTime _ i) (MkLocalTime _ i') = i == i'

Ord (LocalTime tz) where
  compare (MkLocalTime _ i) (MkLocalTime _ i') = compare i i'

Show (LocalTime (MkTimeZone name offset)) where
  show (MkLocalTime (MkTimeZone name offset) i) = show i ++ show offset

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

instantZero : Instant
instantZero = (MkInstant (MkHour 0) (MkMin 0) (MkSec 0))

UTC : TimeZone (MkOffset (+) Time.instantZero)
UTC = MkTimeZone "UTC" (MkOffset (+) instantZero)
