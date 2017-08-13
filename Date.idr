module Date

import Data.Vect

%access public export
%default total

{-
  Helpers
-}

||| Generic Type for Non-inclusive Range of Integers
data Range : Integer -> Integer -> Type where
  MkRange : (min : Integer) -> (max : Integer) -> Range min max

||| Proof that an integer is in range
inRange : Range min max -> Integer -> Type
inRange (MkRange min max) x = Dec (x > min = x < max)

{-
  Date Specific
-}

data Year : Type where
  MkYear : (year : Integer) -> Year

%name Year y,y1,y2,y3

isLeap : Year -> Bool
isLeap (MkYear year) = (modNatNZ (toNat year) 4 SIsNotZ) == 0

data Month : Type where
  January   : Month
  February  : Month
  March     : Month
  April     : Month
  May       : Month
  June      : Month
  July      : Month
  August    : Month
  September : Month
  October   : Month
  November  : Month
  December  : Month

%name Month m,m1,m2,m3

monthOrder : Month -> Nat
monthOrder January   = 1
monthOrder February  = 2
monthOrder March     = 3
monthOrder April     = 4
monthOrder May       = 5
monthOrder June      = 6
monthOrder July      = 7
monthOrder August    = 8
monthOrder September = 9
monthOrder October   = 10
monthOrder November  = 11
monthOrder December  = 12

monthDays : Year -> Month -> Integer
monthDays _    January   = 30
monthDays year February  = if isLeap year then 29 else 28
monthDays _    March     = 30
monthDays _    April     = 31
monthDays _    May       = 30
monthDays _    June      = 31
monthDays _    July      = 30
monthDays _    August    = 31
monthDays _    September = 30
monthDays _    October   = 31
monthDays _    November  = 30
monthDays _    December  = 31

previousMonth : Month -> Month
previousMonth January   = December
previousMonth February  = January
previousMonth March     = February
previousMonth April     = March
previousMonth May       = April
previousMonth June      = May
previousMonth July      = June
previousMonth August    = July
previousMonth September = August
previousMonth October   = September
previousMonth November  = October
previousMonth December  = November

data Day : Type where
  MkDay : (day : Nat) -> Day

%name Day d,d1,d2,d3

inDayRange : Nat -> Day -> Type
inDayRange n (MkDay day) =
  let
    n' = toIntegerNat n
    range = (MkRange 1 (n'+1))
    day' = (toIntegerNat day)
  in
    inRange range day'

validDate : Year -> Month -> Day -> Type
validDate year month = inDayRange (toNat $ monthDays year month)

data Date : Type where
  MkDate : (day : Day) ->
           (month : Month) ->
           (year : Year) ->
           { auto p : (validDate year month day) } -> Date

%name Date d,d1,d2,d3

Eq Year where
  (==) (MkYear y) (MkYear y') = y == y'

Eq Month where
  (==) x y  = monthOrder x == monthOrder y

Eq Day where
  (==) (MkDay y) (MkDay y') = y == y'

Eq Date where
  (==) (MkDate d m y) (MkDate d' m' y') = d == d' && m == m' && y == y'

Ord Year where
  compare (MkYear y) (MkYear y') = compare y y'

Ord Month where
  compare x y = compare (monthOrder x) (monthOrder y)

Ord Day where
  compare (MkDay d) (MkDay d') = compare d d'

Ord Date where
  compare (MkDate d m y) (MkDate d' m' y') =
    case compare y y' of
         EQ => case compare m m' of
                    EQ => compare y y'
                    result => result
         result => result

beginIsBeforeEnd : Date -> Date -> Type
beginIsBeforeEnd begin end = case compare begin end of
                                  LT => Dec (begin = begin)
                                  _ => Not (begin = end)

daysToMonth : Year -> Month -> Integer
daysToMonth year month = count (previousMonth month) (monthOrder $ previousMonth month)
  where
    count : Month -> Nat -> Integer
    count m Z = 0
    count m (S k) = (monthDays year m) + count (previousMonth m) k

daysInDate : Date -> Integer
daysInDate (MkDate (MkDay days) month year@(MkYear y)) =
  let
    yearDays = (y - 1) * 365
    monthDays = daysToMonth year month
  in
    yearDays + monthDays + (toIntegerNat days)


distanceBetweenDates : (begin : Date) ->
                       (end : Date) ->
                       { auto prf : (beginIsBeforeEnd begin end) } -> Integer
distanceBetweenDates a b = daysInDate b - daysInDate a

data DateRange : Type where
  MkDateRange : (begin : Date) ->
                (end : Date) ->
                { auto prf : (beginIsBeforeEnd begin end) } ->
                DateRange

%name DateRange dr,dr1,dr2,dr3

distanceInRange : DateRange -> Integer
distanceInRange (MkDateRange begin end) = distanceBetweenDates begin end
