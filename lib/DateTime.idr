module DateTime

import Date
import Time

{-
  ISO 8601 DateTime Package

  DateTime Representation inclues:

    Year - Month - Day T Hour : Minute : Second (+/-) Offset

  See Time.idr and Date.idr

-}


{-
  Type Definitions
-}

data DateTime : Type where
  MkDateTime : (d : Date) -> (i : Instant) -> DateTime

%name DateTime dt,dt1,dt2,dt3

data LocalDateTime : TimeZone offset -> Type where
  MkLocalDateTime : (tz : TimeZone offset) ->
                    (d : DateTime) ->
                    LocalDateTime tz

%name LocalDateTime ldt,ldt1,ldt2,ldt3

{-
  Standard Interface Implementations
-}

Eq DateTime where
  (==) (MkDateTime d i) (MkDateTime d1 i1) = d == d1 && i == i1

Ord DateTime where
  compare (MkDateTime d i) (MkDateTime d1 i1) =
    case compare d d1 of
         EQ => compare i i1
         r => r

Show DateTime where
  show (MkDateTime d i) = show d ++ "T" ++ show i

Eq (LocalDateTime tz) where
  (==) (MkLocalDateTime tz dt) (MkLocalDateTime tz dt1) = dt == dt1

Ord (LocalDateTime tz) where
  compare (MkLocalDateTime tz dt) (MkLocalDateTime tz dt1) = compare dt dt1

Show (LocalDateTime (MkTimeZone name offset)) where
  show (MkLocalDateTime (MkTimeZone name offset) dt) = show dt ++ show offset
