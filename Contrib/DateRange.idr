module DateRange

import Contrib.Date
import Contrib.Range

%access public export
%default total

daysToMonth : Year -> Month -> Integer
daysToMonth year month = count month' (monthOrder month')
  where
    count : Month -> Nat -> Integer
    count m Z = 0
    count m (S k) = (toIntegerNat $ monthDays year m) + count (previousMonth m) k

    month' : Month
    month' = previousMonth month

daysInDate : Date -> Integer
daysInDate (MkDate year@(MkYear y) month (MkDay days)) =
  let
    yearDays = (y - 1) * 365
    monthDays = daysToMonth year month
  in
    yearDays + monthDays + (toIntegerNat days)

-- Distance Date Integer where
