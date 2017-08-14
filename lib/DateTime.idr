module DateTime

import Date
import Time

data DateTime : Type where
  MkDateTime : (d : Date) -> (i : Instant) -> DateTime

data LocalDateTime : TimeZone offset -> Type where
  MkLocalDateTime : (tz : TimeZone offset) ->
                    (d : DateTime) ->
                    LocalDateTime tz
