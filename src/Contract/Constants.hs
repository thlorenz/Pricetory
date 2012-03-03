module Contract.Constants where

import Contract.Types

secondsPerMinute = 60  :: TimeInterval
minutesPerHour   = 60  :: TimeInterval
hoursPerDay      = 24  :: TimeInterval
daysPerYear      = 365 :: TimeInterval
secondsPerHour   = secondsPerMinute * minutesPerHour
secondsPerDay    = secondsPerHour * hoursPerDay
secondsPerYear   = secondsPerDay * daysPerYear 

