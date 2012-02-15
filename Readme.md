# Pricetory
- attempt at streaming historical market prices as efficient as possible

## Binary file format

Header | Tick | Tick | Tick | ...

### Header

Symbol | StartTime | Interval

- Symbol    :: string 6 * Word8 (the symbol that ticks are for)
- StartTime :: int 32bit (time of first tick relative to agreed on offset)
- Interval  :: int 32bit (time between ticks in seconds)

HeaderExample:
    EURUSD|00ffad34|00000001
    - Symbol: "EurUsd", StartTime: 16 756 020 seconds, Interval: 1 second

### Tick

64bit struct with two 32bit fields

- TimeOffset :: int (offset from start time defined in header)
- Value      :: int (last 4 digits are considered after decimal, e.g., (* 10^-4)
  gets actual value
