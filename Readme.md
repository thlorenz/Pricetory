# Pricetory
- attempt at streaming historical market prices as efficient as possible

## Binary file format

Header | Tick | Tick | Tick | ...

### Header

MagicNumber | Symbol | StartTime | Interval

- MagicNumber :: Identifies file format
- Symbol      :: Word32 (the symbol that ticks are for)
- StartTime   :: Word32 (time of first tick relative to agreed on offset)
- Interval    :: Word32 (time between ticks in seconds)

HeaderExample:
assuming EurUsd code is 0x00000001

    54484f52|00000001|00ffad34|00000001

    - Magic Number: 0x54484f52 
    - Symbol: "EurUsd"
    - StartTime: 16 756 020 seconds
    - Interval: 1 second

### Tick

64bit struct with two 32bit fields

- TimeOffset :: int (offset from start time defined in header)
- Value      :: int (last 4 digits are considered after decimal, e.g., (* 10^-4)
  gets actual value
