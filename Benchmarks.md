# Random Data Generation

## Comparing Lazy to Strict ByteString

- using strict for 10,000 points took 0.8s which shows that it is much slower
- strict benchmark for 100,000 points failed due to out of memory exception

## Comparing two append to file approaches

- concating tick ByteStrings and appending to file in one step
- appending each tick ByteString to file one by one

Generating data points and writing to file:

    | Points        | Concat secs   | Append secs    |
    |---------------|---------------|----------------|
    | 100,000       | 1.3           | 9.9            |
    | 1,000,000     | 12.6          | 100.7          |
    | 5,000,000     | 65.7          |                |
    | 10,000,000    | 128.9         |                |
    |---------------|---------------|----------------|
    | Points/second | 77600         | 9930.5         | 
    
## Early server performance

- serving same data (about 4 ticks) every time
- ran 12 concurrent clients
- it took 42s for 10,000 requests to be fullfilled for each client
- this means that 120,000 requests were served in 42s (2857 req/sec)

## Performance when serving real data

Data that is being served is distributed as follows:

    Interval:       1 sec       1 min       1 hour
    Probability:    5%          40%         35%
    Timespan:       15 mins     600 mins    1 year
    Read Ticks:     900         600         8750
    Faults to Disk: N           N           Y 

This shows that on average 3347.5 ticks are served each time
(0.05 * 900 + 0.4 * 600 + 0.35 * 8750).

- ran 120 concurrent clients
- it took 320s (rough estimate) for 100 requests to be fullfilled for each client
- this means that 12,000 requests were served in 320s (40 req/sec)
- this is obviously much worse than the above benchmark that served 4 ticks only at a time
- most likely this is due to the fact that it is serving 837 times as much data
  per request as before (3347.5 / 4)
- if that would affect performance directly, linear and exclusively, it would
  have been 837 times slower, but is actually only 71 times slower (2857 / 40)
- this means, that performance didn't degrade, but rather that the previous
  performance test looked overly optimistic since such a small amount of data
was served each time

### Using LineBuffering on Server and Client (Send and Receive)
- ran 120 concurrent clients
- served 100 requests in 280s (small improvement)

### Using BlockBuffering on Send Only
- ran 120 concurrent clients
- served 100 requests in 80s (bigger improvement)

### Using BlockBuffering on Send and Receive
- ran 120 concurrent clients
- served 100 requests in 65s

