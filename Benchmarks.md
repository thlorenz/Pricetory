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
    
