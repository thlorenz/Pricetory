# Random Data Generation

## Comparing Lazy to Strict ByteString

Generating data points and writing to file:

    | Points        | Lazy seconds  | Strict seconds |
    |---------------|---------------|----------------|
    | 100,000       | 1.3           |                |
    | 1,000,000     | 12.6          |                |
    | 5,000,000     | 65.7          |                |
    | 10,000,000    | 128.9         |                |
    |---------------|---------------|----------------|
    | Points/second | 77600         |                | 
    
