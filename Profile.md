## Clients

    73,602,289,076 bytes allocated in the heap
       160,246,312 bytes copied during GC
           695,924 bytes maximum residency (1765 sample(s))
           235,592 bytes maximum slop
                 4 MB total memory in use (1 MB lost due to fragmentation)

    Generation 0: 101206 collections,     0 parallel,  3.89s,  4.97s elapsed
    Generation 1:  1765 collections,     0 parallel,  0.59s,  0.68s elapsed

    Parallel GC work balance: nan (0 / 0, ideal 1)

                        MUT time (elapsed)       GC time  (elapsed)
    Task  0 (worker) :  145.48s    (723.30s)       0.00s    (  0.00s)
    Task  1 (worker) :  145.48s    (723.29s)       0.00s    (  0.00s)
    Task  2 (worker) :  143.26s    (723.29s)       2.22s    (  2.78s)
    Task  3 (worker) :  145.48s    (723.29s)       0.00s    (  0.00s)
    Task  4 (worker) :  145.48s    (723.29s)       0.00s    (  0.00s)
    Task  5 (worker) :  145.48s    (723.30s)       0.00s    (  0.00s)
    Task  6 (worker) :  145.48s    (723.30s)       0.00s    (  0.00s)
    Task  7 (worker) :  145.48s    (723.30s)       0.00s    (  0.00s)
    Task  8 (worker) :  145.48s    (723.30s)       0.00s    (  0.00s)
    Task  9 (worker) :  145.48s    (723.30s)       0.00s    (  0.00s)
    Task 10 (worker) :    0.00s    (  0.00s)       2.19s    (  2.76s)
    Task 11 (bound)  :    0.00s    (  0.00s)       0.07s    (  0.10s)

    SPARKS: 0 (0 converted, 0 pruned)

    INIT  time    0.00s  (  0.00s elapsed)
    MUT   time  141.00s  (723.30s elapsed)
    GC    time    4.48s  (  5.65s elapsed)
    EXIT  time    0.01s  (  0.01s elapsed)
    Total time  145.49s  (728.96s elapsed)

    %GC time       3.1%  (0.8% elapsed)

    Alloc rate    521,962,059 bytes per MUT second

    Productivity  96.9% of total user, 19.3% of total elapsed

    gc_alloc_block_sync: 0
    whitehole_spin: 0
    gen[0].sync_large_objects: 0
    gen[1].sync_large_objects: 0

## Server

    65,168,429,292 bytes allocated in the heap
     8,595,918,368 bytes copied during GC
      178,748,468 bytes maximum residency (47 sample(s))
       15,574,024 bytes maximum slop
              373 MB total memory in use (0 MB lost due to fragmentation)

    Generation 0: 114518 collections,     0 parallel, 81.59s, 82.61s elapsed
    Generation 1:    47 collections,     0 parallel,  2.22s,  2.26s elapsed

    Parallel GC work balance: nan (0 / 0, ideal 1)

                        MUT time (elapsed)       GC time  (elapsed)
    Task  0 (worker) :  176.72s    (653.14s)      83.13s    ( 84.15s)
    Task  1 (worker) :  259.63s    (653.15s)       0.23s    (  0.23s)
    Task  2 (bound)  :    0.00s    (  0.00s)       0.45s    (  0.49s)

    SPARKS: 0 (0 converted, 0 pruned)

    INIT  time    0.00s  (  0.00s elapsed)
    MUT   time  176.04s  (653.14s elapsed)
    GC    time   83.81s  ( 84.87s elapsed)
    EXIT  time    0.00s  (  0.01s elapsed)
    Total time  259.86s  (738.03s elapsed)

    %GC time      32.3%  (11.5% elapsed)

    Alloc rate    370,172,353 bytes per MUT second

    Productivity  67.7% of total user, 23.9% of total elapsed

    gc_alloc_block_sync: 0
    whitehole_spin: 0
    gen[0].sync_large_objects: 0
    gen[1].sync_large_objects: 0


