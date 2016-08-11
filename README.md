# tsuru_code_sample
Code sample for Tsuru Capital

## Unordered

time ./parse-quote > unsorted.txt

```
real    0m0.309s

user    0m0.282s

sys     0m0.016s
```

```
831,683,584 bytes allocated in the heap

  8,946,352 bytes copied during GC

     88,440 bytes maximum residency (2 sample(s))

     19,496 bytes maximum slop

          1 MB total memory in use (0 MB lost due to fragmentation)
```

## Ordered by Quote Accept Time

time ./parse-quote -r > sorted.txt

```
real    0m1.827s

user    0m1.744s

sys     0m0.061s
```

```
3,795,475,744 bytes allocated in the heap

1,053,938,808 bytes copied during GC

   17,678,752 bytes maximum residency (15 sample(s))

      531,408 bytes maximum slop

           51 MB total memory in use (0 MB lost due to fragmentation)
```
