# tsuru_code_sample
Code sample for Tsuru Capital

## Unordered

time stack exec marketfeed-exe > unsorted.txt

real    0m1.575s

user    0m1.805s

sys     0m0.319s



## Ordered by Quote Accept Time

time stack exec marketfeed-exe -r > sorted.txt

real    0m3.156s

user    0m4.431s

sys     0m1.611s
