# tsuru_code_sample
Code sample for Tsuru Capital

## Unordered

time for i in {1..10}; do stack exec marketfeed-exe r > sorted.txt; done

real    0m4.974s

user    0m3.833s

sys 0m1.487s

## MergeSort

time for i in {1..10}; do stack exec marketfeed-exe r > sorted.txt; done

real    0m35.596s

user    0m31.566s

sys 0m9.622s

## Insertion

time for i in {1..10}; do stack exec marketfeed-exe r > sorted.txt; done

real    0m18.747s

user    0m19.729s

sys 0m7.708s
