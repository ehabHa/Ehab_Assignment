Name : Ehab Kamal Hamdy

github Files: https://github.com/ehabHa/Ehab_Assignment
===============================

> library(purrr)
>
> factorial_loop <- function(x) {
+     if (x == 0 || x == 1)
+         return(1)
+     for (i in (x - 1):1) {
+         x <- x * i
+     }
+     x
+ }
>
> factorial_reduce <- function(x) {
+     if (x == 0)
+         return(1)
+     reduce(1:x, `*`)
+ }
>
> factorial_func <- function(x) {
+     if (x == 0)
+         return(1)
+     x * factorial_func(x - 1)
+ }
>
> fact_tbl <- c(rep(NA, 65))
>
> factorial_mem <- function(x) {
+     if (x == 0)
+         return(1)
+     if (!is.na(fact_tbl)[x])
+         return(fact_tbl[x])
+     fact_tbl[x] <<- x * factorial_mem(x - 1)
+     fact_tbl[x]
+ }
>
> input <- c(0, 1, 6, 11, 13,  45, 63)
>
> factorial(input)
[1] 1.000000e+00 1.000000e+00 7.200000e+02 3.991680e+07 6.227021e+09 1.196222e+56
[7] 1.982608e+87
> map_dbl(input, factorial_loop)
[1] 1.000000e+00 1.000000e+00 7.200000e+02 3.991680e+07 6.227021e+09 1.196222e+56
[7] 1.982608e+87
> map_dbl(input, factorial_reduce)
[1]        1        1      720 39916800       NA       NA       NA
Warning messages:
1: In .x * .y : NAs produced by integer overflow
2: In .x * .y : NAs produced by integer overflow
3: In .x * .y : NAs produced by integer overflow
> map_dbl(input, factorial_func)
[1] 1.000000e+00 1.000000e+00 7.200000e+02 3.991680e+07 6.227021e+09 1.196222e+56
[7] 1.982608e+87
> map_dbl(input, factorial_mem)
[1] 1.000000e+00 1.000000e+00 7.200000e+02 3.991680e+07 6.227021e+09 1.196222e+56
[7] 1.982608e+87
>
> sink("factorial_output.txt")
>
> cat("====== PART 1: Performance and comparison of indivudual input values ======\n")
>
> cat("======================== across factorial functions ======================= \n\n")
>
> fact_tbl <- c(rep(NA, 65))
>
> individual_results <- map(input, ~ microbenchmark(
+     factorial_loop(.),
+     factorial_reduce(.),
+     factorial_func(.),
+     factorial_mem(.)
+ ))
There were 50 or more warnings (use warnings() to see the first 50)
>
> names(individual_results) <- as.character(input)
>
> individual_results
>
> cat("====== PART 2: Performance and comparison of ranges of input values =======\n")
>
> cat("======================== across factorial functions ======================= \n\n")
>
> get_benchmark <- function(x) {
+     fact_tbl <<- c(rep(NA, 100))
+     microbenchmark(map_dbl(x, factorial_loop),
+                    map_dbl(x, factorial_reduce),
+                    map_dbl(x, factorial_func),
+                    map_dbl(x, factorial_mem))
+ }
>
> ranges <- list(`range 1:10` = 1:10,
+                `range 1:50` = 1:50,
+                `range 1:100` = 1:100)
>
> range_results <- map(ranges, get_benchmark)
There were 50 or more warnings (use warnings() to see the first 50)
> range_results
>
> sink()
>
