# fairness_joss_paper

fairmetrics package JOSS article draft and edits.

## Some notes: 

Consider the following: 

1. Show the comparison between memory usage for fairmetrics and its comparison to the fairness and fairmodels package: 

```r
> library(pryr)
> library(callr)
> 
> measure_memory <- function(pkgs) {
+   stopifnot(is.character(pkgs))
+   results <- lapply(pkgs, function(pkg) {
+     mem_bytes <- callr::r(
+       function(pkg) {
+         gc()
+         before <- pryr::mem_used()
+         suppressPackageStartupMessages(library(pkg, character.only = TRUE))
+         gc()
+         after <- pryr::mem_used()
+         as.numeric(after - before)
+       },
+       args = list(pkg),
+       show = FALSE
+     )
+     data.frame(
+       package = pkg,
+       memory_mb = mem_bytes / 1024^2,
+       stringsAsFactors = FALSE
+     )
+   })
+   df <- do.call(rbind, results)
+   rownames(df) <- NULL
+   message("\nMemory usage (MB):")
+   print(df)
+   invisible(df)
+ }
> 
> measure_memory(c("fairmodels","fairness","fairmetrics"))
Memory usage (MB):
      package    memory_mb
1  fairmodels  17.02067566
2    fairness 117.61164856
3 fairmetrics   0.04934692
```

2. Show comparisonn in package comparisons between fairmetrics, fairness and fairmodels:

```r
> packrat:::recursivePackageDependencies("fairmodels", ignore = "", lib.loc = .libPaths()[1]) |> length()
[1] 29
> 
> packrat:::recursivePackageDependencies("fairness", ignore = "", lib.loc = .libPaths()[1]) |> length()
[1] 141
> 
> packrat:::recursivePackageDependencies("fairmetrics", ignore = "", lib.loc = .libPaths()[1]) |> length()
[1] 0
```