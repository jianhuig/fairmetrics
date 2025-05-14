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
> packrat:::recursivePackageDependencies("fairmodels", ignore = "", lib.loc = .libPaths()[1])
 [1] "DALEX"        "R6"           "RColorBrewer" "cli"          "colorspace"   "fansi"        "farver"       "ggplot2"     
 [9] "glue"         "gridExtra"    "gtable"       "iBreakDown"   "ingredients"  "isoband"      "labeling"     "lifecycle"   
[17] "magrittr"     "mgcv"         "munsell"      "patchwork"    "pillar"       "pkgconfig"    "rlang"        "scales"      
[25] "tibble"       "utf8"         "vctrs"        "viridisLite"  "withr"         
> 
> packrat:::recursivePackageDependencies("fairness", ignore = "", lib.loc = .libPaths()[1])
  [1] "ModelMetrics" "R6"           "RColorBrewer"
  [4] "Rcpp"         "SQUAREM"      "askpass"     
  [7] "base64enc"    "brew"         "brio"        
 [10] "bslib"        "cachem"       "callr"       
 [13] "caret"        "cli"          "clipr"       
 [16] "clock"        "colorspace"   "commonmark"  
 [19] "cpp11"        "crayon"       "credentials" 
 [22] "curl"         "data.table"   "desc"        
 [25] "devtools"     "diagram"      "diffobj"     
 [28] "digest"       "downlit"      "dplyr"       
 [31] "e1071"        "ellipsis"     "evaluate"    
 [34] "fansi"        "farver"       "fastmap"     
 [37] "fontawesome"  "foreach"      "fs"          
 [40] "future"       "future.apply" "generics"    
 [43] "gert"         "ggplot2"      "gh"          
 [46] "gitcreds"     "globals"      "glue"        
 [49] "gower"        "gtable"       "hardhat"     
 [52] "highr"        "htmltools"    "htmlwidgets" 
 [55] "httpuv"       "httr2"        "ini"         
 [58] "ipred"        "isoband"      "iterators"   
 [61] "jquerylib"    "jsonlite"     "knitr"       
 [64] "labeling"     "later"        "lava"        
 [67] "lifecycle"    "listenv"      "lubridate"   
 [70] "magrittr"     "memoise"      "mgcv"        
 [73] "mime"         "miniUI"       "munsell"     
 [76] "numDeriv"     "openssl"      "pROC"        
 [79] "parallelly"   "pillar"       "pkgbuild"    
 [82] "pkgconfig"    "pkgdown"      "pkgload"     
 [85] "plyr"         "praise"       "prettyunits" 
 [88] "processx"     "prodlim"      "profvis"     
 [91] "progressr"    "promises"     "proxy"       
 [94] "ps"           "purrr"        "ragg"        
 [97] "rappdirs"     "rcmdcheck"    "recipes"     
[100] "remotes"      "reshape2"     "rlang"       
[103] "rmarkdown"    "roxygen2"     "rprojroot"   
[106] "rstudioapi"   "rversions"    "sass"        
[109] "scales"       "sessioninfo"  "shape"       
[112] "shiny"        "sourcetools"  "sparsevctrs" 
[115] "stringi"      "stringr"      "sys"         
[118] "systemfonts"  "testthat"     "textshaping" 
[121] "tibble"       "tidyr"        "tidyselect"  
[124] "timeDate"     "timechange"   "tinytex"     
[127] "tzdb"         "urlchecker"   "usethis"     
[130] "utf8"         "vctrs"        "viridisLite" 
[133] "waldo"        "whisker"      "withr"       
[136] "xfun"         "xml2"         "xopen"       
[139] "xtable"       "yaml"         "zip"   
>
> packrat:::recursivePackageDependencies("fairmetrics", ignore = "", lib.loc = .libPaths()[1])
 [1] "R6"         "cli"        "dplyr"      "fansi"     
 [5] "generics"   "glue"       "lifecycle"  "magrittr"  
 [9] "pillar"     "pkgconfig"  "rlang"      "tibble"    
[13] "tidyselect" "utf8"       "vctrs"      "withr"    
```