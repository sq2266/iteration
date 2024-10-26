Writing functions
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## writing my first function

z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.73294111  1.17916464  0.40444568  1.74160421  0.53605825 -0.78500236
    ##  [7]  1.85389269  1.76250851 -1.52962049 -0.74442743  0.59199454  0.03504265
    ## [13]  0.53752299 -0.50682066 -0.38394929 -1.66805332 -0.08630522 -0.12524258
    ## [19]  0.38908461 -0.86962894 -1.25413506 -0.62986423 -0.67810270  0.52969846
    ## [25] -1.03280604

``` r
z_score = function(x) {
  if (!is.numeric(x)){
    stop("x needs to be number")
  }
  if (length(x) < 5){
    stop("you need at least five numbers to compute the z score")
  }
   
    
  z = (x - mean(x)) / sd(x)
  
  return(z)
}
z_score(x = x_vec)
```

    ##  [1]  0.73294111  1.17916464  0.40444568  1.74160421  0.53605825 -0.78500236
    ##  [7]  1.85389269  1.76250851 -1.52962049 -0.74442743  0.59199454  0.03504265
    ## [13]  0.53752299 -0.50682066 -0.38394929 -1.66805332 -0.08630522 -0.12524258
    ## [19]  0.38908461 -0.86962894 -1.25413506 -0.62986423 -0.67810270  0.52969846
    ## [25] -1.03280604

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): you need at least five numbers to compute the z score

``` r
z_score(x = c("my","name"))
```

    ## Error in z_score(x = c("my", "name")): x needs to be number
