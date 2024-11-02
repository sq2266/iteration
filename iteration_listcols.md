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
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## some lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)
l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.093924897 0.406721501 0.673169919 0.552206709 0.312038308 0.397038161
    ##   [7] 0.531873254 0.416062528 0.205798105 0.612721543 0.784346477 0.683261996
    ##  [13] 0.422493010 0.514190268 0.078358247 0.357244330 0.910732037 0.821152227
    ##  [19] 0.452886244 0.841950660 0.171032208 0.225544718 0.065316058 0.890398135
    ##  [25] 0.337023653 0.016036161 0.893368441 0.888302279 0.891281345 0.441372824
    ##  [31] 0.350238168 0.740384005 0.104028117 0.861805360 0.401071371 0.649808479
    ##  [37] 0.956460804 0.868071278 0.202784335 0.448577941 0.128422975 0.406247990
    ##  [43] 0.973453472 0.212416547 0.490521447 0.880270307 0.366775308 0.809257576
    ##  [49] 0.692229165 0.620664949 0.960223186 0.949143104 0.198719714 0.851368690
    ##  [55] 0.654945512 0.118602660 0.198972825 0.927343112 0.162418803 0.067143334
    ##  [61] 0.619052080 0.756742205 0.180148157 0.007142028 0.732596846 0.272828090
    ##  [67] 0.038762602 0.289070614 0.140086913 0.593554741 0.349162441 0.870717097
    ##  [73] 0.087382447 0.913586732 0.107258077 0.881970190 0.797150659 0.702032065
    ##  [79] 0.995194384 0.612700416 0.772178939 0.049277573 0.730733916 0.643048464
    ##  [85] 0.850455491 0.943563300 0.162291778 0.795615855 0.071333059 0.424074847
    ##  [91] 0.986298415 0.957433763 0.687383506 0.397732586 0.438338980 0.098585753
    ##  [97] 0.151853058 0.071910770 0.855816656 0.375250458
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.28212 -0.69479 -0.06421 -0.02355  0.65970  2.97187

``` r
l[["mat"]][1,3]
```

    ## [1] 3

Make a list that is hoepfully a bit more useful.

``` r
list_norm=
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    a = rnorm(20, 4, 10)
  )

list_norm[["b"]]
```

    ##  [1] 13.1376889  5.2784300 11.5512742  3.0846978  6.2458113 -0.2391982
    ##  [7]  1.5934932 11.1013086  6.4858162 11.6611014 10.5294200  4.0235625
    ## [13]  5.6561402  4.3559766 13.1569332 -2.3806209  5.3369800  6.4293266
    ## [19]  3.3121088 -4.6973073

``` r
mean_and_sd = function(x) {
mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  return(out_df)
  
}
```

use the function to take mean ans sd of all sample

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.880  5.13

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.78  5.01

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.53  12.0

``` r
mean_and_sd(list_norm[["d"]])
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1    NA    NA

\###use a loop

``` r
output = vector("list", length = 4)

for(i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
  
}
output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.880  5.13
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.78  5.01
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.53  12.0
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.52  11.3

### “map”

``` r
output = map(list_norm, mean_and_sd)
```

``` r
output = 
  map(list_norm, mean_and_sd)|>
  bind_rows()
  
output = map_dfr(list_norm, mean_and_sd)

output = map_dbl(list_norm, IQR)
output
```

    ##         a         b         c         a 
    ##  6.932188  7.417136 16.323721 12.909974

## list columns

``` r
listcol_df = 
  tibble(
    name = c("a","b","c","d"),
    samp = list_norm
  )

listcol_df
```

    ## # A tibble: 4 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>  
    ## 3 c     <dbl [20]>  
    ## 4 d     <dbl [20]>

``` r
listcol_df|>
  filter(name %in% c("a","b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df |>
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

``` r
listcol_df[["samp"]][["a"]]
```

    ##  [1] -2.6448837 -8.2487707  1.8482687  1.8991320 -4.0269870 -0.6795302
    ##  [7]  2.5560535 -0.7535738 -8.6249376  2.9155037  6.1583580  4.4824082
    ## [13] -3.5757378 -0.2861707  8.2196989 -9.2899648  5.1511798 -5.0641273
    ## [19] -6.9665126 -0.6708087

## compute mean and sd

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.880  5.13

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.78  5.01

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.880  5.13
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.78  5.01
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.53  12.0
    ## 
    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.52  11.3

## list column

``` r
listcol_df |>
  mutate(
    output=map(samp,mean_and_sd),
    iqr = map_dbl(samp, IQR))
```

    ## # A tibble: 4 × 4
    ##   name  samp         output             iqr
    ##   <chr> <named list> <named list>     <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  6.93
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  7.42
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 16.3 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 12.9

``` r
listcol_df |>
  mutate(
    output=map(samp,mean_and_sd),
    iqr = map_dbl(samp, IQR))|>
  select(-samp)|>
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name    mean    sd   iqr
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 a     -0.880  5.13  6.93
    ## 2 b      5.78   5.01  7.42
    ## 3 c      3.53  12.0  16.3 
    ## 4 d      3.52  11.3  12.9

## NSDUH

``` r
nsduh_table_format = function(html, table_num, table_name) {
  
  out_table = 
    html|>
    html_table()|>
    nth(table_num)|>
    slice(-1)|>
    mutate(drug = table_name)|>
    select(-contains("P Value"))
  
  return(out_table)
  
  
}
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_table_format(html = nsduh_html, table_num = 1)
```

    ## Error in `mutate()`:
    ## ℹ In argument: `drug = table_name`.
    ## Caused by error:
    ## ! argument "table_name" is missing, with no default

``` r
nsduh_table_format(html = nsduh_html, table_num = 4)
```

    ## Error in `mutate()`:
    ## ℹ In argument: `drug = table_name`.
    ## Caused by error:
    ## ! argument "table_name" is missing, with no default

``` r
nsduh_table_format(html = nsduh_html, table_num = 5)
```

    ## Error in `mutate()`:
    ## ℹ In argument: `drug = table_name`.
    ## Caused by error:
    ## ! argument "table_name" is missing, with no default

``` r
nsduh_df = 
  tibble(
    drug = c("marj","cocaine","heroin"),
    table_n = c(1, 4, 5)
  ) |>
  mutate(table = map(table_n, nsduh_table_format, html = nsduh_html))|>
  unnest(table)
```

    ## Error in `mutate()`:
    ## ℹ In argument: `table = map(table_n, nsduh_table_format, html =
    ##   nsduh_html)`.
    ## Caused by error in `map()`:
    ## ℹ In index: 1.
    ## Caused by error in `mutate()`:
    ## ℹ In argument: `drug = table_name`.
    ## Caused by error:
    ## ! argument "table_name" is missing, with no default

``` r
nsduh_df|>
  filter(State == "New York")
```

    ## Error in eval(expr, envir, enclos): object 'nsduh_df' not found

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/siiii/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:19:26.958092 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/siiii/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 10:19:33.137313 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/siiii/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 10:19:35.08073 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

``` r
weather_nest = 
weather_df |>
  nest(data = date:tmin)
```

``` r
weather_nest[["data"]][1]
```

    ## [[1]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

define the regression I want.

``` r
weather_lm = function(df) {
  lm(tmax~tmin, data = df)
}
```

using the short function

``` r
weather_lm(weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
weather_nest |>
  map(weather_nest[["data"]],weather_lm)
```

    ## Error in `map()`:
    ## ℹ In index: 1.
    ## ℹ With name: name.
    ## Caused by error in `pluck_raw()`:
    ## ! Index 1 must have length 1, not 4.

``` r
weather_nest |>
  mutate(model_fit = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model_fit
    ##   <chr>          <chr>       <list>             <list>   
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>     
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>     
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>

``` r
weather_nest |>
  mutate(model_fit = map(data, \(x) lm(tmax ~ tmin, data = x)))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model_fit
    ##   <chr>          <chr>       <list>             <list>   
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>     
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>     
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
