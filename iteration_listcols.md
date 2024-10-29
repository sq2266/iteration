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
    ##   [1] 0.315099539 0.286301921 0.256521955 0.388690174 0.366974248 0.525955992
    ##   [7] 0.250650635 0.614554184 0.037463278 0.356165355 0.775662159 0.789636780
    ##  [13] 0.773568944 0.234876106 0.482698377 0.939164761 0.308084977 0.419324364
    ##  [19] 0.472391648 0.672127721 0.289381515 0.375101964 0.291568702 0.468813640
    ##  [25] 0.266245577 0.602736627 0.411592803 0.845675783 0.990428652 0.345649360
    ##  [31] 0.990502030 0.472827478 0.075718348 0.115483175 0.398400163 0.240810267
    ##  [37] 0.659574109 0.139701704 0.007746234 0.334650846 0.281133923 0.305384965
    ##  [43] 0.334423528 0.519430099 0.600483447 0.917468837 0.981135072 0.840213267
    ##  [49] 0.077410598 0.972397682 0.720869685 0.677423778 0.973461017 0.418003672
    ##  [55] 0.767601780 0.622959590 0.223139850 0.211124698 0.429944461 0.337071601
    ##  [61] 0.764099486 0.280808022 0.617857554 0.213046173 0.094093770 0.484639896
    ##  [67] 0.811742852 0.886087514 0.144082503 0.295468160 0.058869187 0.428883580
    ##  [73] 0.238857573 0.523416585 0.720881231 0.658387041 0.719531947 0.451986282
    ##  [79] 0.127208777 0.402409678 0.894674560 0.080008709 0.835780933 0.521935202
    ##  [85] 0.662809130 0.402491489 0.209409187 0.699119515 0.332466505 0.705393565
    ##  [91] 0.756714207 0.046206603 0.329856413 0.916352881 0.326969561 0.528739554
    ##  [97] 0.232221238 0.325102704 0.286891228 0.602522521
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.52305 -0.69626 -0.02837 -0.03644  0.62350  3.58721

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

    ##  [1]  6.9151982  8.5396906  4.0154651  0.3861268  4.3393777 -6.6039034
    ##  [7] -1.5260822  4.7115841  2.8807864 -4.0054077  8.6815959  2.9681766
    ## [13] -4.2385268 -4.4445446 -3.7926846 -2.9608767  4.1719760 -0.6796018
    ## [19] -7.4532286  1.8997050

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
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  5.41

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  4.93

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.82  11.1

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
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  5.41
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  4.93
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.82  11.1
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.06  9.22

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
    ##  8.582787  8.059692 13.353083 11.331011

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

    ##  [1]  3.9437525 -4.0800288 -5.2708962  3.5668939 10.1043043  5.3951421
    ##  [7] -1.1752063 -3.7972525  0.4126564 -1.6042424 -2.3293628  4.7718644
    ## [13] -3.3923896  1.3123709  6.8635812 -6.7466400  8.3020593  5.7863825
    ## [19] 11.6702687 -2.9889834

## compute mean and sd

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  5.41

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  4.93

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  5.41
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.690  4.93
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.82  11.1
    ## 
    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.06  9.22

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
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  8.58
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  8.06
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 13.4 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 11.3

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
    ## 1 a      1.54   5.41  8.58
    ## 2 b      0.690  4.93  8.06
    ## 3 c     -1.82  11.1  13.4 
    ## 4 d      5.06   9.22 11.3

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

``` r
weather_nest |>
  mutate(model_fit = map(data, \(x) lm(tmax ~ tmin, data = )))
```

    ## Error in `mutate()`:
    ## ℹ In argument: `model_fit = map(data, function(x) lm(tmax ~ tmin, data =
    ##   ))`.
    ## Caused by error in `map()`:
    ## ℹ In index: 1.
    ## Caused by error:
    ## ! object 'tmax' not found
