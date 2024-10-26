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

## writing my first function

z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.0541499 -0.7102223  0.2938959  0.7141463 -0.1021468 -1.1962188
    ##  [7]  1.3076472 -0.5992300 -0.8106819 -0.2085400  0.8255899  1.6487248
    ## [13]  0.2180574 -0.4698977 -0.5773207 -0.4511064 -0.5879417  1.4577520
    ## [19] -2.3612280  0.5867928  1.2157615  0.3911122  0.9700754  0.8181801
    ## [25] -1.3190514

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

    ##  [1] -1.0541499 -0.7102223  0.2938959  0.7141463 -0.1021468 -1.1962188
    ##  [7]  1.3076472 -0.5992300 -0.8106819 -0.2085400  0.8255899  1.6487248
    ## [13]  0.2180574 -0.4698977 -0.5773207 -0.4511064 -0.5879417  1.4577520
    ## [19] -2.3612280  0.5867928  1.2157615  0.3911122  0.9700754  0.8181801
    ## [25] -1.3190514

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): you need at least five numbers to compute the z score

``` r
z_score(x = c("my","name"))
```

    ## Error in z_score(x = c("my", "name")): x needs to be number

## mean/sd function

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

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.79  3.98

## using a simulation

``` r
sim_df = 
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df |>
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.4  4.56

simulatoin function to check sample mean and sd.

``` r
sim_mean_sd = function(samp_size, true_mean, true_sd) {
    
  sim_df = 
  tibble(
    x = rnorm(samp_size, true_mean, true_sd)
  )

   out_df = 
     sim_df |>
     summarise(
     mean = mean(x),
     sd = sd(x)
  )
  
return(out_df)

}

sim_mean_sd(samp_size = 30,true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.57  9.12

``` r
sim_mean_sd(samp_size = 4,true_mean = 12, true_sd = 30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.20  40.7

``` r
sim_mean_sd(30, 16, 2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  15.3  2.06

### revisit LoTR

``` r
fellowship_df = 
  read_excel("LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship")|>
  janitor::clean_names()

two_towers_df = 
  read_excel("LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")|>
  janitor::clean_names()

return_king_df = 
  read_excel("LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")|>
  janitor::clean_names()
```

using function instead

``` r
lotr_import = function(cell_range, movie_title) {
  
  movie_df = 
    read_excel("LotR_Words.xlsx", range = cell_range) |>
    mutate(movie = movie_title)|>
    janitor::clean_names() |>
    pivot_longer(
       female:male,
       names_to = "sex",
       values_to = "words"
    )|>
    select(movie,everything())
    
  
  return(movie_df)
}

lotr_df = 
  bind_rows(
    lotr_import(cell_range = "B3:D6", movie_title = "fellowship"),
    lotr_import("F3:H6", "two_towers"),
    lotr_import("J3:L6", "return_king"))
```

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)


marj_table = 
  nsduh_html |>
  html_table()|>
  nth(1) |>
  slice(-1) |>
  mutate(drug = "marj")

cocaine_table = 
  nsduh_html |>
  html_table()|>
  nth(4) |>
  slice(-1) |>
  mutate(drug = "cocaine")

heroin_table = 
  nsduh_html |>
  html_table()|>
  nth(5) |>
  slice(-1) |>
  mutate(drug = "heroin")


nsduh_table_format = function(table_number, table_name) {
  out_table = 
    nsduh_html |>
    html_table()|>
    nth(1) |>
    slice(-1) |>
    mutate(drug = "table_name")
  
  
}
```

``` r
nsduh_table_format = function(html,table_number, table_name) {
  out_table = 
    html |>
    html_table()|>
    nth(1) |>
    slice(-1) |>
    mutate(drug = table_name)|>
    select(-contains("P Value"))
  
  return(out_table)
}
bind_rows(
  nsduh_table_format(html = nsduh_html,1, "marj"),
  nsduh_table_format(html = nsduh_html,4, "cocaine"),
  nsduh_table_format(html = nsduh_html,5, "heroin"))
```

    ## # A tibble: 168 × 12
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 158 more rows
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>
