Broom Baby Broom
================
btupper
6/25/2021

## Problem

I have a data frame, and I want to make some linear models among
different variables (columns). I really only want a few important bits
of information from the models, not the models themselves. How I can do
this programmatically?

## Answer

Make a function that computes the linear model between any two variables
(columns) of your choosing, and extracts the statistics you want.
Iterate over the pairs of columns computing the statistics, and then
pool the results into a single data frame. We’ll use the built-in
`mtcars` dataset (see `?mtcars`), but we’ll cast into a handy
[tibble](https://tibble.tidyverse.org/).

``` r
library(dplyr)
library(broom)
library(knitr)

mt_cars <- dplyr::as_tibble(mtcars, rownames = "vehicle")

mt_cars
```

    ## # A tibble: 32 x 12
    ##    vehicle       mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
    ##    <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1 Mazda RX4    21       6  160    110  3.9   2.62  16.5     0     1     4     4
    ##  2 Mazda RX4 …  21       6  160    110  3.9   2.88  17.0     0     1     4     4
    ##  3 Datsun 710   22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
    ##  4 Hornet 4 D…  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
    ##  5 Hornet Spo…  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
    ##  6 Valiant      18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
    ##  7 Duster 360   14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
    ##  8 Merc 240D    24.4     4  147.    62  3.69  3.19  20       1     0     4     2
    ##  9 Merc 230     22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
    ## 10 Merc 280     19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
    ## # … with 22 more rows

Next we’ll make a function that accepts three arguments… the name of the
independent variable, `x`, the name of dependent variable, `y`, and the
data frame. Best practice is to document the function using
[roxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html)
style comments.

We use the super-duper
[broom](https://cran.r-project.org/web/packages/broom/vignettes/broom.html)
package that will sweep all the useful goodies out of a model. Most
models are complex and store statistics and stuff in deeply nested
lists. `broom's` `tidy` and `glance` functions know how to quickly grab
the good bits without any fuss.

``` r
#' Create a linear model with two variables in a data frame. Retrieve just a few of the
#' model statistics.
#'
#' @param x name of the independent variable
#' @param y name of the dependent variale
#' @param data the input data frame that must contain columns x and y
#' @return a single-row tibble with model output and statistics
model_linear_pairs <- function(x = "disp", y = "mpg", data = mt_cars){
  f <- as.formula(sprintf("%s ~ %s", y[1], x[1]))
  m <- lm(f, data = data)
  t <- broom::tidy(m) 
  g <- broom::glance(m) %>%
    dplyr::mutate(model = list(m))
  r <- dplyr::tibble(
                     x = x, 
                     y = y,
                     intercept = t$estimate[1], 
                     slope = t$estimate[2]) %>%
    dplyr::bind_cols(g)
  return(r)
}
```

Let’s just make a few models - all of which are functionally dependent
upon cylinder displacement (`disp`).

``` r
x <- "disp"
yy <- c("mpg", "hp", "wt", "qsec")

r <- lapply(yy,
            function(y, x = "foo", data = NULL){
              model_linear_pairs(x, y, data)
            }, x= x, data = mt_cars) %>%
  dplyr::bind_rows()

r
```

    ## # A tibble: 4 x 17
    ##   x     y     intercept    slope r.squared adj.r.squared  sigma statistic
    ##   <chr> <chr>     <dbl>    <dbl>     <dbl>         <dbl>  <dbl>     <dbl>
    ## 1 disp  mpg       29.6  -0.0412      0.718         0.709  3.25      76.5 
    ## 2 disp  hp        45.7   0.438       0.626         0.613 42.6       50.1 
    ## 3 disp  wt         1.60  0.00701     0.789         0.781  0.457    112.  
    ## 4 disp  qsec      19.3  -0.00625     0.188         0.161  1.64       6.95
    ## # … with 9 more variables: p.value <dbl>, df <dbl>, logLik <dbl>, AIC <dbl>,
    ## #   BIC <dbl>, deviance <dbl>, df.residual <int>, nobs <int>, model <list>

That’s it!
