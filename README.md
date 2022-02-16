## INRAETableOne

An R package for generating tables of descriptive statistics.

## Installation

To install directly from Github:

``` r
require(devtools)
devtools::install_github("parkseeh/INRAETableOne")
```

## Getting Started
A short introduction to the package with examples is shown below

## Example
For this example, you will be using `pal` dataset, which you can directly load
by using `data()`

``` r
library(INRAETableOne)
data(pal)

INRAETableOne(Groupe_ttt ~ ., pal)
```

There are few options you just choose.
The `show.detail = T` will show more summary for the `continuous` variables such as min, max, and median.

``` r
INRAETableOne(Groupe_ttt ~ ., pal, show.detail = T)
```

You can also choose the maximum level of x level by setting `max.x.level` any number you want.
For instance, if you put `max.x.level = 10`, the MMSE which contains only 8 different level of 
factor will be considered as `categorical` variable.
```r
INRAETableOne(Groupe_ttt ~ ., pal, max.x.level = 10)
```

Also if you want to know whether there exists the missing variable, just put 
`show.missing = T` as other argument.
``` r
INRAETableOne(Groupe_ttt ~ ., pal, max.x.level = 10, show.missing = T)
```


