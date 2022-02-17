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

```
Summary descriptives table by 'Groupe_ttt'

_________________________________________ 
            Placebo       Verum       p  
            (n=364)      (n=366)   
----------------------------------------- 
 Age       64.2 ± 2.9   64.1 ± 2.9  0.442
 Sexe                               0.749
  - F     224 (61.5%)  220 (60.1%)       
  - M     140 (38.5%)  146 (39.9%)       
 Period                             1.000
  - T0    182 (50.0%)  183 (50.0%)       
  - T4    182 (50.0%)  183 (50.0%)       
 Poids    68.0 ± 12.0  67.7 ± 11.7  0.781
 BMI       24.4 ± 2.6   24.4 ± 2.8  0.959
 Systole  129.1 ± 18.3 128.3 ± 18.8 0.577
 Diastole 77.9 ± 11.4  78.2 ± 11.6  0.711
 Freq_car 72.2 ± 10.6  72.3 ± 11.2  0.972
 PAL_TEA  38.4 ± 26.4  39.3 ± 26.8  0.658
 MMSE      28.7 ± 1.2   28.7 ± 1.2  0.697
-----------------------------------------
```


The `show.detail = T` will show more summary for the `continuous` variables such as min, max, and median.

``` r
INRAETableOne(Groupe_ttt ~ ., pal, show.detail = T)
```

```
        Summary descriptives table by 'Groupe_ttt'        

___________________________________________________________ 
                       Placebo            Verum         p  
                       (n=364)           (n=366)     
----------------------------------------------------------- 
 Age                                                  0.442
  - Mean ± SD        64.2 ± 2.9        64.1 ± 2.9          
  - Med [Min;Max]  64.0[60.0;70.0]   64.0[60.0;70.0]       
 Sexe                                                 0.749
  - F                224 (61.5%)       220 (60.1%)         
  - M                140 (38.5%)       146 (39.9%)         
 Period                                               1.000
  - T0               182 (50.0%)       183 (50.0%)         
  - T4               182 (50.0%)       183 (50.0%)         
 Poids                                                0.781
  - Mean ± SD        68.0 ± 12.0       67.7 ± 11.7         
  - Med [Min;Max] 68.0[40.0;103.0]  67.0[46.0;101.0]       
 BMI                                                  0.959
  - Mean ± SD        24.4 ± 2.6        24.4 ± 2.8          
  - Med [Min;Max]  24.2[19.5;30.5]   24.3[18.8;31.4]       
 Systole                                              0.577
  - Mean ± SD       129.1 ± 18.3      128.3 ± 18.8         
  - Med [Min;Max] 129.0[88.0;214.0] 127.0[82.0;195.0]      
 Diastole                                             0.711
  - Mean ± SD        77.9 ± 11.4       78.2 ± 11.6         
  - Med [Min;Max] 77.0[45.0;114.0]  77.0[41.0;111.0]       
 Freq_car                                             0.972
  - Mean ± SD        72.2 ± 10.6       72.3 ± 11.2         
  - Med [Min;Max] 72.0[48.0;113.0]  72.0[41.0;111.0]       
 PAL_TEA                                              0.658
  - Mean ± SD        38.4 ± 26.4       39.3 ± 26.8         
  - Med [Min;Max]  30.0[3.0;131.0]   30.0[3.0;132.0]       
 MMSE                                                 0.697
  - Mean ± SD        28.7 ± 1.2        28.7 ± 1.2          
  - Med [Min;Max]  29.0[23.0;30.0]   29.0[23.0;30.0]       
----------------------------------------------------------- 

```




You can also choose the maximum level of x level by setting `max.x.level` any number you want.
For instance, if you put `max.x.level = 10`, the MMSE which contains only 8 different level of 
factor will be considered as `categorical` variable.
```r
INRAETableOne(Groupe_ttt ~ ., pal, max.x.level = 10)
```

```
Summary descriptives table by 'Groupe_ttt'

_________________________________________ 
            Placebo       Verum       p  
            (n=364)      (n=366)   
----------------------------------------- 
 Age       64.2 ± 2.9   64.1 ± 2.9  0.442
 Sexe                               0.749
  - F     224 (61.5%)  220 (60.1%)       
  - M     140 (38.5%)  146 (39.9%)       
 Period                             1.000
  - T0    182 (50.0%)  183 (50.0%)       
  - T4    182 (50.0%)  183 (50.0%)       
 Poids    68.0 ± 12.0  67.7 ± 11.7  0.781
 BMI       24.4 ± 2.6   24.4 ± 2.8  0.959
 Systole  129.1 ± 18.3 128.3 ± 18.8 0.577
 Diastole 77.9 ± 11.4  78.2 ± 11.6  0.711
 Freq_car 72.2 ± 10.6  72.3 ± 11.2  0.972
 PAL_TEA  38.4 ± 26.4  39.3 ± 26.8  0.658
 MMSE                               0.092
  - 23      1 (0.3%)     1 (0.3%)        
  - 24      1 (0.3%)     1 (0.3%)        
  - 25      7 (2.0%)     4 (1.1%)        
  - 26      9 (2.5%)    11 (3.1%)        
  - 27     26 (7.3%)    49 (13.6%)       
  - 28     84 (23.7%)   69 (19.2%)       
  - 29    130 (36.6%)  116 (32.3%)       
  - 30     97 (27.3%)  108 (30.1%)       
----------------------------------------- 
```

Also if you want to know whether there exists the missing variable, just put 
`show.missing = T` as other argument.
``` r
INRAETableOne(Groupe_ttt ~ ., pal, max.x.level = 10, show.missing = T)
```

```
Summary descriptives table by 'Groupe_ttt'

___________________________________________ 
              Placebo       Verum       p  
              (n=364)      (n=366)   
------------------------------------------- 
 Age         64.2 ± 2.9   64.1 ± 2.9  0.442
 Sexe                                 0.749
  - F       224 (61.5%)  220 (60.1%)       
  - M       140 (38.5%)  146 (39.9%)       
  - Missing   0 (0.0%)     0 (0.0%)        
 Period                               1.000
  - T0      182 (50.0%)  183 (50.0%)       
  - T4      182 (50.0%)  183 (50.0%)       
  - Missing   0 (0.0%)     0 (0.0%)        
 Poids      68.0 ± 12.0  67.7 ± 11.7  0.781
 BMI         24.4 ± 2.6   24.4 ± 2.8  0.959
 Systole    129.1 ± 18.3 128.3 ± 18.8 0.577
 Diastole   77.9 ± 11.4  78.2 ± 11.6  0.711
 Freq_car   72.2 ± 10.6  72.3 ± 11.2  0.972
 PAL_TEA    38.4 ± 26.4  39.3 ± 26.8  0.658
 MMSE                                 0.090
  - 23        1 (0.3%)     1 (0.3%)        
  - 24        1 (0.3%)     1 (0.3%)        
  - 25        7 (1.9%)     4 (1.1%)        
  - 26        9 (2.5%)    11 (3.0%)        
  - 27       26 (7.1%)    49 (13.4%)       
  - 28       84 (23.1%)   69 (18.9%)       
  - 29      130 (35.7%)  116 (31.7%)       
  - 30       97 (26.6%)  108 (29.5%)       
  - Missing   9 (2.5%)     7 (1.9%)        
------------------------------------------- 
```

Last but not least, you can add second y variable using `+` sign at the left hand side of 
`~` on the formula.

```
INRAETableOne(Groupe_ttt + Sexe ~ ., pal)
```

```
      Descriptive Statistics stratified by 'Groupe_ttt' and 'Sexe'      
_________________________________________________________________________ 
                        Verum                           Placebo             
          ------------------------------- ------------------------------- 
               F            M         p        F            M         p  
            (n=220)      (n=146)            (n=224)      (n=140)         
------------------------------------------------------------------------- 
 Age       63.7 ± 2.7   64.6 ± 3.2  0.007  64.1 ± 3.0   64.4 ± 2.7  0.280
 Period                             1.000                           1.000
  - T0    110 (50.0%)   73 (50.0%)        112 (50.0%)   70 (50.0%)       
  - T4    110 (50.0%)   73 (50.0%)        112 (50.0%)   70 (50.0%)       
 Poids     61.4 ± 8.4   77.4 ± 9.1  0.000  61.5 ± 8.6   78.5 ± 8.7  0.000
 BMI       23.6 ± 2.6   25.7 ± 2.6  0.000  23.7 ± 2.6   25.5 ± 2.1  0.000
 Systole  123.9 ± 16.7 135.0 ± 19.9 0.000 124.0 ± 16.2 137.1 ± 18.5 0.000
 Diastole 75.7 ± 11.4  82.1 ± 11.0  0.000 74.6 ± 10.0  83.3 ± 11.5  0.000
 Freq_car 74.3 ± 10.9  69.2 ± 11.2  0.000 73.2 ± 10.7  70.7 ± 10.3  0.030
 PAL_TEA  37.3 ± 25.6  42.4 ± 28.4  0.075 34.4 ± 24.7  44.8 ± 27.8  0.000
 MMSE      28.8 ± 1.2   28.5 ± 1.3  0.075  28.8 ± 1.1   28.6 ± 1.3  0.120
------------------------------------------------------------------------- 
```



