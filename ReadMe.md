Advanced R Stuff
================

## Intoduction

This is my first R package, created for Advanced R programming course at
CZU in Prague. All created functions are based on topics, that we had
during the course. Data table and Spatial data were covered during this
course

## Instalation

``` r
devtools::install_github("https://github.com/richardrex/advanced.r.stuff.git")
```

## Documentation

Using this package you can read .rds and .nc files and create a data
table.

# Data Table

``` r
### DDF precipitation
### Loading the package
library(advanced.r.stuff)
### dir with files
dir = dir()
### Reading data (.rds)
data = readDT(dir("ReadMe_files"))
### DDF precipitation
ddf_pr = ddf_precipitation(data)
### Creating a dt with historical ddf and delta
dt = ddf_calculator(ddf_pr)
```

# Spatial Data

``` r
### Loading the package
library(advanced.r.stuff)
### Reading Spatial Data
aux = readDT_Sp(dir)
### Adding Dates and Forcing
dat_for = create_dates_forcing(dir)
### Merging all together
spatial_function(dir, aux, dat_for)
```
