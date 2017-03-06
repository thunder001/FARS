## ----load_library--------------------------------------------------------
library(dplyr)
library(graphics)
library(maps)
library(FARS)

## ----single year---------------------------------------------------------
fname <- system.file("extdata","accident_2013.csv.bz2", package="FARS")
dat <- fars_read(fname)
dat[1:6, 1:6]

## ----multiple years------------------------------------------------------
dname <- system.file("extdata", package="FARS")
setwd(dname)
dat <- fars_read_years(c(2013, 2014, 2015))
length(dat)

## ----summarize-----------------------------------------------------------
setwd(dname)
dat <- fars_summarize_years(c(2013, 2014,2015))
head(dat)

## ----visualize-----------------------------------------------------------
setwd(dname)
fars_map_state(17, 2015)

