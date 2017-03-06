library(testthat)
library(FARS)
expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
dname <- system.file("extdata", package="FARS")
setwd(dname)
expect_that(fars_read_years(2016), gives_warning())

