
acc <- system.file("external/accident_2013.csv.bz2", package="fars")
acc<-fars_read(acc)
expect_output(str(acc), "50 VARIABLES", ignore.case = TRUE)
expect_is(acc,"data.frame")
