# testAll.R
context("testAll script")
library(fisheryFootprint)

test_that("Check Age Schedule Information",{
	tst1 <- getAgeSchedules(theta)
	expect_output(str(tst1),"List of 19")

	tst2 <- getSelectivities(halibut)
	expect_output(str(tst2),"List of 20")
})