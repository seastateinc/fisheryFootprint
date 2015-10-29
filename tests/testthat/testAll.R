# testAll.R
context("testAll script")
library(fisheryFootprint)

test_that("Check Functions",{
	tst1 <- getAgeSchedules(halibut)
	expect_output(str(tst1),"List of 4")

	tst2 <- getSelectivities(halibut)
	expect_output(str(tst2),"List of 5")

	tst3 <- eqModel(halibut)
	expect_output(str(tst3),"List of 10")

})

test_that("Profile Fstar",{
	prf1 <- run.prf(halibut)
	expect_output(str(prf1),"'data.frame':	400 obs. of  10 variables:")

})

# test_that("Halibut Model",{
# 	M1 <- M2 <- M3 <- M4 <- halibut

# 	# Differences in allocation
# 	# M1$MP$pYPR <- c(0.60,0.20,0.18,0.02)
# 	M2$MP$pYPR <- c(0.70,0.10,0.18,0.02)
# 	M3$MP$slim <- c(0.00,0.00,0.00,0.00)
# 	M4$MP$slim <- c(66.0,0.00,66.0,0.00)

# 	P1 <- cbind(Model="Status Quo",run.prf(M1))
# 	# i1 <- which.max(P1$spr<=M1$HP$sprTarget)
# 	# print(i1)
# 	P2 <- cbind(Model="Reduce bycatch allocation",run.prf(M2))
# 	P3 <- cbind(Model="No minimum size limit",run.prf(M3))
# 	P4 <- cbind(Model="26-inch minimum size limit",run.prf(M4))
# 	df <- rbind(P1,P2,P3,P4)
# 	p <- ggplot(df,aes(spr,ye))
# 	p <- p + geom_area(aes(fill=gear),alpha=0.5) 
# 	p <- p + labs(x="Total Fishing Mortality (FSPR)")
# 	p <- p + geom_vline(xintercept=M1$HP$sprTarget,size=0.5)
# 	p <- p + facet_wrap(~Model)
# 	print(p)

# })