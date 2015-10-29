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

test_that("Halibut Model",{
	M1 <- M2 <- M3 <- M4 <- halibut
	M1$MP$slim <- c(81.3,0.00,81.3,0.00)
	M2$MP$slim <- c(76.2,0.00,76.2,0.00)
	M3$MP$slim <- c(71.1,0.00,71.1,0.00)
	M4$MP$slim <- c(66.0,0.00,66.0,0.00)

	P1 <- run.prf(M1,model="Size limit: 32-inches")
	P2 <- run.prf(M2,model="Size limit: 30-inches")
	P3 <- run.prf(M3,model="Size limit: 28-inches")
	P4 <- run.prf(M4,model="Size limit: 26-inches")
	i1 <- which.max(P1$spr<=M1$HP$sprTarget)

	df <- rbind(P1,P2,P3,P4)

	# data frame for arrow position at F_SPR target
	
	ap <- df %>% filter(spr>=0.4) %>% group_by(model,gear)%>% select(fe,ye) %>% slice(n())
	p <- ggplot(df,aes(spr,mpr))
	p <- p + geom_area(aes(fill=gear),alpha=0.5) 
	p <- p + labs(x="Total Fishing Mortality (FSPR)")
	p <- p + geom_vline(xintercept=M1$HP$sprTarget,size=0.5)
	p <- p + facet_wrap(~model)
	print(p)

})