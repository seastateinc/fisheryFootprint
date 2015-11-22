# HalibutFootprint.R
# Objective:
# calculate a time-series of fisheries footprints for Pacific halibut
library(ggplot2)
library(reshape2)
library(scales)
library(fisheryFootprint)
library(ggthemes)
mytheme <- theme_minimal(28,base_family="Garamond") + theme(legend.position="top")



catch_data <- read.table("HalibutLandings.dat",header=TRUE)
asses_data <- read.table("HalibutAssessment.dat",header=TRUE)


pYield  	<- catch_data[7:25,] 
pYield[,-1] <- pYield[,-1] / 1000
pYield <- cbind(type="Catch",pYield)


H <- halibut
H$MP$pscLimit  <- NA

fn <- function(i,type="MPR"){
	H$MP$pYPR      <- unlist(pYield[i,-1:-2])
	H$HP$sprTarget <- asses_data$Fspr[i]/100
	H <- getFspr(H)
	M <- eqModel(H)
	if(type=="MPR") fp <- (1-M$spr)*M$mpr/sum(M$mpr)
	if(type=="YPR") fp <- (1-M$spr)*M$ypr/sum(M$ypr)
	return(fp)
}

df <- data.frame()
for(i in 1:dim(pYield)[1])
{
	df <- rbind(df,fn(i,type="MPR"))
}
# colnames(df) <- H$MP$sector
df <- cbind(type="Footprint",Year=catch_data$Year[7:25],df)
colnames(df) <- colnames(pYield)

mdf <- melt(df,id.var=c("type","Year"))
p   <- 
	ggplot(mdf,aes(Year,value,group=variable,fill=variable)) +
	labs(x="Year",fill="Sector") +
	scale_y_continuous(labels = percent_format())

pFootprintS  <- p + geom_area(position="stack",alpha=0.75) + labs(y="Fishery Footprint (% of (1-SPR))")
pFootprintP  <- p + geom_area(position="fill",alpha=0.75) + labs(y="Relative Fishery Footprint")


# Graphic: Stacked-area plot of catch proportions.
mdf <- melt(pYield,id.var=c("type","Year"))
p <- ggplot(mdf,aes(Year,value,group=variable,fill=variable)) + 
	labs(x="Year",fill="Sector")

pCatchS <- p + geom_area(position="stack",alpha=0.75) + labs(y="Total Mortality (Mlb)")
pCatchP <- p + geom_area(position="fill",alpha=0.75) + labs(y="Proportion of total mortality by weight")

# Combine the two data frames
cbdf <- rbind(pYield,df)
mcbdf <- melt(cbdf,id.var=c("type","Year"))
p <- ggplot(mcbdf,aes(Year,value,group=variable,fill=variable)) + 
	 labs(x="Year",y="Proportion",fill="Sector") + 
	 geom_area(position="fill",alpha=0.75) + 
	 facet_wrap(~type,scales="free_y")


# MATHEMATICAL PROOF FOR APPENDIX.
# If the bycatch allocation is 10% of the Spawning capital and SPR target = 40%
# then you should get the same vector of F's for non-bycatch fisheries if the 
# allocation for bycatch 0 0%, and the SPR target is 50%.

B1 <- B2 <- B3 <- halibut

B1$HP$sprTarget <- 0.40
B2$HP$sprTarget <- 0.40
B3$HP$sprTarget <- 0.40
B1$HP$type <- "YPR"
B2$HP$type <- "MPR"
B3$HP$type <- "FPR"


B1 <- getFstar(B1)
B2 <- getFstar(B2)
B3 <- getFstar(B3)


M1 <- eqModel(B1)
M2 <- eqModel(B2)
M3 <- eqModel(B3)
print(rbind(M1$ye,M2$ye,M3$ye))

# Allocation example
A1 <- A2 <- halibut
A1$MP$pscLimit <- NA
A2$MP$pscLimit <- NA
A1$MP$pYPR = c(1.0,0.0,0,0)
A2$MP$pYPR = c(0.0,1.0,0,0)
print(rbind(eqModel(A1)$spr,eqModel(A2)$spr))
print(rbind(eqModel(A1)$ypr,eqModel(A2)$ypr))
