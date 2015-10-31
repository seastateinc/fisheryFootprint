# HalibutFootprint.R
# Objective:
# calculate a time-series of fisheries footprints for Pacific halibut

library(reshape2)
library(scales)
library(fisheryFootprint)

catch_data <- read.table("HalibutLandings.dat",header=TRUE)
asses_data <- read.table("HalibutAssessment.dat",header=TRUE)


pYield  	<- catch_data[7:25,]
pYield[,-1] <- pYield[,-1] / rowSums(pYield[,-1])


H <- halibut
H$MP$pscLimit  <- NA

fn <- function(i){
	H$MP$pYPR      <- unlist(pYield[i,-1])
	H$HP$sprTarget <- asses_data$Fspr[i]/100
	H <- getFspr(H)
	M <- eqModel(H)
	fp <- (1-M$spr)*M$mpr/sum(M$mpr)
	return(fp)
}

df <- data.frame()
for(i in 1:dim(pYield)[1])
{
	df <- rbind(df,fn(i))
}
colnames(df) <- H$MP$sector
df <- cbind(Year=catch_data$Year[7:25],df)

mdf <- melt(df,id.var="Year")
p   <- 
	ggplot(mdf,aes(Year,value,group=variable,fill=variable)) +
	labs(x="Year",fill="Sector") +
	scale_y_continuous(labels = percent_format())

pS  <- p + geom_area(position="stack") + labs(y="Fishery Footprint (% of (1-SPR))")
pP  <- p + geom_area(position="fill")  + labs(y="Relative Fishery Footprint")


# Graphic: Stacked-area plot of catch proportions.
mdf <- melt(pYield,id.var="Year")
pCatchProportion <- 
	ggplot(mdf,aes(Year,value,group=variable,fill=variable)) + 
	geom_area(position="fill") +
	labs(x="Year",y="Proportion of total mortality by weight")



