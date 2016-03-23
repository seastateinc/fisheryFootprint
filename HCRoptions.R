# HCRoptions.R
library(ggplot2)
library(reshape2)
library(grid) #for unit
# A simple R-script for generating alternative Harvest control rules.
N <- 100
status <- seq(0,1,length=N)
refp   <- c(0.2,0.3)
intv   <- findInterval(status,refp)

# Default HCR for Halibut Commission
IPHC_hcr <- rep(0.20,length=N)
IPHC_hcr[intv==0] = 0
IPHC_hcr[intv==1] = 0.2 * (status[intv==1]-refp[1])/diff(refp)
ITQ_ct <- IPHC_hcr * status

# Implied HCR for fixed PSC limit.
bo     <- 235868  # Unfished biomass 520 Mlb in metric tons.
PSC    <- 3515	  # Set by NPFMC in June 2015 (Sitka meeting)
pscLimit <- PSC/(0.4*bo)
PSCL_hcr <- pscLimit/status
PSC_ct <- PSCL_hcr * status
alloc <- cbind(ITQ_ct,PSC_ct)/rowSums(cbind(ITQ_ct,PSC_ct))
df1  <- data.frame("Model"="Status Quo","Stock Status"=status,"Directed"=IPHC_hcr,"Non-directed"=PSCL_hcr)
af1  <- data.frame("Model"="Status Quo","Stock Status"=status,"Directed"=alloc[,1],"Non-directed"=alloc[,2])
cf1  <- data.frame("Model"="Status Quo","Stock Status"=status,"Directed"=ITQ_ct,"Non-directed"=PSC_ct)


# Fixed harvest rate policy.
psc_hr   <- pscLimit/0.4
PSCL_hcr <- psc_hr
PSC_ct <- PSCL_hcr * status
alloc <- cbind(ITQ_ct,PSC_ct)/rowSums(cbind(ITQ_ct,PSC_ct))
df2  <- data.frame("Model"="Fixed HR","Stock Status"=status,"Directed"=IPHC_hcr,"Non-directed"=PSCL_hcr)
af2  <- data.frame("Model"="Fixed HR","Stock Status"=status,"Directed"=alloc[,1],"Non-directed"=alloc[,2])
cf2  <- data.frame("Model"="Fixed HR","Stock Status"=status,"Directed"=ITQ_ct,"Non-directed"=PSC_ct)

# Fixed harvest rate & cap over 40%
refp <- c(0.4,1)
intv <- findInterval(status,refp)
PSCL_hcr[intv==0] <- psc_hr
PSCL_hcr[intv!=0] <- pscLimit/(status[intv!=0])
PSC_ct <- PSCL_hcr * status
alloc <- cbind(ITQ_ct,PSC_ct)/rowSums(cbind(ITQ_ct,PSC_ct))
df3  <- data.frame("Model"="Fixed HR & Cap","Stock Status"=status,"Directed"=IPHC_hcr,"Non-directed"=PSCL_hcr)
af3  <- data.frame("Model"="Fixed HR & Cap","Stock Status"=status,"Directed"=alloc[,1],"Non-directed"=alloc[,2])
cf3  <- data.frame("Model"="Fixed HR & Cap","Stock Status"=status,"Directed"=ITQ_ct,"Non-directed"=PSC_ct)


mdf <- melt(rbind(df1,df2,df3),id.var=c("Model","Stock.Status"))
cdf <- melt(rbind(cf1,cf2,cf3),id.var=c("Model","Stock.Status"))
adf <- melt(rbind(af1,af2,af3),id.var=c("Model","Stock.Status"))

mytheme <- theme_minimal(36,base_family="Garamond") + theme(legend.position="top")  + 
		   theme(panel.margin=unit(3, "lines")) +
		   theme(axis.text=size(12))
.LWD <- 1.5		   

p <- ggplot(mdf,aes(Stock.Status,value,linetype=variable)) + 
	 geom_line(size=.LWD) + ylim(c(0,0.35)) +
	 labs(x="Relative Spawning Stock Biomass",y="Target Harvest Rate",linetype="Fishery") +
	 facet_wrap(~Model) 
print(p + theme_bw()+theme(text=element_text(size=10, family="Garamond")))
print(p + mytheme)

p <- ggplot(cdf,aes(Stock.Status,value,linetype=variable)) + 
	 geom_line(size=.LWD) + ylim(c(0,0.20)) +
	 labs(x="Relative Spawning Stock Biomass",y="Relative catch",linetype="Fishery") +
	 facet_wrap(~Model) 
print(p + theme_bw()+theme(text=element_text(size=10, family="Garamond")))
print(p + mytheme)

p <- ggplot(adf,aes(Stock.Status,value,fill=variable)) + 
	 geom_area(alpha=0.75) + ylim(c(0,1.00)) +
	 labs(x="Relative Spawning Stock Biomass",y="Implied Allocation",fill="Fishery") +
	 facet_wrap(~Model) 
print(p + theme_bw()+theme(text=element_text(size=10, family="Garamond")))
print(p + mytheme)







