# R-scripts for the FFP package.
# LIST OF FUNCTIONS.
# .getAgeSchedules

# 
# AGE SCHEDULE INFORMATION
# 
#' Calculate age-schedule information given life-history parameters.
#' @params theta A list object containing model dimensions (age-sex), 
#'         stock parameters (Natural mortlaity, unfished biomass, steepness)
#'         and life-history parameters for growth \& maturity.
#'
.getAgeSchedules <- function(theta)
{
	with(theta,{
		# Length-at-age, weight-at-age, mature weight-at-age
		vonb <- function(age,linf,k,to) {return( linf*(1-exp(-k*(age+to))) )}
		la <- sapply(age,vonb,linf=linf,k=vbk,to=to)
		wa <- a*la^b
		ma <- sapply(age,plogis,location=ahat,scale=ghat);  ma[,1:7] <- 0
		fa <- ma * wa

		# Age-dependent natural mortality (Lorenzen)
		getM <- function(age,vbk,m,cm){
			t1 <- exp(vbk*(age+2))-1
			t2 <- exp(vbk*(age+1))-1
			sa <- (t1/t2)^(-m/vbk)
			mx=m*((log(t1)-log(t2))/vbk)^cm
			return(mx)
		}
		mx <- sapply(age,getM,m=m,vbk=vbk,cm=cm)

		# Survivorship at unfished conditions
		lx <- matrix(0,H,A)
		for(i in age)
		{
			if(i == min(age))
			{
				lx[,i] <- 1.0/H
			}
			else
			{
				lx[,i] <- lx[,i-1] * exp(-mx[,i-1])
			}
			if(i == A)
			{
				lx[,i] <- lx[,i] / (1-exp(-mx[,i]))
			}
		}
		phi.E <- sum(lx*fa)
		ro    <- bo/phi.E

		# List objects to return
		ageSc <- list(ro=ro,la=la,wa=wa,ma=ma,fa=fa,lx=lx,mx=mx,phi.E=phi.E)
		theta <- c(theta,ageSc)
		return(theta)
	})
}