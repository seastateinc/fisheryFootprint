# R-scripts for the FFP package.
# LIST OF FUNCTIONS.
# 	getAgeSchedules
# 	explogis
#	getSelectivities
# 	eqModel


#' Run the Equilibrium Model
#' @param MP A list object containing the list of model parameters and selectivity parameters.
#' @export
run.ffp <- function(MP)
{
	# Compute age-schedule information.
	if(with(MP,!exists("ageSc"))) MP <- getAgeSchedules(MP)

	# Get length-based selectivity coefficients.
	if(with(MP,!exists("selex"))) MP <- getSelectivities(MP)

	# Establish allocations for YPR or MPR methods.
	# selex$pYPR <- MP$pYPR
	# selex$pMPR <- MP$pMPR
	
	# Run the equilibriu model.
	EM    <- eqModel(theta,selex,type=MP$type)
	return(EM)
}



# 
# EQUILIBRIUM MODEL
# 
#' Equilibrium age- sex-structured model for multiple fleets.
#' @param ffp List of model parameters and age-schedule information.
#' @param ...
#' @export
# eqModel <- function(theta,selex,type="YPR")
eqModel <- function(ffp,...)
{

	with(ffp,{
		lz  <- matrix(1/H,nrow=H,ncol=A)
		za  <- matrix(0,nrow=H,ncol=A)
		qa  <- array(0,dim=c(H,A,ngear))
		pa  <- array(0,dim=c(H,A,ngear))
		dlz <- array(0,dim=c(H,A,ngear))

		# Survivorship under fished conditions at fstar
		fbar <- fstar
		lambda <- rep(1.0,length=ngear)
		for(iter in 1:4)
		{
			# total mortality and survival rates
			fe <- fbar * lambda
			# browser()
			for(h in sex)
			{
				# print(fe)
				if(dim(va)[3] > 1){
					fage   <- rowSums(fe*va[h,,])
				}
				else if(dim(va)[3] == 1){
					fage   <- fe * va[h,,]
				}
				za[h,] <- mx[h,] + fage
			}
			sa <- exp(-za)
			oa <- 1.0 - sa

			# per recruit yield & numbers for gear k
			for(k in 1:ngear)
			{
				qa[,,k] <- va[,,k] * wa * oa / za
				pa[,,k] <- va[,,k] * oa / za
			}

			#  survivorship
			for(j in 2:A)
			{
				lz[,j] <- lz[,j-1] * sa[,j-1]
				if(j == A)
				{
					lz[,j] <- lz[,j] / oa[,j]
				}

				# partial derivatives
				# for(k in 1:ngear)
				# {
				# 	dlz[,j,k] <- sa[,j-1]*(dlz[,j-1,k]-lz[,j-1]*va[,j-1,k])
				# 	if(j == A)
				# 	{
				# 		dlz[,j,k] <- dlz[,j,k]/oa[,j] - lz[,j-1]*sa[,j-1]*va[,j,k]*sa[,j]/oa[,j]^2
				# 	}
				# }
			}


			# Fmultipliers for fstar based on allocations
			qp    <- switch(type,YPR=qa,MPR=pa)
			ak    <- switch(type,YPR=pYPR,MPR=pMPR)
			phi.t <- 0
			for(h in sex)
			{
				phi.t <- phi.t + as.vector(lz[h,] %*% qp[h,,])
			}
			
			
			lam.t  <- ak / (phi.t/sum(phi.t))
			lambda <- lam.t / sum(lam.t)
			# cat(iter," lambda = ",lambda,"\n")
		}

		# incidence functions
		phi.e  <- sum(lz*fa)
		phi.q  <- phi.m <- dphi.e <- dre <- 0
		# dphi.q <- matrix(0,ngear,ngear) 
		for(h in sex)
		{
			# dphi.e <- dphi.e + as.vector(fa[h,] %*% dlz[h,,])
			phi.q  <- phi.q + as.vector(lz[h,] %*% qa[h,,])
			phi.m  <- phi.m + as.vector(lz[h,] %*% pa[h,,])
			# derivatives for yield equivalence
			# for(k in 1:ngear)
			# {
			# 	for(kk in 1:ngear)
			# 	{
			# 		va2 <- va[h,,k] * va[h,,kk]
			# 		dqa <- va2*wa[h,]*sa[h,]/za[h,] - va2*oa[h,]*wa[h,]/za[h,]^2
			# 		dpq  <- as.double(qa[h,,k] %*% dlz[h,,k] + lz[h,] %*% dqa)
			# 		dphi.q[k,kk] <- dphi.q[k,kk] + dpq
			# 	}
			# }
		}
		spr   <- (phi.e/phi.E)
		ispr  <- (phi.E/phi.e)

		# equilibrium recruitment & sensitivity
		re    <- max(0,ro*(kappa-ispr)/(kappa-1))
		be    <- re * phi.e
		# dre   <- ro * phi.E * dphi.e / (phi.e^2 * (kappa-1))
		
		# yield per recuit and yield
		ypr   <- fe * phi.q
		ye    <- re * ypr

		# mortality per recruit and mortality
		mpr   <- fe * phi.m
		me    <- re * mpr
		
		# Jacobian for yield
		# dye <- matrix(0,nrow=ngear,ncol=ngear)
		# for(k in 1:ngear)
		# {
		# 	for(kk in 1:ngear)
		# 	{
		# 		dye[k,kk] = fe[k]*re*dphi.q[k,kk] + fe[k]*phi.q[k]*dre[kk]
		# 		if(k == kk)
		# 		{
		# 			dye[k,kk] = dye[k,kk] + re*phi.q[k]
		# 		}
		# 	}
		# }
		# dye   <- re * as.vector(phi.q * diag(1,ngear)) + fe * phi.q * dre + fe * re * dphi.q
		# dye   <- re * (phi.q) + fe * phi.q * dre + fe * re * dphi.q
		
		# Yield equivalence
		# v  <- sqrt(diag(dye))
		# M  <- dye / (v %o% v)
		# print(v %o% v)
		# print(t(matrix(v,4,4)))

		# print(M)
		# cat("ye\n",ye,"\n")


		# Equilibrium Model output
		out <- list(
		            "fe"  = fe,
		            "ye"  = ye,
		            "me"  = me,
		            "be"  = be,
		            "re"  = re,
		            "spr" = spr,
		            "ypr" = ypr,
		            "mpr" = mpr,
		            # "dre" = dre,
		            # "dye" = as.vector(diag(dye)),
		            "fstar" = fstar,
		            "gear" = slx$sector
		            )

		return(out)
	})
}


# 
# EXPONENTIAL LOGISTIC FUNCTION FOR SELECTIVITY
# 
#' Expontnential logistic function
#' @details From Thompson, 1994 paper for a simple 3 parameter dome-shape selectivity function.
#' @param   x vector of length-intervals
#' @param   mu length-at-50\%  selectivity
#' @param   sd standard deviation
#' @param   g shape parameter (0-1)
#' @export
explogis <- function(x,mu,sd,g)
{
	s <- (1/(1-g))*((1-g)/g)^g
	p <- plogis(x,mu,sd)*exp(g*(mu-x)/sd)
	return(s*p)
}


# 
# GET SELECTIVITIES 
# 
#' Calculate age-specific selectivity coefficients for fishery given a length-at-age and size-limits
#' @param ffp a list object representing selectivity parameters, size-limits, and discard mortality rtes.
#' @details  Based on the joint probability of capture and retention.
#' @export
getSelectivities <- function(ffp)
{
	if( with(ffp,!exists("ageSc")) )
	{
		ffp <- getAgeSchedules(ffp)
	}

	with (ffp,{
		la    <- ageSc$la
		ngear <- dim(slx)[1]
		va <- array(0,dim=c(H,A,ngear))
		for(k in 1:ngear)
		for(h in sex)
		{

			sc <- explogis(la[h,],slx$slx1[k],slx$slx2[k],slx$slx3[k])
			sc[slx$slx4[k]:A] <- sc[slx$slx4[k]-1]

			ra <- plogis(la[h,],slim[k],0.1*la[h,])- plogis(la[h,],ulim[k],0.1*la[h,])
			da <- (1-ra)*dmr[k]
			va[h,,k] <- sc*(ra+da)
		}

		ffp$selex <- list(ngear=ngear,fstar=fstar,va=va)
		return(ffp)
	})
}


# 
# AGE SCHEDULE INFORMATION
# 
#' Calculate age-schedule information given life-history parameters.
#' @param ffp A list object containing model dimensions (age-sex), 
#'        stock parameters (Natural mortlaity, unfished biomass, steepness)
#'        and life-history parameters for growth \& maturity.
#' @export
getAgeSchedules <- function(ffp)
{
	with(ffp,{
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
		ffp$ageSc <- list(ro=ro,la=la,wa=wa,ma=ma,fa=fa,lx=lx,mx=mx,phi.E=phi.E)
		# theta <- c(theta,ageSc)
		return(ffp)
	})
}