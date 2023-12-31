################################################################################################
# WASH Benefits Bangladesh - Effect modification by climate/environment

# Base functions for the WASH Benefits intention to treat (ITT) analyses

# Credit: Ben Arnold (benarnold@berkeley.edu)
################################################################################################

#---------------------------------------
# WASH Benefits permutation test function
# computes a Wilcoxon Signed Rank test
# statistic for two treatment arms
# conditional on randomization block
#---------------------------------------
washb.permute <- function(Y,tr,block,contrast,nreps=100000,seed=NULL) {
  # conduct a permutation test of the indepdence of Y and tr, conditional on randomization block
  # using the Wilcoxon rank-sum test statistic
  # Y  : outcome variable
  # tr : treatment assignment, factor
  # block : randomiaztion block, factor
  # contrast : string with 2 levels of tr that should be compared in the permutation test
  # nreps : number of permutations to run to approximate the null (default=100,000)
  # seed : a seed for pseudo-random number generation (for reproducible results)
  require(coin)
  require(plyr)
  pd <- data.frame(Y=Y,tr=tr,block=block)
  pd <- subset(pd,tr==contrast[1]|tr==contrast[2])
  pd$tr <- factor(pd$tr,levels=contrast[1:2])
  pd <- ddply(pd,c("block","tr"),summarise,Y=mean(Y))
  if(!is.null(seed)) set.seed(seed)
  W <- wilcoxsign_test(Y~tr|block,data=pd,distribution = approximate(B=nreps),zero.method="Pratt" )
  show(W)
  
  # now pull out some of the useful information, for convenience since coin() uses S4
  Ho <- qperm(W,seq(0,1,by=0.01))
  p.value <- pvalue(W)[1]
  Z <- statistic(W)
  
  list(p.value=p.value,Z=Z,Ho=Ho,W=W)
}

#---------------------------------------
# Mantel-Haenszel Pooled estimates of
# the prevalence ratio (PR) or the
# prevalence difference (PD) using
# randomization block as the stratification
# variable
#---------------------------------------
# Wrapper function to call the M-H estimator for two different arms of the study
# this relies on teh rma.mh() function in the metafor package
mh.pool <- function(Y,tr,strat,contrast,measure="RR") {
  # estimate the Mantel-Haenszel prevalence ratio
  # note: strata with no outcomes (i.e., missing PR) are dropped
  # this is consistent with a fixed-effects regression analysis, in
  # which those strata would not contribute to the estimates
  # the arguments Y,tr,strat, below need to be from the same dataset
  # Y     : binary outcome variable (here: diar7d)
  # tr    : binary treatment group variable, comparison group first
  # strat : stratification variable (here: block)
  # contrast : vector of length 2 that includes the tr groups to contrast (control(reference arm) and then intervention)
  # measure  : measure of effect. RR = prev ratio, RD = prev difference
  require(metafor)
  mhdat <- data.frame(Y=Y[tr==contrast[1]|tr==contrast[2]],
                      tr=tr[tr==contrast[1]|tr==contrast[2]],
                      strat=strat[tr==contrast[1]|tr==contrast[2]])
  mhdat$tr <- factor(mhdat$tr,levels=contrast[2:1])
  mhtab <- table(mhdat$tr,mhdat$Y,mhdat$strat)
  mhtab <- mhtab[,c(2:1),] # re-order to be consistent w/ metafor table orientation
  
  # suppress warning about yi/vi values being NA -- we know there are sparse tables
  # and a straum with no cases in it is effectively dropped from any fixed-effect estimator
  # the warning thrown by metafor's rma.mh just makes the output unnecessarily noisy
  muffw <- function(w) if( any( grepl( "Some yi/vi values are NA", w) ) ) invokeRestart( "muffleWarning" )
  
  # MH-estimation
  mh.est <- withCallingHandlers( rma.mh(ai=mhtab[1,1,],bi=mhtab[1,2,],ci=mhtab[2,1,],di=mhtab[2,2,],measure=measure,drop00=TRUE,level=95), warning = muffw)
  
  # format the output
  # these two additional objects include log likelihood and AIC (currently not saved): mh.est$fit.stats[1,1],mh.est$fit.stats[3,1]
  res <- c(mh.est$b,mh.est$se,mh.est$ci.lb,mh.est$ci.ub,mh.est$zval,mh.est$pval)
  if(measure=="RR") {
    names(res) <- c("logPR","se.logPR","ci.lb","ci.ub","Z","p")
  } else{
    names(res) <- c("RD","se.RD","ci.lb","ci.ub","Z","p")
  }
  return(res)
}

#---------------------------------------
# Estimate paired t-tests for differences
# in means at the randomization block level
#---------------------------------------
# Wrapper function to call the paired t-test for two different arms of the study
paired.ttest <- function(Y,tr,strat,contrast) {
  # estimate the paired t-test for differences in means
  # paired within randomization blocks
  # the arguments Y,tr,strat, below need to be from the same dataset
  # Y     : quantitative outcome variable (e.g. LAZ)
  # tr    : binary treatment group variable, comparison group first
  # strat : stratification variable (here: block)
  # contrast : vector of length 2 that includes the tr groups to contrast
  ttdat <- data.frame(Y=Y[tr==contrast[1]|tr==contrast[2]],
                      tr=tr[tr==contrast[1]|tr==contrast[2]],
                      strat=strat[tr==contrast[1]|tr==contrast[2]])
  ttdat$tr <- factor(ttdat$tr,levels=contrast[1:2])
  blockmeans <- tapply(ttdat$Y,list(ttdat$strat,ttdat$tr),function(x) mean(x))
  t.est <- t.test(x=blockmeans[,2],y=blockmeans[,1],alternative="two.sided",paired=TRUE,var.equal=FALSE,conf.level=0.95)
  
  # format output
  res <- c(t.est$estimate,t.est$conf.int[1],t.est$conf.int[2],t.est$statistic,t.est$p.value)
  names(res) <- c("diff","ci.lb","ci.ub","t-stat","p")
  return(res)
}

#---------------------------------------
# Unadjusted estimates for
# continuous (paired t-test) or
# binomial (mantel-haenszel) outcomes
#---------------------------------------
ITT.unadj <- function(Y,tr,strat,contrast,binomial=FALSE,measure="RR") {
  # unadjusted ITT estimates of differences between arms, calculated with
  # either a paired T-test (continuous outcomes) or a Mantel-Haenszel pooled
  # estimator (binary outcomes).
  # the arguments Y,tr,strat, below need to be from the same dataset
  # Y     : binary outcome variable (here: diar7d)
  # tr    : binary treatment group variable, comparison group first
  # strat : stratification variable (here: block)
  # contrast : vector of length 2 that includes the tr groups to contrast
  # binomial : logicial. If TRUE, then the M-H estimator is used for binary outcomes
  # measure  : measure of effect for the M-H estimator. RR = prev ratio, RD = prev difference. ignored unless binomial==TRUE.
  if(binomial==TRUE) {
    est <- mh.pool(Y=Y,tr=tr,strat=strat,contrast=contrast,measure=measure)
  } else{
    est <- paired.ttest(Y=Y,tr=tr,strat=strat,contrast=contrast)
  }
  return(est)
}


#---------------------------------------
# estimate variable means by treatment
# arm and survey round. this is useful
# for estimating prevalence + influence
# curve 95% confidence intervals for
# diarrhea, stunting, etc..., as well
# as for uptake measures at enrollment,
# year 1, and year 2.
# this is essentially a wrapper function
# to call tmle for each treatment and survey round
# to just get the correct 95% CIs that
# adjust for clustering
#---------------------------------------
tmle.mean.est <- function(Y,tr,svy,id,group="Control",s=0,family="binomial",print=FALSE) {
  # Y : outcome variable
  # tr: treatment indicator variable
  # svy  : measurment round variable
  # id: cluster ID variable
  # group : string. treatment factor level to compute mean
  # s     : survey round to compute mean. 0, 1, or 2
  require(tmle)
  tmledat <- data.frame(id=id[tr==group & svy==s],
                        svy=svy[tr==group & svy==s],
                        Y=Y[tr==group & svy==s],
                        tr=tr[tr==group & svy==s])
  tmledat <- tmledat[complete.cases(tmledat),]
  mu.fit <- tmle(Y=tmledat$Y,A=NULL,
                 W=as.matrix(rep(1,nrow(tmledat)),nrow=length(Y)),
                 id=tmledat$id,
                 Q.SL.library="SL.mean",
                 family=family
  )
  if(print==TRUE) print(mu.fit)
  mu <- mu.fit$estimates$EY1$psi
  se <- sqrt(mu.fit$estimates$EY1$var.psi)
  ci <- mu.fit$estimates$EY1$CI
  res <- c(mu,ci[1],ci[2])
  names(res) <- c("mean","ci.lb","ci.ub")
  return(res)
}


# --------------------------------------
# function to pre-screen adjustment
# covariates -- restrict to those
# with a LR test P<0.2
# --------------------------------------
washb_prescreen <- function(Y,Ws,family="gaussian", pval=0.2, print=TRUE) {
  # Y   : outcome variable of interest
  # Ws  : data frame of candidate covariates to screen
  # family : exponential model family (gaussian for continuous outcomes, binomial for binary outcomes, poisson for counts, and neg.binom for negative binomial models)
  require(lmtest)
  if(family[1]=="neg.binom"){
    require(MASS)
  }
  
  #Check pvalue
  if(pval>0.99|pval<0){
    stop("P-value threshold not set between 0 and 1.")
  }
  
  dat <- data.frame(Ws,Y)
  dat <- dat[complete.cases(dat),]
  nW <- ncol(Ws)
  LRp <- rep(NA,nW)
  if(family[1]!="neg.binom"){
    for(i in 1:nW) {
      dat$W <- dat[,i]
      fit1 <- glm(Y~W,data=dat,family=family)
      fit0 <- glm(Y~1,data=dat,family=family)
      LRp[i] <- lrtest(fit1,fit0)[2,5]
    }
  }else{
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("Pkg needed for this function to work. Please install it.",
           call. = FALSE)
    }else{
      for(i in 1:nW) {
        dat$W <- dat[,i]
        fit1 <- glm.nb(Y~W,data=dat)
        fit0 <- glm.nb(Y~1,data=dat)
        LRp[i] <- lrtest(fit1,fit0)[2,5]
      }
    }
  }
  p20 <- ifelse(LRp<pval,1,0)
  if(print==TRUE){
    cat("\nLikelihood Ratio Test P-values:\n")
    print(cbind(names(Ws),paste("P =",sprintf("%1.3f",LRp))))
    cat(paste("\n\nCovariates selected (P<",pval,"):\n",sep=""))
    print(cbind(names(Ws)[p20==1],paste("P =",sprintf("%1.3f",LRp[p20==1]))))
  }
  return(names(Ws)[p20==1])
}


# --------------------------------------
# automatic transform of a covariate
# data.frame with factors into one
# with indicator variables (and an
# ommitted category)
# --------------------------------------
design_matrix <- function(W) {
  # W : data frame of covariates that might include factors
  if(class(W)!="matrix" & class(W)!="data.frame"){
    
    W <- data.frame(W)
    if(is.null(ncol(W)) | ncol(W)==0) {
      stop("Something is wrong with W.\nTo be safe, please try specifying it as class=data.frame.")
    }
    if(ncol(W)==1) {
      cat("\n-----------------------------------------\nThe design matrix you supplied is not a matrix or a data.frame\nAssuming that it is a single variable\n-----------------------------------------\n")
    }
    
  }
  ncolW <- ncol(W)
  flist <- numeric()
  for(i in 1:ncolW) {
    if(class(W[,i])!="factor"){
      next
    } else {
      flist <- c(flist,i)
      # strip out extra levels
      W[,i] <- factor(W[,i])
      # create a design matrix, remove the first level
      mm <- model.matrix(~-1+W[,i])
      mW <- mm[,-c(1)]
      # format the names of the indicator variables
      # and add them to the design matrix
      levs <- gsub(" ","",levels(W[,i]) )[-c(1)]
      if(length(levs)<2) mW <- matrix(mW,ncol=1)
      colnames(mW) <- paste(names(W)[i],levs,sep="")
      W <- data.frame(W,mW)
    }
  }
  # now drop the factors that have been replaced by indicators (if any)
  if(length(flist)>0) {
    W <- W[,-c(flist)]
  }
  # return results
  return(W)
}


# --------------------------------------
# Robust clustered SE function
# http://people.su.se/~ma/mcluster.R
# R (www.r-project.org) codes for computing multi-way clustered-standard errors
# Mahmood Arai, Jan 21, 2008.
# See: Thompson (2006), Cameron, Gelbach and Miller (2006) and Petersen (2006).
#
# slightly modified to have it return the vcovCL object
# rather than the updated fit (since need the VC matrix)
# --------------------------------------
cl   <- function(dat,fm, cluster){
  # dat: data used to fit the model
  # fm : model fit (object)
  # cluster : vector of cluster IDs
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}

