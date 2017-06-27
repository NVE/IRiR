#-------------------------------------------------------------------------------
# BACON-test
# Multivariate outlier test written by ANDERSSON, J.. Based on Algorithm 3 in
# "BACON: blocked adaptive computationally efficient outlier nominators",
#  Billor et al (2000), p. 286
#
bacon=function(X,alpha=0.15,const=4)
{
        p=ncol(X)
        n=nrow(X)
        m=const*p
        mx=colMeans(X)
        Sx=var(X)
        d=sqrt(mahalanobis(X,center=mx,cov=Sx))
        o=order(d)
        Xs=X[o[1:m],]
        test=TRUE
        while(test)
        {
                m=colMeans(Xs)
                S=var(Xs)
                d=sqrt(mahalanobis(X,center=m,cov=S))
                h=floor((n+p+1)/2)
                r=nrow(Xs)
                chr=max(0,(h-r)/(h+r))
                cnp=1+(p+1)/(n-1)+2/(n-1-3*p)
                cnpr=chr+cnp
                crit=cnpr*qchisq(1-alpha,df=p)
                ind=d<crit
                Xs=X[ind,]
                test=r!=sum(ind)
        }
        return(data.frame(X=X,outlier=ind,dist=d))
}



#compensating for z-variables based on two-stage methods
#----- begin function two.stage
# x is a vector of input values (n_dmu x 1)
# z is a matrix with values of environmental variables (n_dmu x n_z)
# eff is a vector of unconditional efficiency scores (n_dmu x 1)
# lambda is a matrix of reference weights (n_dmu x n_dmu)
two.stage <- function(x,z,eff,lambda)
{
  #data types
  x <- as.vector(x)
  z <- as.matrix(z)
  eff <- as.vector(eff)
  lambda <- as.matrix(lambda)

  #correction based on absolute levels of z-variables
  #regression
  res.regr.abs <- lm(eff ~ z)
  #calculate final efficiency scores
  eff.corr.abs <- as.vector(eff - z%*%res.regr.abs$coefficients[2:(ncol(z)+1)])

  #correction based on NVEs "difference" method
  # AMUNDSVEEN, R.; KORDAHL, O.-P.; KVILE, H. M. & LANGSET, T.
  # SECOND STAGE ADJUSTMENT FOR FIRM HETEROGENEITY IN DEA: A NOVEL APPROACH USED IN REGULATION OF NORWEGIAN ELECTRICITY DSOS
  # Recent Developments in Data Envelopment Analysis and its Applications, 2014, 334

  #cost norm for each dmu
  x.norm <- lambda %*% x
  #norm contribution for each reference dmu
  x.norm.contrib <- lambda %*% diag(x)
  #weight for each reference dmu
  w.ref <- diag(1 / as.vector(x.norm)) %*% x.norm.contrib
  #differences versus reference dmus
  z.diff <- z - w.ref %*% z
  #regression for stage 2 based on differences
  res.regr.NVE <- lm(eff ~ z.diff, data = )  #r_normal,
  #calculate final efficiency scores based on updated z-differences
  eff.corr.NVE <- as.vector(eff - z.diff%*%res.regr.NVE$coefficients[2:(ncol(z)+1)])

  res <- list(eff.corr.abs=eff.corr.abs,eff.corr.NVE=eff.corr.NVE,regr.coeff.abs=res.regr.abs$coefficients,regr.coeff.NVE=res.regr.NVE$coefficients)

  return(res)
}

#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------

# x = x.snitt.d
# z = z=cbind(d_tilDEA$dr_hsjordand, d_tilDEA$dr_s4, d_tilDEA$dr_Geo1, d_tilDEA$dr_Geo2, d_tilDEA$dr_Geo3)
# y = y.snitt.d
# eff = d_tilDEA$d_bs_correst_e3
# lambda = d_lambda.snitt
# id = names(x)
# id.out = as.character(d_separat_dmuer)
# coeff = res.Zvar1$regr.coeff.NVE
#----------------------------------------------------------------------------------------------


#Create Geo variables used for estimating coefficients


z.est = function (geovar.in, restricted.obs = NULL)
        {
        geovar.in = as.matrix(geovar.in)
        pca = (prcomp(geovar.in, scale. = TRUE))
        geovar.ut = predict(pca, newdata = restricted.obs)[,1]*-1
        return(geovar.ut)
        }


#----------------------------------------------------------------------------------------------
#NY VERSJON TILPASSET NVE
# x is a vector of input values (n_dmu dx 1)
# z is a matrix with values of environmental variables (n_dmu x n_z)
# eff is a vector of unconditional efficiency scores (n_dmu x 1)
# lambda is a matrix of reference weights (n_dmu x n_dmu)
Zvar1 <- function(x,z,eff,lambda,id,id.out)
        {
        #data types
        x <- as.vector(x)
        z <- as.matrix(z)
        lambda <- as.matrix(lambda)
        names(x) = id
        rownames(z) = id


        #correction based on NVEs "difference" method
        # AMUNDSVEEN, R.; KORDAHL, O.-P.; KVILE, H. M. & LANGSET, T.
        # SECOND STAGE ADJUSTMENT FOR FIRM HETEROGENEITY IN DEA: A NOVEL APPROACH USED IN REGULATION OF NORWEGIAN ELECTRICITY DSOS
        # Recent Developments in Data Envelopment Analysis and its Applications, 2014, 334

        #cost norm for each dmu
        x.norm = lambda %*% x
        #norm contribution for each reference dmu  - kostnadsbidrag
        x.norm.contrib = lambda %*% diag(x)
        #weight for each reference dmu  - normkostandel
        w.ref = x.norm.contrib / rowSums(x.norm.contrib)
        #differences versus reference dmus
        z.diff = z - w.ref %*% z
        #Only companys defining the technology included in regression
        tech = setdiff(id, id.out)
        z.diff = z.diff[tech, ]
        #outlier-test
        outlier.X <- bacon(cbind(eff, z.diff), alpha = 0.15, const=4)
        id.out = union(id.out,rownames(outlier.X[which(!outlier.X$outlier),]))
        #regression for stage 2 based on differences
        res.regr.NVE <- lm(eff ~ z.diff,subset = setdiff(id,id.out))
        coeff=res.regr.NVE$coefficients
        names(coeff)=c("constant",colnames(z.diff))

        res <- list(coeff=coeff,z.diff=z.diff, id.out=id.out)
        return(res)
        }
#----------------------------------------------------------------------------------------------
#calculate final efficiency scores based on updated z-differences

Zvar2 <- function(x,eff,id,coeff,z,lambda)
        {
        #data types
        x <- as.vector(x)
        z <- as.matrix(z)
        eff <- as.vector(eff)
        lambda <- as.matrix(lambda)
        names(x) = id
        rownames(z) = id
        names(eff) = id

        #cost norm for each dmu
        x.norm = lambda %*% x
        #norm contribution for each reference dmu
        x.norm.contrib = lambda %*% diag(x)
        #weight for each reference dmu
        w.ref = x.norm.contrib / rowSums(x.norm.contrib)
        #differences versus reference dmus
        z.diff = z - w.ref %*% z

        #Adjusts efficiency score
        eff.corr <- as.vector(eff - z.diff%*%coeff[2:(ncol(z)+1)])

        res <- list(eff.corr=eff.corr,z.diff=z.diff)
        return(res)
        }


#----------------------------------------------------------------------------------------------


#calibrating efficiency scores
#----- begin function calibrate
calibrate <- function(eff,totex,weight=NULL)
  {
  #eff, totex, and weight are vectors with lengths equal to the number of DMUs.
  eff = as.vector(eff)
  totex = as.vector(totex)
  if(!is.null(weight)) weight = as.vector(weight)

  #The purpose of the calibration is to ensure that the averagely efficient company a return equal to the reference rate of return.
  #The capital weighted calibration also corrects (somewhat) for the age bias caused by using book values as basis for the capital costs.
  #Other calibration variants, e.g., a multiplicative formula, have been used previously.
  #Setting weight=NULL means that the multiplicative calibration variant will be used.
  #See Bj�rndal, Bj�rndal and Fange (2010).

  industry.avg <- sum(totex*eff)/sum(totex)
  calibration.amount <- sum(totex)-sum(totex*eff)
  if(is.null(weight))
    {
    #multiplicative calibration, i.e., scaling the efficiency scores
    eff.cal <- eff / industry.avg
    }else
    {
    #additive calibration, i.e., adding a constant to all efficiency scores
    #note that weight = totex is equivalent to adding (1-industry.avg) to all the efficiency scores
    weight <- weight / sum(weight)
    eff.cal <- eff + calibration.amount*weight/totex
    }

  return(list(eff.cal=eff.cal,industry.avg=industry.avg,calibration.amount=calibration.amount))
  }
#------------------------------------------------------------------------------

NVE_cal = function(eff, cost_base, RAB)
{
        eff = as.vector(eff)
        cost_base = as.matrix(cost_base)
        RAB = as.matrix(RAB)
        #eff is the Geo-adjusted efficency-scores
        #cost_base is an "estimated" cost base, estimated from future CPI-values
        #RAB is the regulatory asset base
        
        cost_norm = eff*cost_base
        tot.cost_base = sum(cost_base)
        tot.cost_norm = sum(cost_norm)
        tot.RAB = sum(RAB)
        
        cost_norm.calRAB = cost_norm + (tot.cost_base - tot.cost_norm) * (RAB/tot.RAB)
        cost_norm.supp =  (tot.cost_base - tot.cost_norm) * (RAB/tot.RAB)
        
        eff.cal = cost_norm.calRAB / cost_base
        ind.av.eff = tot.cost_norm/tot.cost_base
        
        res = list(eff.cal=as.vector(eff.cal), cost_norm=as.vector(cost_norm), cost_norm.supp=as.vector(cost_norm.supp),
                   tot.cost_base=tot.cost_base,tot.RAB=tot.RAB, ind.av.eff=ind.av.eff,
                   cost_norm.calRAB=as.vector(cost_norm.calRAB))

}
#---------------------------------------------------------------------------------------------------------------------
#Function for comparing dataframes or groups from Cookbook for R
#Written by knitr and Jekyll. If you find any errors, please email winston@stdout.org
#http://www.cookbook-r.com/Manipulating_data/Comparing_data_frames/

dupsBetweenGroups <- function (df, idcol) {
        # df: the data frame
        # idcol: the column which identifies the group each row belongs to
        
        # Get the data columns to use for finding matches
        datacols <- setdiff(names(df), idcol)
        
        # Sort by idcol, then datacols. Save order so we can undo the sorting later.
        sortorder <- do.call(order, df)
        df <- df[sortorder,]
        
        # Find duplicates within each id group (first copy not marked)
        dupWithin <- duplicated(df)
        
        # With duplicates within each group filtered out, find duplicates between groups. 
        # Need to scan up and down with duplicated() because first copy is not marked.
        dupBetween = rep(NA, nrow(df))
        dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
        dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
        
        # ============= Replace NA's with previous non-NA value ==============
        # This is why we sorted earlier - it was necessary to do this part efficiently
        
        # Get indexes of non-NA's
        goodIdx <- !is.na(dupBetween)
        
        # These are the non-NA values from x only
        # Add a leading NA for later use when we index into this vector
        goodVals <- c(NA, dupBetween[goodIdx])
        
        # Fill the indices of the output vector with the indices pulled from
        # these offsets of goodVals. Add 1 to avoid indexing to zero.
        fillIdx <- cumsum(goodIdx)+1
        
        # The original vector, now with gaps filled
        dupBetween <- goodVals[fillIdx]
        
        # Undo the original sort
        dupBetween[sortorder] <- dupBetween
        
        # Return the vector of which entries are duplicated across groups
        return(dupBetween)
}