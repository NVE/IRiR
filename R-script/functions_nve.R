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

x = x.snitt.r
z = r_tilDEA$rr_Geo3
y = y.snitt.r
eff = dea(x,y,RTS="crs")$eff
lambda = dea(x,y,RTS="crs")$lambda
id = names(x)
id.ut = as.character(r_separat_dmuer)
eff = ifelse(id == "62",0.1,eff)
coeff = res.rvk1$regr.coeff.NVE

res.rvk1 = rvk1(x,z,eff,lambda,id,id.ut)

res.rvk2 = rvk2(eff,id,coeff,z,lambda)
        

#NY VERSJON TILPASSET NVE
# x is a vector of input values (n_dmu dx 1)
# z is a matrix with values of environmental variables (n_dmu x n_z)
# eff is a vector of unconditional efficiency scores (n_dmu x 1)
# lambda is a matrix of reference weights (n_dmu x n_dmu)
rvk1 <- function(x,z,eff,lambda,id,id.ut)  
        {
        #data types  
        x <- as.vector(x)  
        z <- as.matrix(z)  
        eff <- as.vector(eff)  
        lambda <- as.matrix(lambda)
        names(x) = id
        rownames(z) = id
        names(eff) = id
        
        #correction based on NVEs "difference" method  
        # AMUNDSVEEN, R.; KORDAHL, O.-P.; KVILE, H. M. & LANGSET, T.  
        # SECOND STAGE ADJUSTMENT FOR FIRM HETEROGENEITY IN DEA: A NOVEL APPROACH USED IN REGULATION OF NORWEGIAN ELECTRICITY DSOS 
        # Recent Developments in Data Envelopment Analysis and its Applications, 2014, 334
        
        #cost norm for each dmu  
        x.norm = lambda %*% x  
        #norm contribution for each reference dmu  
        x.norm.contrib = lambda %*% diag(x)  
        #weight for each reference dmu  
        w.ref = x.norm.contrib / rowSums(x.norm.contrib)
        #differences versus reference dmus  
        z.diff = z - w.ref %*% z  
        #outlier-test
        # outlier.d <- BEM(baconvar.d, alpha = 0.15)
        outlier.dX <- mvBACON(cbind(eff, z.diff), alpha = 0.00001, init.sel = "Mahalanobis", m=4)
        id.ut = union(id.ut,which(!outlier.dX$subset))
        #regression for stage 2 based on differences  
        res.regr.NVE <- lm(eff ~ z.diff,subset = setdiff(id,id.ut)) 
        
        res <- list(regr.coeff.NVE=res.regr.NVE$coefficients,z.diff=z.diff)
        return(res)  
        }
#----------------------------------------------------------------------------------------------
#calculate final efficiency scores based on updated z-differences  

rvk2 <- function(eff,id,coeff,z,lambda)
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
        
        #beregner justerte effektivitetstall
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
  #See Bjørndal, Bjørndal and Fange (2010).
  
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