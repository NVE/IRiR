## IRiR

#### R-teknisk oppsett ####

# Remove all objects from memory
remove(list=ls())
# Get current working directory
getwd()
# Set working directory to where the data file is located
# The address can be copied from the address bar in Windows Explorer
# Remember to change "\" to "/" or "\\" 
#my.path = "C:\\users\\roam\\Dropbox\\IRcalc i R"
my.path = "C:\\Users\\MOHH\\Documents\\GitHub\\IRiR"
setwd(my.path)
# Load benchmarking package of Bogetoft & Otto
library(Benchmarking)
library(xlsx)
source("functions_nve.R")
# Ønsker å vise store tall som fulle verdier, ikke som potenser
options(scipen = 100)

#### Grunnlag for DEA ####

source("1_0_Config_Assumptions_Data.R")

source("1_1_Calculated_Input_Values.R")

source("1_2_Company_Selection.R")

source("1_3_Input_Data_DEA.R")


#### Trinn 1 - DEA-kjøringer ####

#Runder av data til DEA til "hele tusen"  
x.sf.d$sf_d_TOTXDEA <- round(x.sf.d$sf_d_TOTXDEA, digits = 0)  
y.sf.d$sf_d_ab  <- round(y.sf.d$sf_d_ab, digits = 0)  
y.sf.d$sf_d_ns <- round(y.sf.d$sf_d_ns, digits = 0)  
y.sf.d$sf_d_hs <- round(y.sf.d$sf_d_hs, digits = 0) 

# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i x.snitt.r og y.snitt.r som tilvhører selskapene i front.r
res.tmp1 = dea(X=x.snitt.r,Y=y.snitt.r,XREF=x.snitt.r[as.character(front.r)],YREF=y.snitt.r[as.character(front.r),],RTS="crs")
#plot(sort(res.snitt.snitt.r$eff))

# Spesialkjøring for selskaper som bare kan være front for seg selv
eff.snitt.snitt.r = res.tmp1$eff
lambda.snitt.snitt.r = cbind(res.tmp1$lambda,matrix(NA,nrow=nrow(res.tmp1$lambda),ncol=length(sep.eval.r)))
colnames(lambda.snitt.snitt.r) = c(front.r,sep.eval.r)
for(i in sep.eval.r)
  {
  res.tmp2 = dea(X=x.snitt.r,Y=y.snitt.r,RTS="crs",XREF=x.snitt.r[as.character(c(front.r,i))],YREF=y.snitt.r[as.character(c(front.r,i)),])
  eff.snitt.snitt.r[as.character(i)] = res.tmp2$eff[as.character(i)] 
  for(j in c(front.r,i))
    lambda.snitt.snitt.r[as.character(i),as.character(j)] = res.tmp2$lambda[as.character(i),paste("L_",as.character(j),sep="")]
  }
remove(res.tmp1)
remove(res.tmp2)

plot(sort(eff.snitt.snitt.r))
View(lambda.snitt.snitt.r)




####  Trinn 2 - RVK-justering vha regresjon ####

# #correct for environmental effects
res.stage2 = two.stage(x.snitt,z.snitt,res.snitt.snitt.r$eff,res.snitt.snitt.r$lambda) 
plot(sort(res.stage2$eff.corr.NVE))
# 


#### Trinn 3 - Kalibrering av kostnadsnormer ####
# #kalibrerer kostnadsnormer basert på avkastningsgrunnlag
res.stage3 = calibrate(res.faktisk.snitt.r$eff,x.faktisk,weight=kap.faktisk)
# #vis gjennomsnittlig kostnadsvektet effektivitet
res.stage3$industry.avg
# #plott kalibrerte effektivitetstall
plot(sort(res.stage3$eff.cal))
# #sjekk at sum kalibrert kostnadsnorm = sum kostnad
x%*%res.stage3$eff.cal
sum(x)

source("X_4_Excel_export.R")
