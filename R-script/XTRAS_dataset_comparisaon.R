#### Dataframe Comparison script ####

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
my.path = "C:\\Users\\ens\\Jottacloud\\GitHub\\IRiR"
setwd(my.path)
# Load benchmarking package of Bogetoft & Otto
# Følgende pakker benyttes
# Benchmarking, xlsx, plyr, dplyr
library(Benchmarking)
library(xlsx)
library(xlsxjars)
library(plyr)
library(dplyr)
library(FactoMineR) # Kan fjernes?
library(outliers)
library(plot3D)

source("./R-script/functions_nve.R")
# Ønsker å vise store tall som fulle verdier, ikke som potenser
options(scipen = 100)

df1 = read.csv("./Data/Grunnlagsdata/Grunnlagsdata_faktiskvarsel.csv",sep=",") # INSERT PATH FOR DATASET NR1

df2 = read.csv("./Data/Grunnlagsdata/Grunnlagsdata_faktiskvarsel_2.csv",sep=",") # INSERT PATH FOR DATASET NR2


# #### Insert IDs ####
# id = read.csv("./Data/Grunnlagsdata/id.csv", sep = ",")
# # Tilegner ID-er til Grunnlagsdata vha merge
# df1 = merge.data.frame(df1, id, by = "orgnr", all.x = TRUE)
# df1$selskap <- as.character(df1$selskap)
# df1$navn<- as.character(df1$navn)
# 
# # Legger manuelt til IDer til selskapene som mangler
# # Angir ny ID for Gassco
# df1$id[df1$orgnr == 983452841] <- 900
# df1$navn[df1$orgnr == 983452841] <- "Gassco"
# # Angir ny ID for Lyse sentralnett
# df1$id[df1$orgnr == 996325458] <- 872
# df1$navn[df1$orgnr == 996325458] <- "Lyse Sentralnett"
# # Angir ny ID for Mørenett
# df1$id[df1$orgnr == 912631532] <- 460
# df1$navn[df1$orgnr == 912631532] <- "Morenett"
# 
# # Lager idaar og orgnraar variabler og endrer type for disse
# df1$idaar <- paste(df1$id, df1$aar, sep="")
# df1$orgnraar <- paste(df1$aar, df1$orgnr, sep="")
# df1$idaar <- as.numeric(df1$idaar)
# df1$orgnraar <- as.numeric(df1$orgnraar)
# 
# 
# # Tilegner ID-er til Grunnlagsdata vha merge
# df2 = merge.data.frame(df2, id, by = "orgnr", all.x = TRUE)
# df2$selskap <- as.character(df2$selskap)
# df2$navn<- as.character(df2$navn)
# 
# # Legger manuelt til IDer til selskapene som mangler
# # Angir ny ID for Gassco
# df2$id[df2$orgnr == 983452841] <- 900
# df2$navn[df2$orgnr == 983452841] <- "Gassco"
# # Angir ny ID for Lyse sentralnett
# df2$id[df2$orgnr == 996325458] <- 872
# df2$navn[df2$orgnr == 996325458] <- "Lyse Sentralnett"
# # Angir ny ID for Mørenett
# df2$id[df2$orgnr == 912631532] <- 460
# df2$navn[df2$orgnr == 912631532] <- "Morenett"
# 
# # Lager idaar og orgnraar variabler og endrer type for disse
# df2$idaar <- paste(df2$id, df2$aar, sep="")
# df2$orgnraar <- paste(df2$aar, df2$orgnr, sep="")
# df2$idaar <- as.numeric(df2$idaar)
# df2$orgnraar <- as.numeric(df2$orgnraar)
# 

#### Discrepancy checks ####

df1$label = "ORG"
df2$label = "MOD"

TOTdf = rbind(df1, df2)

dupRows = dupsBetweenGroups(TOTdf, "label")
TOTdf$differs = !dupRows

DISCREPANCYdf = subset(TOTdf, differs == TRUE)

rm(df1,df2)


df1.disc = subset(DISCREPANCYdf, label == "ORG")
df2.disc = subset(DISCREPANCYdf, label == "MOD")

discrep.sum = mapply(setdiff, df1.disc, df2.disc)
discrep.sum
