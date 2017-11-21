# Script for changing name of data for desired data-exports

vars = read.csv2(file="Variables_eng_nor_labels.csv", sep = "," , dec = ".")

#Change from English variablenames to English labels
cRC = colnames(RevCap) %in% vars$Variables
rV = vars$Variables %in% colnames(RevCap) 
colnames(RevCap)[cRC] = as.character(vars$Description[rV])

v = colnames(ld_ncs) %in% KeyFig$id
w = KeyFig$id %in% colnames(ld_ncs)
colnames(ld_ncs)[v] = KeyFig$comp[w]
colnames(ld_ncs)[v] = paste("ld_ncs", colnames(ld_ncs[v]), sep = "_")
v = colnames(rd_ncs) %in% KeyFig$id
w = KeyFig$id %in% colnames(rd_ncs)
colnames(rd_ncs)[v] = KeyFig$comp[w]
colnames(rd_ncs)[v] = paste("rd_ncs", colnames(rd_ncs[v]), sep = "_")

#Change from English variablenames to Norwegian labels



#Change from English variable names to Norwegian variable names
