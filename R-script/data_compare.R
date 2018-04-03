# Om man ikke har brukt "daff"-pakken kjør: install.packages("daff")
library(daff)


# df1 = read.csv() # Choose dataset desired dataset
# 
# df2 = read.csv() # Choose dataset to compare

#Code snippet below is for demo purpose only
df1 = dat
df2 =  dat
df2[df2$id.y == 92016, "ld_dep.sf"] = 2500 #Changed from 5502 to 2500

#Use function diff_data ftom daff-package
diffs = diff_data(df1, df2)
summary(diffs)
