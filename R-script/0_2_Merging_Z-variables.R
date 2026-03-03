
#### 0_2 Merging Z-variables ####
#### Import data from previous notice ####
load(file = "./Data/geo_variables.Rdata")
load(file = "./Data/costs_t_2.Rdata")

# Setting pnl.rc (price of network loss) from RC-calculation two years prior to ap.t_2 (area price)
ap.t_2 <- cb.y_ex_cap %>% select (c(id, pnl.rc))
names(ap.t_2)[2] <- "ap.t_2"
ap.t_2$ap.t_2 <- ap.t_2$ap.t_2/1000

# Setter ap.t_2 manuelt for Linja
ap.t_2 <- ap.t_2 %>% 
  mutate(ap.t_2 = if_else(id == 269, 0.3837, 
                          if_else(id == 275, 0.54424, ap.t_2)))

dat <- merge.data.frame(dat, ap.t_2, by = "id", all.x = TRUE)

# Defining variables that are summed for all merging companies
harm.var_sum = c("ldz_n.mgc_sum")

# Defining variables weighted with number of map grid cells
dat_geo <- dat_geo %>% dplyr::rename(y = y.cb)
dat_geo$id.y <- paste(dat_geo$id, dat_geo$y, sep="")
ldz_harm.var_gc <- startsWith(colnames(dat_geo), prefix = "ldz")
ldz_harm.var_gc <- colnames(dat_geo)[ldz_harm.var_gc]

# Defining merging companies
fusjon_Linja = c(912631532, 925315958) # Linja AS = Linja AS + Tindra AS
fusjon_Area = c(923993355, 924934867, 924868759) # Area AS = Area AS + Luostejok Nett AS + Lega Nett AS
fusjon_Foie = c(971589752, 923050612, 987626844) # F??ie AS = Hallingdal Kraftnett AS + F??ie AS + Hemsil Nett AS

list <- list() 

list$Linja = merge_ldz_rdz(comps = fusjon_Linja, new.org = 912631532, new.id = 269, new.name = "fusjonert_Linja", sum_variables = harm.var_sum, ldz_weighted.var = ldz_harm.var_gc, df = dat_geo)$mds
list$Area = merge_ldz_rdz(comps = fusjon_Area, new.org = 923993355, new.id = 164, new.name = "fusjonert_Area", sum_variables = harm.var_sum, ldz_weighted.var = ldz_harm.var_gc, df = dat_geo)$mds
list$Foie = merge_ldz_rdz(comps = fusjon_Foie, new.org = 971589752, new.id = 275, new.name = "fusjonert_Foie", sum_variables = harm.var_sum, ldz_weighted.var = ldz_harm.var_gc, df = dat_geo)$mds

# Combining all rows in the list into one data frame 
merged_ldz <- do.call("rbind", list) 
merged_ldz <- merged_ldz[,c(2,3,1,5,6,7,8,9,10,11,12,13,14,15,16,4)]

# Deleting companies who have merged (with IDs equal to IDs of merged companies)
dat_geo = dat_geo[!(dat_geo$id %in% merged_ldz$id),] 

# Adding newly merged companies 
# merged_ldz = merged_ldz %>% select(-c(comp, name))
dat_geo <- rbind(dat_geo, merged_ldz) 

# Merging data
dat_geo = dat_geo %>% select(-c(orgn, y, id.y))
dat <- merge.data.frame(dat, dat_geo, by = "id", all.x = T)

# Creating ldz_prod
ldz_prod = dat[dat$y == y.cb, c("orgn","ldz_innmat_midl","ldz_le")]
ldz_prod$ldz_prod = ldz_prod$ldz_innmat_midl /ldz_prod$ldz_le
ldz_prod$ldz_prod[!is.finite(ldz_prod$ldz_prod)] <- 0 # Removing NaN and Inf values
ldz_prod = ldz_prod %>% select(-c(ldz_innmat_midl, ldz_le))
dat <- merge.data.frame(dat, ldz_prod, by = "orgn", all.x = T )

print(unique(dat[is.na(dat$ldz_mgc), "comp" ]))
length(dat[is.na(dat)])

# Deleting NAs
dat[is.na(dat)] <- 0

# Sorting data
idasvector <- as.vector(dat$id)
dat <- dat[order(idasvector),]
rm(idasvector, ldz_prod, merged_ldz, list)

