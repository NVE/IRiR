
#### 0_1 Configuration, assumptions and data import ####
forutsetninger = load(file="./Data/forutsetninger.Rdata")

#### Import data ####
dat = read.xlsx("./Data/BaseData/irBase - Stata - 27.11.2025 09_53_35.xlsx")
kraftpris = read.xlsx("./Data/BaseData/kraftpris2026.xlsx") 
id = read.xlsx("./Data/BaseData/id_ir_26.xlsx") 

dat <- dat %>% dplyr::rename(orgn = id,
                            y = aar,
                            comp = selskap,
                            ld_usla = d_usla,
                            rd_impl = r_impl,
                            ld_impl = d_impl,
                            t_impl = s_impl,
                            rd_usla = r_usla,
                            ld_dep.sf = d_avs,
                            rd_dep.sf = r_avs,
                            t_dep.sf = s_avs,
                            rd_nl = r_nettap,
                            ld_nl = d_nettap,
                            t_pens = s_pensj,
                            t_sal = s_lonn,
                            t_sal.cap = s_lonnakt,
                            t_pens.eq = s_pensjek,
                            rd_sal = r_lonn,
                            rd_sal.cap = r_lonnakt,
                            rd_pens = r_pensj,
                            rd_pens.eq = r_pensjek,
                            ld_pens = d_pensj,
                            ld_sal = d_lonn,
                            ld_sal.cap = d_lonnakt,
                            ld_pens.eq = d_pensjek,
                            ld_gci = d_grs,
                            rd_wv.ol = r_vluft,
                            rd_wv.uc = r_vjord,
                            rd_wv.sc = r_vsjo,
                            rd_wv.ss = r_grmva,
                            ld_sub = d_ab,
                            ld_bv.sf = d_bfv,
                            rd_bv.sf = r_bfv,
                            t_bv.sf = s_bfv,
                            ld_ss = d_ns,
                            ld_hvoh = d_hsll,
                            ld_hv = d_hs,
                            ld_cens = d_kile,
                            rd_cens = r_kile,
                            t_cens = s_kile,
                            ld_cga = d_utred,
                            rd_dep.gf = r_abavs,
                            rd_bv.gf = r_abbfv,
                            ld_dep.gf = d_abavs,
                            ld_bv.gf = d_abbfv,
                            ld_391 = d_391,
                            rd_391 = r_391,
                            t_391 = s_391,
                            ld_hvug = d_hsjord,
                            ld_hvsc = d_hssjo,
                            ld_OPEXxS = d_DVxL,
                            rd_OPEXxS = r_DVxL,
                            t_OPEXxS = s_DVxL,
                            rd_cga = r_utred,
                            ld_elhub = d_elhub,
                            rd_elhub = r_elhub,
                            t_elhub =s_elhub, 
                            ldz_innmat_midl = d_innmat_midl,
                            ldz_le = d_le,
                            rd_cga_tidl = r_nettutdr,
                            rd_coord= r_koordin
                            )

        dat$orgn <- as.integer(dat$orgn)
        
        # Deleting companies with no revenue cap
        dat <- dat[!(dat$orgn==879867762),] # Andersen Web Consulting
        if (Statnett_calc){
          print("Statnett er nC% med analyse for regionalt distribusjonsnett. Skal kun vC&re med i inntektsrammeberegning for Statnett")
        }else{ dat <- dat[!(dat$orgn==962986633),] # Statnett}
        }
        dat <- dat[!(dat$orgn==995204517),] # Nordlink
        dat <- dat[!(dat$orgn==954090493),] # Sira Kvina
        dat <- dat[!(dat$orgn==996732673),] # Norske Skog Skogn
        dat <- dat[!(dat$orgn==984015666),] # Yara Norge
        
        dat = merge.data.frame(dat, kraftpris, by = "orgn", all.x = TRUE)

        id$orgn <- as.integer(id$orgn)
        id$name = NULL
        dat = merge.data.frame(dat, id, by = "orgn", all.x = TRUE)
        dat$comp <- as.character(dat$comp)
        dat$id.y <- paste(dat$id, dat$y, sep="")     # Create id.y variable
        dat$id.y <- as.numeric(dat$id.y)
        dat$orgn.y <- paste(dat$y, dat$orgn, sep="") # Create orgn.y variable
        dat$orgn.y <- as.numeric(dat$orgn.y)
        
        
# Check for companies without ID
        missing.id <- dat[is.na(dat$id),]
        unique(missing.id[c("comp", "orgn")])
        stopifnot(nrow(missing.id) == 0)
        rm(missing.id, id)

# Replace NA values with zeros to have correct means for pensions costs
        dat[is.na(dat)] = 0
      
# Sorting data
      idasvector <- as.vector(dat$id)
      dat <- dat[order(idasvector),]
      rm(idasvector)
      
      