##################################################################
##                       ETM- event rates  (PAD cohort)         ##
##################################################################

# this is the analysis of outcome according to the presence of PAD.
# PAD is an important factor in outcome and the COMPASS trial 
# demonstrates improvement in vascular event rates in patients with PAD.
# Hence, demonstrate baseline outcome of patients according to presence of PAD.


library(pacman)
p_load("tidyverse","Hmisc","rms","survival","haven","readxl","naniar","lubridate",
       "arsenal", "haven", "etm")

# get the dataset.

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte_vasc_mi.csv')


# function for df 

etm_to_df <- function(object, ci.fun = "cloglog", level = 0.95, ...) {
  l.X <- ncol(object$X)
  l.trans <- nrow(object[[1]]$trans)
  res <- list()
  for (i in seq_len(l.X)) {
    temp <- summary(object[[i]], ci.fun = ci.fun, level = level)
    res[[i]] <- data.table::rbindlist(
      temp[object$failcode + 1], idcol = "CIF"
    )[, CIF := paste0("CIF", CIF, "; ", names(object)[i])]
  }
  do.call(rbind, res)
}


# t_col ##############################

t_col <- function(color, percent = 80, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END

#Example - 
#mycol = t_col("blue")

######################

# see the patients according to PAD

df %>% count(pvd)
df$pvd[is.na(df$pvd)]<- 0
df %>% count(pvd)

prop.table(table(df$pvd))

# 0         1 
# 0.7545875 0.2454125 

# 

df$pvd = factor(df$pvd)


# stroke

stroke_pvd = etmCIF(Surv(stroke_years_comp, comp_stroke != 0) ~ pvd, 
                    failcode = 1,
                    etype = comp_stroke,
                    data = df)



st_pvd = etm_to_df(stroke_pvd) %>% tbl_df()

plot(stroke_pvd, ylim = c(0,0.2))

st_pvd # now to save this dataset so that do not need to repeat the analysis.

write_csv(st_pvd,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\stroke_pvd.csv')

# create CIF figure for the data.
# 
# tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\stroke_cif.tiff", height = 6, width = 6,
#      units = "in", res = 1200)
# plot(x = st$time, y = st$P*100, type = "l", xlab = "Time of Follow-up:Years",
#      ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
#      col = "blue")
# 
# axis(2, at = c(0,5,10,15), 
#      labels = c("0%","5%","10%",'15%'))
# 
# 
# polygon(c(st$time, rev(st$time)),
#         c(st$lower*100, rev(st$upper)*100),
#         col = t_col("blue"), border = NA)
# 
# dev.off()
# 


# PCI

pci_pvd = etmCIF(Surv(comp_pci_years,comp_pci_event != 0) ~ pvd, 
                 failcode = 1,
                 etype = comp_pci_event,
                 data = df)



pci2_pvd = etm_to_df(pci_pvd) %>% tbl_df()

pci2_pvd # now to save this dataset so that do not need to repeat the analysis.

write_csv(pci2_pvd,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\pci_pvd.csv')

# 
# 
# tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\pci_cif.tiff", height = 6, width = 6,
#      units = "in", res = 1200)
# 
# plot(x = pci2$time, y = pci2$P*100, type = "l", xlab = "Time of Follow-up:Years",
#      ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
#      col = "blue")
# 
# axis(2, at = c(0,5,10,15), 
#      labels = c("0%","5%","10%",'15%'))
# 
# 
# polygon(c(pci2$time, rev(pci2$time)),
#         c(pci2$lower*100, rev(pci2$upper)*100),
#         col = t_col("blue"), border = NA)
# 
# dev.off()
 
# # heart failure.
 
 
 hf_pvd = etmCIF(Surv(hf_years_comp, hf_readmit_comp != 0) ~ pvd,                  failcode = 1,
                 etype = hf_readmit_comp,
                data = df)
 
 
 
 hf_pvd2 = etm_to_df(hf_pvd) %>% tbl_df()
 
 hf # now to save this dataset so that do not need to repeat the analysis.
 
 write_csv(hf_pvd2,
           'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\hf_pvd.csv')


# figure for hf
# 
# 
# tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\hf_cif.tiff", height = 6, width = 6,
#      units = "in", res = 1200)
# 
# plot(x = hf$time, y = hf$P*100, type = "l", xlab = "Time of Follow-up:Years",
#      ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
#      col = "blue")
# 
# axis(2, at = c(0,2.5, 5), 
#      labels = c("0%","2.5%","5%"))
# 
# 
# polygon(c(hf$time, rev(hf$time)),
#         c(hf$lower*100, rev(hf$upper)*100),
#         col = t_col("blue"), border = NA)
# 
# dev.off()

# figure for vascular events.

vasc_pvd = etmCIF(Surv(comp_vasc_years, comp_vasc_event != 0) ~ pvd, 
                  failcode = 1,
                  etype = comp_vasc_event,
                  data = df)




vasc_pvd2 = etm_to_df(vasc_pvd) %>% tbl_df()


write_csv(vasc_pvd2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vasc_pvd.csv')


# MI 

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte_vasc_mi.csv')


mi_pvd = etmCIF(Surv(comp_mi_years, comp_mi_event != 0) ~ pvd, 
                failcode = 1,
                etype = comp_mi_event,
                data = df)




mi_pvd2 = etm_to_df(mi_pvd) %>% tbl_df()


write_csv(mi_pvd2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\mi_pvd.csv')





# obtain binomial proportions for the trial results at 3 years using the Wilson method for CI.

# mi 


x = c(16,2,7,7,10,8,5,54)
n = rep(483,8)


trial_res = binconf(x = x, n = n,
                    include.x = T, include.n = T, return.df = T)

trial_res = trial_res %>% tbl_df()

trial_res

write_csv(trial_res,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\trial_res.csv'    )


# VTE results CIF.


vte_pvd = etmCIF(Surv(vte_years_comp, vte_comp_event != 0) ~ pvd, 
                 failcode = 1,
                 etype = vte_comp_event,
                 data = df)




vte_pvd2 = etm_to_df(vte_pvd) %>% tbl_df()


write_csv(vte_pvd2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vte_pvd.csv')

# mortality for PAD 

pad_surv = survfit(Surv(fupyears, died) ~ pvd, data = df)

summary(pad_surv, times = c(0,1,3,5))

# calculate RMST for the data.

library(survRM2)

pad_rmst5 = rmst2(time = df$fupyears,
                 status = df$died,
                 arm = df$pvd,
                 tau = 5)
pad_rmst5


# The truncation time: tau = 5  was specified. 
# 
# Restricted Mean Survival Time (RMST) by arm 
# Est.    se lower .95 upper .95
# RMST (arm=1) 4.428 0.021     4.386     4.470
# RMST (arm=0) 4.632 0.010     4.612     4.652
# 
# 
# Restricted Mean Time Lost (RMTL) by arm 
# Est.    se lower .95 upper .95
# RMTL (arm=1) 0.572 0.021     0.530     0.614
# RMTL (arm=0) 0.368 0.010     0.348     0.388
# 
# 
# Between-group contrast 
# Est. lower .95 upper .95 p
# RMST (arm=1)-(arm=0) -0.204    -0.250    -0.157 0
# RMST (arm=1)/(arm=0)  0.956     0.946     0.966 0
# RMTL (arm=1)/(arm=0)  1.553     1.418     1.701 0

pad_rmst10 = rmst2(time = df$fupyears,
                  status = df$died,
                  arm = df$pvd,
                  tau = 10)
pad_rmst10

# The truncation time: tau = 10  was specified. 
# 
# Restricted Mean Survival Time (RMST) by arm 
# Est.    se lower .95 upper .95
# RMST (arm=1) 7.599 0.057     7.488     7.710
# RMST (arm=0) 8.336 0.031     8.275     8.396
# 
# 
# Restricted Mean Time Lost (RMTL) by arm 
# Est.    se lower .95 upper .95
# RMTL (arm=1) 2.401 0.057     2.290     2.512
# RMTL (arm=0) 1.664 0.031     1.604     1.725
# 
# 
# Between-group contrast 
# Est. lower .95 upper .95 p
# RMST (arm=1)-(arm=0) -0.737    -0.863    -0.610 0
# RMST (arm=1)/(arm=0)  0.912     0.897     0.927 0
# RMTL (arm=1)/(arm=0)  1.443     1.360     1.530 0


# to check with HR

summary(coxph(Surv(fupyears, died) ~ pvd, data = df)) # amazing, 
# small changes between -group RMST ratio and HR from the Cox model.

