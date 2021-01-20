##################################################################
##                       ETM- event rates                       ##
##################################################################




library(pacman)
p_load("tidyverse","Hmisc","rms","survival","haven","readxl","naniar","lubridate",
       "arsenal", "haven", "etm")

# get the dataset.

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte_vasc.csv')


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

# stroke

stroke_est = etmCIF(Surv(stroke_years_comp, comp_stroke != 0) ~ 1, 
              failcode = 1,
              etype = comp_stroke,
              data = df)



st = etm_to_df(stroke_est) %>% tbl_df()

st # now to save this dataset so that do not need to repeat the analysis.

write_csv(st,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\stroke_etm.csv')

# create CIF figure for the data.

tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\stroke_cif.tiff", height = 6, width = 6,
     units = "in", res = 1200)
plot(x = st$time, y = st$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
     col = "blue")

axis(2, at = c(0,5,10,15), 
     labels = c("0%","5%","10%",'15%'))


polygon(c(st$time, rev(st$time)),
        c(st$lower*100, rev(st$upper)*100),
        col = t_col("blue"), border = NA)

dev.off()



# PCI

pci_est = etmCIF(Surv(comp_pci_years,comp_pci_event != 0) ~ 1, 
                    failcode = 1,
                    etype = comp_pci_event,
                    data = df)



pci2 = etm_to_df(pci_est) %>% tbl_df()

pci2 # now to save this dataset so that do not need to repeat the analysis.

write_csv(pci2,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\pci_etm.csv')



tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\pci_cif.tiff", height = 6, width = 6,
     units = "in", res = 1200)

plot(x = pci2$time, y = pci2$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
     col = "blue")

axis(2, at = c(0,5,10,15), 
     labels = c("0%","5%","10%",'15%'))


polygon(c(pci2$time, rev(pci2$time)),
        c(pci2$lower*100, rev(pci2$upper)*100),
        col = t_col("blue"), border = NA)

dev.off()

# heart failure.


hf_est = etmCIF(Surv(hf_years_comp, hf_readmit_comp != 0) ~ 1, 
                 failcode = 1,
                 etype = hf_readmit_comp,
                 data = df)



hf = etm_to_df(hf_est) %>% tbl_df()

hf # now to save this dataset so that do not need to repeat the analysis.

write_csv(hf,
'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\hf_etm.csv')


# figure for hf


tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\hf_cif.tiff", height = 6, width = 6,
     units = "in", res = 1200)

plot(x = hf$time, y = hf$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
     col = "blue")

axis(2, at = c(0,2.5, 5), 
     labels = c("0%","2.5%","5%"))


polygon(c(hf$time, rev(hf$time)),
        c(hf$lower*100, rev(hf$upper)*100),
        col = t_col("blue"), border = NA)

dev.off()

# figure for vascular events.

vasc_est = etmCIF(Surv(comp_vasc_years, comp_vasc_event != 0) ~ 1, 
                failcode = 1,
                etype = comp_vasc_event,
                data = df)




vasc = etm_to_df(vasc_est) %>% tbl_df()


write_csv(vasc,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vasc_etm.csv')


# MI 

df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds_pci_hf_vte_vasc_mi.csv')


mi_est = etmCIF(Surv(comp_mi_years, comp_mi_event != 0) ~ 1, 
                  failcode = 1,
                  etype = comp_mi_event,
                  data = df)




mi = etm_to_df(mi_est) %>% tbl_df()


write_csv(mi,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\mi_etm.csv')





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


vte_est = etmCIF(Surv(vte_years_comp, vte_comp_event != 0) ~ 1, 
                failcode = 1,
                etype = vte_comp_event,
                data = df)




vte = etm_to_df(vte_est) %>% tbl_df()


write_csv(vte,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\vte_etm.csv')

