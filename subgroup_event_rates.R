##################################################################
##                    Event rates- subgroups                    ##
##################################################################

# age > 65 years
# PAD
# HF
# CKD

library(pacman)
p_load(tidyverse, survival, rms, Hmisc, etm, survminer, ggsci,cmprsk)


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


# age > 65 years.

df = 
read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_allevents_bleed.csv')

glimpse(df)

df %>% count(hf)

# age 

df$Age = with(df, ifelse(age >= 65, 1, 0))

df %>% count(Age)

# now to do the analysis according to Age.

stroke_age = survfit(Surv(stroke_years_comp, comp_stroke, type = "mstate") ~ Age,
                     data = df)

summary(stroke_age, times = c(0,1,3,5))

# use etm to obtain values with CI; then plot using etm data.


stroke_age = etmCIF(Surv(stroke_years_comp, comp_stroke != 0) ~ Age, 
                    failcode = 1,
                    etype = comp_stroke,
                    data = df)



st_age = etm_to_df(stroke_age) %>% tbl_df()

# need to add the survival lines too.

age_survfit = survfit(Surv(fupyears, died) ~ Age, data = df)

age_s = broom::tidy(age_survfit)

st # now to save this dataset so that do not need to repeat the analysis.

write_csv(st_age,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\stroke_age.csv')

# create CIF figure for the data.

stage0 = st_age %>% filter(CIF == "CIF0 1; Age=0")
stage1 = st_age %>% filter(CIF == "CIF0 1; Age=1")

age_s0 = age_s %>% filter(strata == "Age=0")
age_s1 = age_s %>% filter(strata == "Age=1")

tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\stroke_age.tiff", 
     height = 5, width = 7,
     units = "in", res = 1200)


plot(x = stage0$time, y = stage0$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
     col = "blue", ylim = c(0,20))

axis(2, at = c(0,5, 10,15, 20), 
     labels = c("0%","5%","10%","15%", "20%"))


polygon(c(stage0$time, rev(stage0$time)),
        c(stage0$lower*100, rev(stage0$upper)*100),
        col = t_col("blue"), border = NA)


lines(x = stage1$time, y = stage1$P*100, col = "red")


polygon(c(stage1$time, rev(stage1$time)),
        c(stage1$lower*100, rev(stage1$upper)*100),
        col = t_col("red"), border = NA)

lines(x = age_s0$time,
      y = (1 - age_s0$estimate)*100,
      col = "blue", lty = 2)



lines(x = age_s1$time,
      y = (1 - age_s1$estimate)*100,
      col = "red", lty = 2)


dev.off()

# Gray test for the data.
# use pdata to weight and then we can use Coxph to do the Fine and Gray model.

df$fg_stroke = factor(df$comp_stroke, 0:2, labels = c("censor", "stroke","death"))

pdata <- finegray(Surv(stroke_years_comp, fg_stroke) ~ ., data = df)

fg_stroke <- coxph(Surv(fgstart, fgstop, fgstatus) ~ Age, data = pdata,
                   weight = fgwt)

# p = 0.2

# MI


mi_age = survfit(Surv(comp_mi_years, comp_mi_event, type = "mstate") ~ Age,
                     data = df)

summary(mi_age, times = c(0,1,3,5))



mi_age = etmCIF(Surv(comp_mi_years, comp_mi_event != 0) ~ Age, 
                    failcode = 1,
                    etype = comp_mi_event,
                    data = df)



mi_age = etm_to_df(mi_age) %>% tbl_df()

# crr for Gray test.

age_mi = crr(ftime = df$comp_mi_years,
             fstatus = df$comp_mi_event,
             cencode = 0,
             failcode = 1,
             cov1 = df$Age)


summary(age_mi)

# Competing Risks Regression
# 
# Call:
#   crr(ftime = df$comp_mi_years, fstatus = df$comp_mi_event, cov1 = df$Age, 
#       failcode = 1, cencode = 0)
# 
# coef exp(coef) se(coef)     z p-value
# df$Age1 -0.251     0.778     0.06 -4.19 2.8e-05
# 
# exp(coef) exp(-coef)  2.5% 97.5%
#   df$Age1     0.778       1.29 0.691 0.875
# 
# Num. cases = 15586
# Pseudo Log-likelihood = -10588 
# Pseudo likelihood ratio test = 17.1  on 1 df,

# now to save this dataset so that do not need to repeat the analysis.

write_csv(mi_age,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\mi_age.csv')

# create CIF figure for the data.

miage0 = mi_age %>% filter(CIF == "CIF0 1; Age=0")
miage1 = mi_age %>% filter(CIF == "CIF0 1; Age=1")

tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\mi_age.tiff", 
     height = 5, width = 7,
     units = "in", res = 1200)


plot(x = miage0$time, y = miage0$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)",   
     col = "blue", xlim = c(0,5), ylim = c(0,10), yaxt= "n")

axis(2, at = c(0,2.5, 5,7.5, 10), 
     labels = c("0%","2.5%","5%","7.5%", "10%"))


polygon(c(miage0$time, rev(miage0$time)),
        c(miage0$lower*100, rev(miage0$upper)*100),
        col = t_col("blue"), border = NA)


lines(x = miage1$time, y = miage1$P*100, col = "red")


polygon(c(miage1$time, rev(miage1$time)),
        c(miage1$lower*100, rev(miage1$upper)*100),
        col = t_col("red"), border = NA)


dev.off()

# get pdata and then FG test.


df$fg_mi = factor(df$comp_mi_event, 0:2, labels = c("censor", "mi","death"))

pdata_mi <- finegray(Surv(comp_mi_years, fg_mi) ~ ., data = df)

fg_mi <- coxph(Surv(fgstart, fgstop, fgstatus) ~ Age, data = pdata_mi,
                   weight = fgwt)

summary(fg_mi) # p < 0.001

# CKD.

describe(df$egfr)

# create variable CKD 

df$ckd = with(df, ifelse(egfr < 60, 1, 0))

df$ckd = factor(df$ckd)

# stroke ---


stroke_ckd = survfit(Surv(stroke_years_comp, comp_stroke, type = "mstate") ~ ckd,
                     data = df)

summary(stroke_ckd, times = c(0,1,3,5))

# use etm to obtain values with CI; then plot using etm data.


stroke_ckd = etmCIF(Surv(stroke_years_comp, comp_stroke != 0) ~ ckd, 
                    failcode = 1,
                    etype = comp_stroke,
                    data = df)



st_ckd = etm_to_df(stroke_ckd) %>% tbl_df()

 # now to save this dataset so that do not need to repeat the analysis.

write_csv(st_ckd,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\stroke_ckd.csv')



# create CIF figure for the data.

st_ckd0 = st_ckd %>% filter(CIF == "CIF0 1; ckd=0")
st_ckd1 = st_ckd %>% filter(CIF == "CIF0 1; ckd=1")


tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\stroke_ckd.tiff", 
     height = 5, width = 7,
     units = "in", res = 1200)


plot(x = st_ckd0$time, y = st_ckd0$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)", xlim = c(0,5),  frame.plot = F, yaxt = "n",
     col = "blue", ylim = c(0,10))

axis(2, at = c(0,2.5, 5,7.5, 10), 
     labels = c("0%","2.5%","5%","7.5%", "10%"))


polygon(c(st_ckd0$time, rev(st_ckd0$time)),
        c(st_ckd0$lower*100, rev(st_ckd0$upper)*100),
        col = t_col("blue"), border = NA)


lines(x = st_ckd1$time, y = st_ckd1$P*100, col = "red")


polygon(c(st_ckd1$time, rev(st_ckd1$time)),
        c(st_ckd1$lower*100, rev(st_ckd1$upper)*100),
        col = t_col("red"), border = NA)


dev.off()

# run FG test to get the p-value (Gray test)



fg_stroke_ckd <- crr(ftime = df$stroke_years_comp,
                     fstatus = df$comp_stroke,
                     failcode = 1,
                     cencode = 0,
                     cov1 = df$ckd)

summary(fg_stroke_ckd)


# > summary(fg_stroke_ckd)
# Competing Risks Regression
# 
# Call:
#   crr(ftime = df$stroke_years_comp, fstatus = df$comp_stroke, cov1 = df$ckd, 
#       failcode = 1, cencode = 0)
# 
# coef exp(coef) se(coef)    z p-value
# df$ckd1 0.102      1.11   0.0598 1.71   0.088
# 
# exp(coef) exp(-coef)  2.5% 97.5%
#   df$ckd1      1.11      0.903 0.985  1.25
# 
# Num. cases = 15586
# Pseudo Log-likelihood = -13039 
# Pseudo likelihood ratio test = 2.86  on 1 df,



# p = 0.08.

# MI ---

mi_ckd = survfit(Surv(comp_mi_years, comp_mi_event, type = "mstate") ~ ckd,
                 data = df)

summary(mi_ckd, times = c(0,1,3,5))



mi_ckd = etmCIF(Surv(comp_mi_years, comp_mi_event != 0) ~ ckd, 
                failcode = 1,
                etype = comp_mi_event,
                data = df)



mi_ckd = etm_to_df(mi_ckd) %>% tbl_df()

# now to save this dataset so that do not need to repeat the analysis.

write_csv(mi_ckd,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\mi_ckd.csv')

# create CIF figure for the data.

mickd0 = mi_ckd %>% filter(CIF == "CIF0 1; ckd=0")
mickd1 = mi_ckd %>% filter(CIF == "CIF0 1; ckd=1")

tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\mi_ckd.tiff", 
     height = 5, width = 7,
     units = "in", res = 1200)


plot(x = mickd0$time, y = mickd0$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)",   
     col = "blue", xlim = c(0,5), ylim = c(0,10), yaxt= "n")

axis(2, at = c(0,2.5, 5,7.5, 10), 
     labels = c("0%","2.5%","5%","7.5%", "10%"))


polygon(c(mickd0$time, rev(mickd0$time)),
        c(mickd0$lower*100, rev(mickd0$upper)*100),
        col = t_col("blue"), border = NA)


lines(x = mickd1$time, y = mickd1$P*100, col = "red")


polygon(c(mickd1$time, rev(mickd1$time)),
        c(mickd1$lower*100, rev(mickd1$upper)*100),
        col = t_col("red"), border = NA)


dev.off()

# get Gray test using crr.



fg_mi_ckd <- crr(ftime = df$comp_mi_years,
                     fstatus = df$comp_mi_event,
                     failcode = 1,
                     cencode = 0,
                     cov1 = df$ckd)

summary(fg_mi_ckd)

# > summary(fg_mi_ckd)
# Competing Risks Regression
# 
# Call:
#   crr(ftime = df$comp_mi_years, fstatus = df$comp_mi_event, cov1 = df$ckd, 
#       failcode = 1, cencode = 0)
# 
# coef exp(coef) se(coef)    z p-value
# df$ckd1 0.34       1.4   0.0634 5.35 8.6e-08
# 
# exp(coef) exp(-coef) 2.5% 97.5%
#   df$ckd1       1.4      0.712 1.24  1.59
# 
# Num. cases = 15586
# Pseudo Log-likelihood = -10583 
# Pseudo likelihood ratio test = 27.3  on 1 df,


# HF

df %>% count(hf)


# look at all-cause mortality according to hf.


survfit_hf = survfit(Surv(fupyears, died) ~ hf, data = df)


tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\heart_failure_mortality.tiff",
     height = 7, width = 7, res = 1200, units = "in")

ggsurvplot(survfit_hf,
           fun = "event",
           risk.table = F,
           surv.scale = "percent",
           xlim = c(0,5),
           conf.int = T,
           censor.size = 0,
           data = df,
           palette = "lancet",
           ylim = c(0,0.3),
           legend.labs = c("HF-ve","HF+ve"),
           break.x.by = 1,
           break.y.by = 0.05,
           cumevents = T, 
           xlab = "Follow-up Time:Years",
           ylab = "Cumulative Mortality",
           pval = T,
           fontsize = 3,
           tables.col = "strata")



dev.off()

# stroke ...


stroke_hf = etmCIF(Surv(stroke_years_comp, comp_stroke != 0) ~ hf, 
                    failcode = 1,
                    etype = comp_stroke,
                    data = df)



st_hf = etm_to_df(stroke_hf) %>% tbl_df()



write_csv(st_hf,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\st_hf.csv')

# create CIF figure for the data.

sthf0 = st_hf %>% filter(CIF == "CIF0 1; hf=0")
sthf1 = st_hf %>% filter(CIF == "CIF0 1; hf=1")

# overall survival according to hf.

survfit_hf = survfit(Surv(fupyears, died) ~ hf, data = df)

surv_hf = broom::tidy(survfit_hf)

surv_hf0 = surv_hf[surv_hf$strata == "hf=0", ]

surv_hf1 = surv_hf[surv_hf$strata == "hf=1", ]

tiff("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\figures\\st_hf.tiff", 
     height = 5, width = 7,
     units = "in", res = 1200)


plot(x = sthf0$time, y = sthf0$P*100, type = "l", xlab = "Time of Follow-up:Years",
     ylab = "Cumulative Incidence (%)", frame.plot = F,  
     col = "blue", xlim = c(0,5), ylim = c(0,30), yaxt= "n")

axis(2, at = c(0,5,10,15,20,25,30), 
     labels = c("0%","5%","10%","15%","20%","25%","30%"))


polygon(c(sthf0$time, rev(sthf0$time)),
        c(sthf0$lower*100, rev(sthf0$upper)*100),
        col = t_col("blue"), border = NA)


lines(x = sthf1$time, y = sthf1$P*100, col = "red")


polygon(c(sthf1$time, rev(sthf1$time)),
        c(sthf1$lower*100, rev(sthf1$upper)*100),
        col = t_col("red"), border = NA)

# need to also add the lines for cumulative mortality here...

lines(x = surv_hf0$time, y = (1 - surv_hf0$estimate)*100, col = "blue", lty = 2)

lines(x = surv_hf1$time, y = (1 - surv_hf1$estimate)*100, col = "red", lty = 2)


# survival event summary.


s <- survfit(Surv(fupyears, died) ~ 1, data = df)


summary(s, times = c(1,3,5))

# > summary(s, times = c(1,3,5))
# Call: survfit(formula = Surv(fupyears, died) ~ 1, data = df)
# 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
# 1  14920     621    0.960 0.00157        0.957        0.963
# 3  11477     800    0.904 0.00243        0.899        0.909
# 5   7702     786    0.832 0.00334        0.826        0.839
