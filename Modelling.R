library(dlnm)
library(splines)
library(dplyr)
library(data.table)
library(mixmeta)
library(zoo)
library(survival)
library(ggplot2)

#### Variable definition ####
#####cb_fire: crossbasis function of wildfire-specific PM2.5
#####cb_nonfire: crossbasis function of non-wildfire PM2.5
#####case: indicating visiting ED (1) or not visiting ED (0)
#####nonfire_mvavg_7: the 7-day moving average of non-wildfire PM2.5 
#####nonfire_mvavg_6: the 6-day moving average of non-wildfire PM2.5 
#####temp_mvavg_7: the 7-day moving average of daily mean temperature
#####rhum_mvavg_7: the 7-day moving average of daily relative humidity
#####fire_mvavg_6: the 6-day moving average of daily wildfire-specific PM2.5
#####holiday: public holiday (0 or 1)
#####id: the ID of the patients
#####increase_unit: the unit of increase in wildfire-specific PM2.5 or non-wildfire PM2.5 (e.g., increase_unit = 1)


# lag model
lagday <- 7 ## length of lag, use 7 to explore the specific lag of effect
df_lag <- 3 ## degree of freedom used in lag-response dimension of the crossbasis function
data <- ED_data ## ED_data is the health data used in the case-crossover analysis
lag_start <- 5 ## the starting column of the exposure variable (i.e., wildfire-specific PM2.5)
lag_end <- 12 ## the ending column of the exposure variable (i.e., wildfire-specific PM2.5)
outcome <- "depression" ## the sub-type of mental disorders investigated, use depression as example

cb_fire <- crossbasis(data[,c(lag_start:lag_end)],argvar=list(fun="lin"),
                         arglag = list(fun="ns",knots = logknots(lagday,df_lag)))
    
model<-clogit(case~cb_fire+nonfire_mvavg_7+ns(temp_mvavg_7,df = 4)+ns(rhum_mvavg_7,df=4)+holiday+strata(id),method = "exact",data = data)
    
mid_lag = crossreduce(cb_fire,model,cen= 0,by = 0.1,type = 'var',value = 1)
    
## Store results
result_lag_RR = data.frame(outcome = outcome,lagday = 0:lagday,country = 'overall',RR = unname(mid_lag$RRfit),RRlow = unname(mid_lag$RRlow),RRhigh = unname(mid_lag$RRhigh))
    
    

# Exposure-response models
## The final lag used in the main analysis is 6 
knots_air = quantile(data$fire_mvavg_6,probs = c(0.25,0.75)) ## the knots used in the exposure-response dimention of the crossbasis function
    
cb_fire <- onebasis(data$fire_mvavg_6, "ns", knots = knots_air)

model<-clogit(case~cb_fire+nonfire_mvavg_6+ns(temp_mvavg_7,df=4)+ns(rhum_mvavg_7,df=4)+holiday+strata(id),method = "exact",data = data)
    
mid_ER = crosspred(cb_fire,model,cen= min(dat_mid["fire_mvavg_6"],na.rm = T),by = 0.1)

## Store results
result_overall_ER = data.frame(outcome = outcome,x = as.numeric(unname(mid_ER$predvar)),RR = unname(mid_ER$allRRfit),RRlow = unname(mid_ER$allRRlow),RRhigh = unname(mid_ER$allRRhigh))
 


## Calculate Relative risk (RR)
lag_start <- 5 ## the starting column of wildfire-specific PM2.5
lag_end <- 11 ## the ending column of wildfire-specific PM2.5
lagday <- 6
df_lag <- 3

### for fire
cb_fire <- crossbasis(data[,c(lag_start:lag_end)],argvar=list(fun="lin"),
                         arglag = list(fun="ns",knots = logknots(lagday,df_lag)))
    
model<-clogit(case~cb_fire+nonfire_mvavg_6+ns(temp_mvavg_7,df=4)+ns(rhum_mvavg_7,df=4)+holiday+strata(id),method = "exact",data = data)
    
mid_RR = crosspred(cb_fire,model,cen= 0,at = increase_mid)
    
#### Store results
result_RR = data.frame(pollutant = "wildfire PM2.5",outcome = outcome,x = increase_mid,RR = unname(mid_RR$allRRfit),RRlow = unname(mid_RR$allRRlow),RRhigh = unname(mid_RR$allRRhigh))

### for non-fire
lag_start <- 36 ## the starting column of non-wildfire PM2.5
lag_end <- 42 ## the ending column of non-wildfire PM2.5

cb_nonfire <- crossbasis(dat_mid[,c(lag_start:lag_end)],argvar=list(fun="lin"),
                       arglag = list(fun="ns",knots = logknots(lagday,df_lag)))
  
model<-clogit(case~cb_nonfire+fire_mvavg_6+ns(temp_mvavg_7,df=4)+ns(rhum_mvavg_7,df=4)+holiday+strata(id),method = "exact",data = dat_mid)## method="breslow" ## method 'exact' means use correct calculation in the conditional likelihood
  
mid_RR = crosspred(cb_nonfire,model,cen= 0,at = increase_mid)
  
  
#### Store results
result_RR = data.frame(outcome = outcome,x = increase_mid,RR = unname(mid_RR$allRRfit),RRlow = unname(mid_RR$allRRlow),RRhigh = unname(mid_RR$allRRhigh))