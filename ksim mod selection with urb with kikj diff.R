
library(tidycensus)
library(tidyverse)
library(splines)
library(splm)
library(plm)

load("data_processed/imputed_for_ksim.Rdata") 
#this dataset is called natmorturb_imp
# it has natality, death, population, premature mortality rates, migration flows, rural/urb codes but no migterms (yet)
# it is balanced and has no missing rates (except for initial years)

# create urban and rural datasets 
# ruralksim = natmorturb_imp %>% filter(rural == 1)
# urbanksim = natmorturb_imp %>% filter(rural == 0)
# this looks like there are more urban counties than rural...i double checked to make sure i assigned the rural code correctly...and it looks correct
# might need to go back and make sure i didn't accidentally miss some counties? perhaps there are few rural bc some were lost if they didn't have enought data? but imputation should have already corrected for this

#this formula produced lowest bic for spatial and non spatial models as shown in "spatial mod selection.R"
fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4) + ns(migterm, df = 4) # + (1 + t | GEOID) 

# add geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)


# cprural = merge(x = cp, y = ruralksim, by.x = "GEOID", by.y= "destid", all.y = TRUE)
# cpurb = merge(x = cp, y = urbanksim, by.x = "GEOID", by.y= "destid", all.y = TRUE)
# 
# cpurb = cpurb %>% filter(!duplicated(.))
# cprural = cprural %>% filter(!duplicated(.))


# need separate weights matrices for rural and urban 
load("data_processed/queenruralw.Rdata")
load("data_processed/queenurbw.Rdata")



# function for BIC for spatial models 
BICsplm = function(object, k=2){ 
  sp = summary(object)
  l = sp$logLik
  np = length(coef(sp))
  N = nrow(sp$model)
  bic = -2*l+log(N)*np
  names(bic) = "BIC"
  return(bic)
}




ksi = 0
ksj = seq(from = -50, to = 50, by = 10)


###############################################################################
#sticking em both in the same loop....seems maybe more efficient 
bickij_rural = data.frame()
bickij_urb = data.frame()

for (i in 1:length(ksi)) {
  for (j in 1:length(ksj)) {
    
    #rural wrangling 
    tempdf_rural = natmorturb_imp %>% filter(rural == 1) %>% 
      group_by(destid, year) %>% 
      mutate(migterm = (sum(out_o *(rate_o0 + ksi[i]), na.rm = TRUE) + (rate_d0 + ksj[j]) * (as.numeric(pop_d0)-as.numeric(totout_d)))/(sum(out_o, na.rm = TRUE) + (as.numeric(pop_d0)- totout_d))) %>% 
      distinct(destid, migterm, year, rate_d0, rate_d1, .keep_all = TRUE)
    
    tempdf_rural$ksi = ksi[i]
    tempdf_rural$ksj = ksj[j]
    
    cpij_rural = merge(cp, tempdf_rural, by.x = "GEOID", by.y = "destid", all.y = TRUE)
    
    cpij_rural = plm::pdata.frame(tempdf_rural, index = c("destid", "year"))
    cpij_rural$ft = as.factor(cpij_rural$year)
    
    
    
    # urban wrangling 
    tempdf_urb = natmorturb_imp %>% filter(rural == 0) %>% 
      group_by(destid, year) %>% 
      mutate(migterm = (sum(out_o *(rate_o0 + ksi[i]), na.rm = TRUE) + (rate_d0 + ksj[j]) * (as.numeric(pop_d0)-as.numeric(totout_d)))/(sum(out_o, na.rm = TRUE) + (as.numeric(pop_d0)- totout_d))) %>% 
      distinct(destid, migterm, year, rate_d0, rate_d1, .keep_all = TRUE)
    
    tempdf_urb$ksi = ksi[i]
    tempdf_urb$ksj = ksj[j]
    
    cpij_urb = merge(cp, tempdf_urb, by.x = "GEOID", by.y = "destid", all.y = TRUE)
    
    cpij_urb = plm::pdata.frame(tempdf_urb, index = c("destid", "year"))
    cpij_urb$ft = as.factor(cpij_urb$year)
    
   
    # some ksi, ksj pairs throw fail to converge errors..... trycatch is trying to continue past those errors... 
    tryCatch({
      #this is the best nonspatial model as determined during hypothesis 3 
      nospat_fff4_rural = splm::spml(formula = fff4, 
                             data =cpij_rural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none") 
             
      #this is the best spatial model as determined during hypothesis 3 
      spat_fff4_rural = splm::spml(formula = fff4, data =cpij_rural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "b", local = list(parallel = TRUE))
             
      bicnospat_rural = BICsplm(nospat_fff4_rural)
      bicspat_rural = BICsplm(spat_fff4_rural)
             
      outputtempdf_rural = data.frame(ksi = unique(cpij_rural$ksi), ksj = unique(cpij_rural$ksj), bicnospat = bicnospat_rural, bicspat = bicspat_rural)
             
      
      
      
      nospat_fff4_urb = splm::spml(formula = fff4, 
                                     data =cpij_urb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none") 
      
      #this is the best spatial model as determined during hypothesis 3 
      spat_fff4_urb = splm::spml(formula = fff4, data =cpij_urb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "b", local = list(parallel = TRUE))
      
      bicnospat_urb = BICsplm(nospat_fff4_urb)
      bicspat_urb = BICsplm(spat_fff4_urb)
      
      outputtempdf_urb = data.frame(ksi = unique(cpij_urb$ksi), ksj = unique(cpij_urb$ksj), bicnospat = bicnospat_urb, bicspat = bicspat_urb)
  
      bickij_rural =  rbind(bickij_rural, outputtempdf_rural)
      bickij_urb =  rbind(bickij_urb, outputtempdf_urb)},
             
      error = function(e){cat("ERROR :", conditionMessage(e))})
    
    
   
  }
}

save(bickij_rural, file = "data_processed/ksim_bic_rural_kikjdiff.Rdata")
save(bickij_urb, file = "data_processed/ksim_bic_urb_kikjdiff.Rdata")







spatchoice_kij_rural = bickij_rural[bickij_rural$bicspat == min(bickij_rural$bicspat),]
nospatchoice_kij_rural = bickij_rural[bickij_rural$bicnospat == min(bickij_rural$bicnospat),]

spatchoice_kij_urb= bickij_urb[bickij_urb$bicspat == min(bickij_urb$bicspat),]
nospatchoice_kij_urb = bickij_urb[bickij_urb$bicnospat == min(bickij_urb$bicnospat),]


bickij_rural$urbcat = "rural"
bickij_urb$urbcat = "urban"
bickij = rbind(bickij_rural, bickij_urb)

bickij = bickij %>% group_by(urbcat) %>% 
  mutate(zscore = scale(bicspat))
ggplot(bickij) + geom_line(aes(x = ksj, y = zscore, group = urbcat, color = urbcat))
