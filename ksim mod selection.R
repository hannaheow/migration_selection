
library(tidycensus)
library(tidyverse)
library(splines)
library(splm)
library(plm)

load("data_processed/imputed_for_ksim.Rdata") 
#this dataset is called natmorturb_imp
# it has natality, death, population, premature mortality rates, migration flows, rural/urb codes but no migterms (yet)
# it is balanced and has no missing rates (except for initial years)

#this formula produced lowest bic for spatial and non spatial models as shown in "spatial mod selection.R"
fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4) + ns(migterm, df = 4) # + (1 + t | GEOID) 

# add geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)


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


#load spatial weights matrix created previously 
load("data_processed/queenw.Rdata")


ksi = seq(from =-400, to = 300, by = 100)
ksj = seq(from = 300, to = -300, by = -100)

bickij = data.frame()

for (i in 1:length(ksi)) {
  for (j in 1:length(ksj)) {
    
    tempdf = natmorturb_imp %>% group_by(destid, year) %>% 
      mutate(migterm = (sum(out_o *(rate_o0 + ksi[i]), na.rm = TRUE) + (rate_d0 + ksj[j]) * (as.numeric(pop_d0)-as.numeric(totout_d)))/(sum(out_o, na.rm = TRUE) + (as.numeric(pop_d0)- totout_d))) %>% 
      distinct(destid, migterm, year, rate_d0, rate_d1, .keep_all = TRUE)
    
    tempdf$ksi = ksi[i]
    tempdf$ksj = ksj[j]
    
    cpij = merge(cp, tempdf, by.x = "GEOID", by.y = "destid", all.y = TRUE)
    
    cpij = plm::pdata.frame(cpij, index = c("GEOID", "year"))
    cpij$ft = as.factor(cpij$year)
    
   
    # some ksi, ksj pairs throw fail to converge errors..... trycatch is trying to continue past those errors... 
    tryCatch({
      #this is the best nonspatial model as determined during hypothesis 3 
      nospat_fff4 = splm::spml(formula = fff4, 
                             data =cpij, model = "random", listw = queenw, lag = FALSE, spatial.error= "none") 
             
      #this is the best spatial model as determined during hypothesis 3 
      spat_fff4 = splm::spml(formula = fff4, data =cpij, model = "random", listw = queenw, lag = FALSE, spatial.error= "b", local = list(parallel = TRUE))
             
      bicnospat = BICsplm(nospat_fff4)
      bicspat = BICsplm(spat_fff4)
             
      outputtempdf = data.frame(ksi = unique(cpij$ksi), ksj = unique(cpij$ksj), bicnospat = bicnospat, bicspat = bicspat)},
             
             
      error = function(e){cat("ERROR :", conditionMessage(e))})
    
    
    bickij =  rbind(bickij, outputtempdf)
  }
}

save(bickij, file = "data_processed/ksim_bic.Rdata")

