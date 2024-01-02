
library(tidycensus)
library(tidyverse)
library(splines)
library(splm)
library(plm)

# get data 
load("data_processed/migterm_imp.RData")

# add geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)


#split data into urb and rural 
urb = migterm_imp %>% filter(rural == 0)
rural = migterm_imp %>% filter(rural == 1)

cpurb = merge(cp, urb, by.x = "GEOID", by.y = "destid", all.y = TRUE)
cprural = merge(cp, rural, by.x = "GEOID", by.y = "destid", all.y = TRUE)

cpurb = cpurb %>% select(rate_d1, rate_d0, migterm, GEOID, geometry, year)
cprural = cprural %>% select(rate_d1, rate_d0, migterm, GEOID, geometry, year)


# need separate weights matrices for rural and urban 
spurb = cpurb %>% select(GEOID, geometry) %>% distinct()
queenurb = spdep::poly2nb(spurb, row.names = "GEOID", queen = T)
queenurbw = spdep::nb2listw(queenurb, style = "W", zero.policy = TRUE)

sprural = cprural %>% select(GEOID, geometry) %>% distinct()
queenrural = spdep::poly2nb(sprural, row.names = "GEOID", queen = T)
queenruralw = spdep::nb2listw(queenrural, style = "W", zero.policy = TRUE)

cpurb = plm::pdata.frame(cpurb, index = c("GEOID", "year"))
cpurb$ft = as.factor(cpurb$year)
cprural= plm::pdata.frame(cprural, index = c("GEOID", "year"))
cprural$ft = as.factor(cprural$year)

#confirmed that the two calls below yield the same results (plm versus splm package)
# they do return slightly different summaries...... so better to be consistent with the splm package! 
# m1plm = plm::plm(formula = rate_d1 ~ ft + migterm , data = cpall, model = "random")
# 
# m1mignospat = splm::spml(formula = rate_d1 ~ ft + migterm , listw = queenw, data = cpall, model = "random", lag = FALSE, spatial.error = "none", local = list(parallel = TRUE))



fm = rate_d1 ~ ft + rate_d0 + migterm
fnom = rate_d1 ~ ft + rate_d0 
fff2 = rate_d1 ~ ft + ns(rate_d0, df = 2) + ns(migterm, df = 2) # + (1 + t | GEOID) 
fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4) + ns(migterm, df = 4) # + (1 + t | GEOID) 
fff3 = rate_d1 ~ ft + ns(rate_d0, df = 3) + ns(migterm, df = 3) # + (1 + t | GEOID) 
fff5 = rate_d1 ~ ft + ns(rate_d0, df = 5) + ns(migterm, df = 5) # + (1 + t | GEOID) 
fff2_nomig = rate_d1 ~ ft + ns(rate_d0, df = 2) # + (1 + t | GEOID) 
fff4_nomig = rate_d1 ~ ft + ns(rate_d0, df = 4) # + (1 + t | GEOID) 
fff3_nomig = rate_d1 ~ ft + ns(rate_d0, df = 3) # + (1 + t | GEOID) 
fff5_nomig = rate_d1 ~ ft + ns(rate_d0, df = 5) # + (1 + t | GEOID) 

# URB 
#without spatial stuff 
nospat_fm_urb = splm::spml(formula = fm, data =cpurb, model = "random", 
                       listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fnom_urb = splm::spml(formula = fnom, data =cpurb, model = "random", 
                         listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff2_urb = splm::spml(formula = fff2, data =cpurb, model = "random", 
                         listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff3_urb = splm::spml(formula = fff3, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff4_urb = splm::spml(formula = fff4, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff5_urb = splm::spml(formula = fff5, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff2_nomig_urb = splm::spml(formula = fff2_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff3_nomig_urb = splm::spml(formula = fff3_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff4_nomig_urb = splm::spml(formula = fff4_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none")
nospat_fff5_nomig_urb = splm::spml(formula = fff5_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error= "none")

#RURAL 
# without spatial stuff 
# URB 
#without spatial stuff 
nospat_fm_rural = splm::spml(formula = fm, data =cprural, model = "random", 
                       listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fnom_rural = splm::spml(formula = fnom, data =cprural, model = "random", 
                         listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fff2_rural = splm::spml(formula = fff2, data =cprural, model = "random", 
                         listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fff3_rural = splm::spml(formula = fff3, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none")
# unclear why this one throws singularity error but others do not...... 

nospat_fff4_rural = splm::spml(formula = fff4, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fff5_rural = splm::spml(formula = fff5, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fff2_nomig_rural = splm::spml(formula = fff2_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fff3_nomig_rural = splm::spml(formula = fff3_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fff4_nomig_rural = splm::spml(formula = fff4_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none")
nospat_fff5_nomig_rural = splm::spml(formula = fff5_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error= "none")



# URB 
# with spatial stuff 

spat_fm_urb = splm::spml(formula = fm, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fm_urb, file = "temporary output/spat_fm_urb.Rdata")

spat_fnom_urb = splm::spml(formula = fnom, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fnom_urb, file = "temporary output/spat_fnom_urb.Rdata")

spat_fff2_urb = splm::spml(formula = fff2, data =cpurb, model = "random", listw= queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2_urb, file = "temporary output/spat_fff2_urb.Rdata")

spat_fff3_urb = splm::spml(formula = fff3, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3_urb, file = "temporary output/spat_fff3_urb.Rdata")


spat_fff4_urb = splm::spml(formula = fff4, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4_urb, file = "temporary output/spat_fff4_urb.Rdata")


spat_fff5_urb = splm::spml(formula = fff5, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5_urb, file = "temporary output/spat_fff5_urb.Rdata")


spat_fff2_nomig_urb = splm::spml(formula = fff2_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2_nomig_urb, file = "temporary output/spat_fff2_nomig_urb.Rdata")


spat_fff3_nomig_urb = splm::spml(formula = fff3_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3_nomig_urb, file = "temporary output/spat_fff3_nomig_urb.Rdata")


spat_fff4_nomig_urb = splm::spml(formula = fff4_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4_nomig_urb, file = "temporary output/spat_fff4_nomig_urb.Rdata")


spat_fff5_nomig_urb = splm::spml(formula = fff5_nomig, data =cpurb, model = "random", listw = queenurbw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5_nomig_urb, file = "temporary output/spat_fff5_nomig_urb.Rdata")





# RURAL 
# with spatial stuff 

spat_fm_rural = splm::spml(formula = fm, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fm_rural, file = "temporary output/spat_fm_rural.Rdata")

spat_fnom_rural = splm::spml(formula = fnom, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fnom_rural, file = "temporary output/spat_fnom_rural.Rdata")

spat_fff2_rural = splm::spml(formula = fff2, data =cprural, model = "random", listw= queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2_rural, file = "temporary output/spat_fff2_rural.Rdata")

spat_fff3_rural = splm::spml(formula = fff3, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3_rural, file = "temporary output/spat_fff3_rural.Rdata")


spat_fff4_rural = splm::spml(formula = fff4, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4_rural, file = "temporary output/spat_fff4_rural.Rdata")


spat_fff5_rural = splm::spml(formula = fff5, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5_rural, file = "temporary output/spat_fff5_rural.Rdata")


spat_fff2_nomig_rural = splm::spml(formula = fff2_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2_nomig_rural, file = "temporary output/spat_fff2_nomig_rural.Rdata")


spat_fff3_nomig_rural = splm::spml(formula = fff3_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3_nomig_rural, file = "temporary output/spat_fff3_nomig_rural.Rdata")


spat_fff4_nomig_rural = splm::spml(formula = fff4_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4_nomig_rural, file = "temporary output/spat_fff4_nomig_rural.Rdata")


spat_fff5_nomig_rural = splm::spml(formula = fff5_nomig, data =cprural, model = "random", listw = queenruralw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5_nomig_rural, file = "temporary output/spat_fff5_nomig_rural.Rdata")








# AIC and BIC function for splm object #
#copied (and then edited) from this git repository https://github.com/rfsaldanha/ecoespacialunicamp/blob/master/OLD/AICsplm.R
# at the suggestion of this stackoverflow post: https://stackoverflow.com/questions/55838656/extract-aic-from-a-fixed-effect-spatial-panel-model-estimation-result

BICsplm = function(object, k=2){ 
  sp = summary(object)
  l = sp$logLik
  np = length(coef(sp))
  N = nrow(sp$model)
  bic = -2*l+log(N)*np
  names(bic) = "BIC"
  return(bic)
}


bicnospat_rural = c(fm = BICsplm(nospat_fm_rural),
              fnom = BICsplm(nospat_fnom_rural),
              fff2 = BICsplm(nospat_fff2_rural), 
              fff2_nomig = BICsplm(nospat_fff2_nomig_rural),
              #fff3 = BICsplm(nospat_fff3_rural),
              fff3_nomig = BICsplm(nospat_fff3_nomig_rural),
              fff4 = BICsplm(nospat_fff4_rural),
              fff4_nomig = BICsplm(nospat_fff4_nomig_rural),
              fff5 = BICsplm(nospat_fff5_rural),
              fff5_nomig = BICsplm(nospat_fff5_nomig_rural))
names(bicnospat_rural) = c("fm", "fnom", "fff2", "fff2_nomig", #"fff3", 
                           "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
nospatchoice_rural = names(bicnospat_rural[bicnospat_rural== min(bicnospat_rural)])


bicnospat_urb = c(fm = BICsplm(nospat_fm_urb),
                    fnom = BICsplm(nospat_fnom_urb),
                    fff2 = BICsplm(nospat_fff2_urb), 
                    fff2_nomig = BICsplm(nospat_fff2_nomig_urb),
                    fff3 = BICsplm(nospat_fff3_urb),
                    fff3_nomig = BICsplm(nospat_fff3_nomig_urb),
                    fff4 = BICsplm(nospat_fff4_urb),
                    fff4_nomig = BICsplm(nospat_fff4_nomig_urb),
                    fff5 = BICsplm(nospat_fff5_urb),
                    fff5_nomig = BICsplm(nospat_fff5_nomig_urb))
names(bicnospat_urb) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", 
                           "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
nospatchoice_urb = names(bicnospat_urb[bicnospat_urb== min(bicnospat_urb)])




bicspat_rural = c(fm = BICsplm(spat_fm_rural),
            fnom = BICsplm(spat_fnom_rural),
            fff2 = BICsplm(spat_fff2_rural), 
            fff2_nomig = BICsplm(spat_fff2_nomig_rural),
            fff3 = BICsplm(spat_fff3_rural),
            fff3_nomig = BICsplm(spat_fff3_nomig_rural),
            fff4 = BICsplm(spat_fff4_rural),
            fff4_nomig = BICsplm(spat_fff4_nomig_rural),
            fff5 = BICsplm(spat_fff5_rural),
            fff5_nomig = BICsplm(spat_fff5_nomig_rural))
names(bicspat_rural) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
spatchoice_rural = names(bicspat_rural[bicspat_rural== min(bicspat_rural)])

bicspat_urb = c(fm = BICsplm(spat_fm_urb),
                  fnom = BICsplm(spat_fnom_urb),
                  fff2 = BICsplm(spat_fff2_urb), 
                  fff2_nomig = BICsplm(spat_fff2_nomig_urb),
                  fff3 = BICsplm(spat_fff3_urb),
                  fff3_nomig = BICsplm(spat_fff3_nomig_urb),
                  fff4 = BICsplm(spat_fff4_urb),
                  fff4_nomig = BICsplm(spat_fff4_nomig_urb),
                  fff5 = BICsplm(spat_fff5_urb),
                  fff5_nomig = BICsplm(spat_fff5_nomig_urb))
names(bicspat_urb) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
spatchoice_urb = names(bicspat_urb[bicspat_urb== min(bicspat_urb)])



save(bicspat_rural, file = "temporary output/bicspat_rural.Rdata")
save(bicnospat_rural, file = "temporary output/bicnospat_rural.Rdata")
save(bicspat_urb, file = "temporary output/bicspat_urb.Rdata")
save(bicnospat_urb, file = "temporary output/bicnospat_urb.Rdata")

# i believe that this warning message: In sqrt(diag(object$vcov.errcomp)) : NaNs produced
# is the result of one year of migterms missing a rate_d0. based on this stackoverflow, it is not a current concern: https://stackoverflow.com/questions/67338560/zeroinfl-model-warning-message-in-sqrtdiagobjectvcov-nans-produced