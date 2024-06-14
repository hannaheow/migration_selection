
library(tidycensus)
library(tidyverse)
library(splines)
library(splm)
library(plm)

# get data 
load("data_processed/migterm_imp.RData")

# add geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)

cpall = merge(cp, migterm_imp, by.x = "GEOID", by.y = "destid", all.y = TRUE)

# no longer taking the stratified approach..... 
#####################################################################
#split data into urb and rural 
# urb = migterm_imp %>% filter(rural == 0)
# rural = migterm_imp %>% filter(rural == 1)
# 
# cpurb = merge(cp, urb, by.x = "GEOID", by.y = "destid", all.y = TRUE)
# cprural = merge(cp, rural, by.x = "GEOID", by.y = "destid", all.y = TRUE)
# 
# cpurb = cpurb %>% select(rate_d1, rate_d0, migterm, GEOID, geometry, year)
# cprural = cprural %>% select(rate_d1, rate_d0, migterm, GEOID, geometry, year)


# need separate weights matrices for rural and urban 
# spurb = cpurb %>% select(GEOID, geometry) %>% distinct()
# queenurb = spdep::poly2nb(spurb, row.names = "GEOID", queen = T)
# queenurbw = spdep::nb2listw(queenurb, style = "W", zero.policy = TRUE)
# 
# sprural = cprural %>% select(GEOID, geometry) %>% distinct()
# queenrural = spdep::poly2nb(sprural, row.names = "GEOID", queen = T)
# queenruralw = spdep::nb2listw(queenrural, style = "W", zero.policy = TRUE)
# 
# cpurb = plm::pdata.frame(cpurb, index = c("GEOID", "year"))
# cpurb$ft = as.factor(cpurb$year)
# cprural= plm::pdata.frame(cprural, index = c("GEOID", "year"))
# cprural$ft = as.factor(cprural$year)
#########################################################################

#load spatial weights matrix which was created using the following: 
# commented out because queenw was saved and can now be loaded directly from data_processed 
# spall = cpall %>% select(GEOID, geometry) %>% distinct()
# queen = spdep::poly2nb(spall, row.names = "GEOID", queen = T)
# queenw = spdep::nb2listw(queen, style = "W", zero.policy = TRUE)

load("data_processed/queenw.Rdata")




#confirmed that the two calls below yield the same results (plm versus splm package)
# they do return slightly different summaries...... so better to be consistent with the splm package! 
# m1plm = plm::plm(formula = rate_d1 ~ ft + migterm , data = cpall, model = "random")
# 
# m1mignospat = splm::spml(formula = rate_d1 ~ ft + migterm , listw = queenw, data = cpall, model = "random", lag = FALSE, spatial.error = "none", local = list(parallel = TRUE))


# original models, no interaction for urbanicity 
fm = rate_d1 ~ ft + rate_d0 + migterm   
fnom = rate_d1 ~ ft + rate_d0 
fff2 = rate_d1 ~ ft + ns(rate_d0, df = 2) + ns(migterm, df = 2) 
fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4) + ns(migterm, df = 4) 
fff3 = rate_d1 ~ ft + ns(rate_d0, df = 3) + ns(migterm, df = 3) 
fff5 = rate_d1 ~ ft + ns(rate_d0, df = 5) + ns(migterm, df = 5)  
fff2_nomig = rate_d1 ~ ft + ns(rate_d0, df = 2) 
fff4_nomig = rate_d1 ~ ft + ns(rate_d0, df = 4) 
fff3_nomig = rate_d1 ~ ft + ns(rate_d0, df = 3) 
fff5_nomig = rate_d1 ~ ft + ns(rate_d0, df = 5) 


#models with as.factor(rural) as an interaction with lagged and an interaction with migration
fmr = rate_d1 ~ ft + rate_d0*as.factor(rural) + migterm*as.factor(rural) + as.factor(rural) +    
fnomr = rate_d1 ~ ft + rate_d0*as.factor(rural) + as.factor(rural)
fff2r = rate_d1 ~ ft + ns(rate_d0, df = 2)*as.factor(rural) + ns(migterm, df = 2)*as.factor(rural) + as.factor(rural)
fff4r = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural) + ns(migterm, df = 4)*as.factor(rural) + as.factor(rural)
fff3r = rate_d1 ~ ft + ns(rate_d0, df = 3)*as.factor(rural) + ns(migterm, df = 3)*as.factor(rural) + as.factor(rural)
fff5r = rate_d1 ~ ft + ns(rate_d0, df = 5)*as.factor(rural) + ns(migterm, df = 5)*as.factor(rural) + as.factor(rural)
fff2r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 2)*as.factor(rural) + as.factor(rural)
fff4r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural) + as.factor(rural)
fff3r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 3)*as.factor(rural) + as.factor(rural)
fff5r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 5)*as.factor(rural) + as.factor(rural)

# models with "stratified" interaction terms for rural 
fmrs = rate_d1 ~ ft + rate_d0[rural == 0] + rate_d0[rural == 1] + migterm[rural ==0] + migterm[rural == 1]   
fnomrs = rate_d1 ~ ft + rate_d0[rural ==0] + rate_d0[rural == 1] 
fff2rs = rate_d1 ~ ft + ns(rate_d0[rural == 0], df = 2) + ns(migterm[rural == 0], df = 2) + ns(rate_d0[rural == 1], df = 2) + ns(migterm[rural == 1], df = 2) 
fff4rs = rate_d1 ~ ft + ns(rate_d0[rural == 0], df = 4) + ns(migterm[rural == 0], df = 4) + ns(rate_d0[rural == 1], df = 4) + ns(migterm[rural == 1], df = 4) 
fff3rs = rate_d1 ~ ft + ns(rate_d0[rural == 0], df = 3) + ns(migterm[rural == 0], df = 3) + ns(rate_d0[rural == 1], df = 3) + ns(migterm[rural == 1], df = 3) 
fff5rs = rate_d1 ~ ft + ns(rate_d0[rural == 0], df = 5) + ns(migterm[rural == 0], df = 5) + ns(rate_d0[rural == 1], df = 5) + ns(migterm[rural == 1], df = 5) 
fff2rs_nomig = rate_d1 ~ ft + ns(rate_d0[rural ==0], df = 2) + ns(rate_d0[rural ==1], df = 2) 
fff4rs_nomig = rate_d1 ~ ft + ns(rate_d0[rural ==0], df = 4) + ns(rate_d0[rural ==1], df = 4) 
fff3rs_nomig = rate_d1 ~ ft + ns(rate_d0[rural ==0], df = 3) + ns(rate_d0[rural ==1], df = 3) 
fff5rs_nomig = rate_d1 ~ ft + ns(rate_d0[rural ==0], df = 5) + ns(rate_d0[rural ==1], df = 5) 


# some singularity errors..... trycatch is trying to continue past those errors... 
tryCatch({

# with spatial stuff 
# original models, no urbanicity interaction 

spat_fm  = splm::spml(formula = fm, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fm , file = "temporary output/spat_fm.Rdata")

spat_fnom  = splm::spml(formula = fnom, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fnom , file = "temporary output/spat_fnom.Rdata")

spat_fff2  = splm::spml(formula = fff2, data =cpall, model = "random", listw=  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2 , file = "temporary output/spat_fff2.Rdata")

spat_fff3  = splm::spml(formula = fff3, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3 , file = "temporary output/spat_fff3.Rdata")


spat_fff4  = splm::spml(formula = fff4, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4 , file = "temporary output/spat_fff4.Rdata")


spat_fff5  = splm::spml(formula = fff5, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5 , file = "temporary output/spat_fff5.Rdata")


spat_fff2_nomig  = splm::spml(formula = fff2_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2_nomig , file = "temporary output/spat_fff2_nomig.Rdata")


spat_fff3_nomig  = splm::spml(formula = fff3_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3_nomig , file = "temporary output/spat_fff3_nomig.Rdata")


spat_fff4_nomig  = splm::spml(formula = fff4_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4_nomig , file = "temporary output/spat_fff4_nomig.Rdata")


spat_fff5_nomig  = splm::spml(formula = fff5_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5_nomig , file = "temporary output/spat_fff5_nomig.Rdata")





# with spatial stuff 
# models with as.factor(rural) as an interaction with lagged and an interaction with migration

spat_fmr  = splm::spml(formula = fmr, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fmr , file = "temporary output/spat_fmr.Rdata")

spat_fnomr  = splm::spml(formula = fnomr, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fnomr , file = "temporary output/spat_fnomr.Rdata")

spat_fff2r  = splm::spml(formula = fff2r, data =cpall, model = "random", listw=  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2r , file = "temporary output/spat_fff2r.Rdata")

spat_fff3r  = splm::spml(formula = fff3r, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3r , file = "temporary output/spat_fff3r.Rdata")


spat_fff4r  = splm::spml(formula = fff4r, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4r , file = "temporary output/spat_fff4r.Rdata")


spat_fff5r  = splm::spml(formula = fff5r, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5r , file = "temporary output/spat_fff5r.Rdata")


spat_fff2r_nomig  = splm::spml(formula = fff2r_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2r_nomig , file = "temporary output/spat_fff2r_nomig.Rdata")


spat_fff3r_nomig  = splm::spml(formula = fff3r_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3r_nomig , file = "temporary output/spat_fff3r_nomig.Rdata")


spat_fff4r_nomig  = splm::spml(formula = fff4r_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4r_nomig , file = "temporary output/spat_fff4r_nomig.Rdata")


spat_fff5r_nomig  = splm::spml(formula = fff5r_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5r_nomig , file = "temporary output/spat_fff5r_nomig.Rdata")


# i don't have output for the models below here which i think means that they didn't run properly 
# # with spatial stuff 
# # models with "stratified" interaction terms for rural 
# 
# spat_fmrs  = splm::spml(formula = fmrs, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fmrs , file = "temporary output/spat_fmrs.Rdata")
# 
# spat_fnomrs  = splm::spml(formula = fnomrs, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fnomrs , file = "temporary output/spat_fnomrs.Rdata")
# 
# spat_fff2rs  = splm::spml(formula = fff2rs, data =cpall, model = "random", listw=  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff2rs , file = "temporary output/spat_fff2rs.Rdata")
# 
# spat_fff3rs  = splm::spml(formula = fff3rs, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff3rs , file = "temporary output/spat_fff3rs.Rdata")
# 
# 
# spat_fff4rs  = splm::spml(formula = fff4rs, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff4rs , file = "temporary output/spat_fff4rs.Rdata")
# 
# 
# spat_fff5rs  = splm::spml(formula = fff5rs, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff5rs , file = "temporary output/spat_fff5rs.Rdata")
# 
# 
# spat_fff2rs_nomig  = splm::spml(formula = fff2rs_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff2rs_nomig , file = "temporary output/spat_fff2rs_nomig.Rdata")
# 
# 
# spat_fff3rs_nomig  = splm::spml(formula = fff3rs_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff3rs_nomig , file = "temporary output/spat_fff3rs_nomig.Rdata")
# 
# 
# spat_fff4rs_nomig  = splm::spml(formula = fff4rs_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff4rs_nomig , file = "temporary output/spat_fff4rs_nomig.Rdata")
# 
# 
# spat_fff5rs_nomig  = splm::spml(formula = fff5rs_nomig, data =cpall, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
# save(spat_fff5rs_nomig , file = "temporary output/spat_fff5rs_nomig.Rdata")

}, error = function(e){cat("ERROR :", conditionMessage(e))})




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






# with spatial stuff 
# original models, no urbanicity interaction 
bicspat = c(fm = BICsplm(spat_fm),
            fnom = BICsplm(spat_fnom),
            fff2 = BICsplm(spat_fff2), 
            fff2_nomig = BICsplm(spat_fff2_nomig),
            fff3 = BICsplm(spat_fff3),
            fff3_nomig = BICsplm(spat_fff3_nomig),
            fff4 = BICsplm(spat_fff4),
            fff4_nomig = BICsplm(spat_fff4_nomig),
            fff5 = BICsplm(spat_fff5),
            fff5_nomig = BICsplm(spat_fff5_nomig))
names(bicspat) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
spatchoice = names(bicspat[bicspat== min(bicspat)])

# models with as.factor(rural) as an interaction with lagged and an interaction with migration
bicspatr = c(fm = BICsplm(spat_fmr),
                  fnom = BICsplm(spat_fnomr),
                  fff2 = BICsplm(spat_fff2r), 
                  fff2_nomig = BICsplm(spat_fff2r_nomig),
                  fff3 = BICsplm(spat_fff3r),
                  fff3_nomig = BICsplm(spat_fff3r_nomig),
                  fff4 = BICsplm(spat_fff4r),
                  fff4_nomig = BICsplm(spat_fff4r_nomig),
                  fff5 = BICsplm(spat_fff5r),
                  fff5_nomig = BICsplm(spat_fff5r_nomig))
names(bicspatr) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
spatchoicer = names(bicspatr[bicspatr== min(bicspatr)])

# models with "stratified" interaction terms for rural 
# bicspatrs = c(fm = BICsplm(spat_fmrs),
#              fnom = BICsplm(spat_fnomrs),
#              fff2 = BICsplm(spat_fff2rs), 
#              fff2_nomig = BICsplm(spat_fff2rs_nomig),
#              fff3 = BICsplm(spat_fff3rs),
#              fff3_nomig = BICsplm(spat_fff3rs_nomig),
#              fff4 = BICsplm(spat_fff4rs),
#              fff4_nomig = BICsplm(spat_fff4rs_nomig),
#              fff5 = BICsplm(spat_fff5rs),
#              fff5_nomig = BICsplm(spat_fff5rs_nomig))
# names(bicspatrs) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
# spatchoicers = names(bicspatrs[bicspatrs== min(bicspatrs)])


save(bicspat, file = "temporary output/bicspat.Rdata")
save(bicspatr, file = "temporary output/bicspatr.Rdata")
#save(bicspatrs, file = "temporary output/bicspatrs.Rdata")

# i believe that this warning message: In sqrt(diag(object$vcov.errcomp)) : NaNs produced
# is the result of one year of migterms missing a rate_d0. based on this stackoverflow, it is not a current concern: https://stackoverflow.com/questions/67338560/zeroinfl-model-warning-message-in-sqrtdiagobjectvcov-nans-produced