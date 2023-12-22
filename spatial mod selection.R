
library(tidycensus)
library(tidyverse)
library(splines)
library(splm)
library(plm)

# get data 
load("data_processed/migterm_imp.RData")
load("data_processed/queenw.Rdata")

# add geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)

cpmig = merge(cp, migterm_imp, by.x = "GEOID", by.y = "destid", all.y = TRUE)


# cpmig = cpmig %>% 
#  arrange(year) %>% 
#  group_by(GEOID) %>% 
#  mutate(t = row_number())
# 
# cpmig$ft = as.factor(cpmig$t)

# these are the steps for wrangling all contiguous US states, not just a subset 

cpall = cpmig %>% select(rate_d1, rate_d0, migterm, GEOID, geometry, year)

# commented out because queenw was saved and can now be loaded directly from data_processed 
# spall = cpall %>% select(GEOID, geometry) %>% distinct()
# queen = spdep::poly2nb(spall, row.names = "GEOID", queen = T)
# queenw = spdep::nb2listw(queen, style = "W", zero.policy = TRUE)

cpall = plm::pdata.frame(cpall, index = c("GEOID", "year"))
cpall$ft = as.factor(cpall$year)


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


#without spatial stuff 
nospat_fm = splm::spml(formula = fm, data =cpall, model = "random", 
                       listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fnom = splm::spml(formula = fnom, data =cpall, model = "random", 
                         listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff2 = splm::spml(formula = fff2, data =cpall, model = "random", 
                         listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff3 = splm::spml(formula = fff3, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff4 = splm::spml(formula = fff4, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff5 = splm::spml(formula = fff5, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff2_nomig = splm::spml(formula = fff2_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff3_nomig = splm::spml(formula = fff3_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff4_nomig = splm::spml(formula = fff4_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error= "none")
nospat_fff5_nomig = splm::spml(formula = fff5_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error= "none")


# with spatial stuff 

spat_fm = splm::spml(formula = fm, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fm, file = "temporary output/spat_fm.Rdata")

spat_fnom = splm::spml(formula = fnom, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fnom, file = "temporary output/spat_fnom.Rdata")

spat_fff2 = splm::spml(formula = fff2, data =cpall, model = "random", listw= queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2, file = "temporary output/spat_fff2.Rdata")

spat_fff3 = splm::spml(formula = fff3, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3, file = "temporary output/spat_fff3.Rdata")


spat_fff4 = splm::spml(formula = fff4, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4, file = "temporary output/spat_fff4.Rdata")


spat_fff5 = splm::spml(formula = fff5, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5, file = "temporary output/spat_fff5.Rdata")


spat_fff2_nomig = splm::spml(formula = fff2_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff2_nomig, file = "temporary output/spat_fff2_nomig.Rdata")


spat_fff3_nomig = splm::spml(formula = fff3_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff3_nomig, file = "temporary output/spat_fff3_nomig.Rdata")


spat_fff4_nomig = splm::spml(formula = fff4_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff4_nomig, file = "temporary output/spat_fff4_nomig.Rdata")


spat_fff5_nomig = splm::spml(formula = fff5_nomig, data =cpall, model = "random", listw = queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
save(spat_fff5_nomig, file = "temporary output/spat_fff5_nomig.Rdata")





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


bicnospat = c(fm = BICsplm(nospat_fm),
              fnom = BICsplm(nospat_fnom),
              fff2 = BICsplm(nospat_fff2), 
              fff2_nomig = BICsplm(nospat_fff2_nomig),
              fff3 = BICsplm(nospat_fff3),
              fff3_nomig = BICsplm(nospat_fff3_nomig),
              fff4 = BICsplm(nospat_fff4),
              fff4_nomig = BICsplm(nospat_fff4_nomig),
              fff5 = BICsplm(nospat_fff5),
              fff5_nomig = BICsplm(nospat_fff5_nomig))
names(bicnospat) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
nospatchoice = names(bicnospat[bicnospat== min(bicnospat)])


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

save(bicspat, file = "temporary output/bicspat.Rdata")
save(bicnospat, file = "temporary output/bicnospat.Rdata")

# i believe that this warning message: In sqrt(diag(object$vcov.errcomp)) : NaNs produced
# is the result of one year of migterms missing a rate_d0. based on this stackoverflow, it is not a current concern: https://stackoverflow.com/questions/67338560/zeroinfl-model-warning-message-in-sqrtdiagobjectvcov-nans-produced