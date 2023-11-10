#first need a subset of data to make sure this is all working properly 

load("data_processed/migterm_imp.Rdata") 
#this dataset is called migterm_imp 
# it has natality, death, population, premature mortality rates, migration flows, rural/urb codes and two different precalculated migterms (one including natality and one not including natality)
# it is balanced and has no missing rate or migterm values (except for initial years)


migmodm = migterm_imp %>% arrange(lagyear) %>% 
  group_by(destid) %>% 
  filter(n_distinct(rate_d1)>4) #817 rows removed during this step 


mn = data.frame(migmodm)

mnn = mn %>% 
  arrange(year) %>% 
  group_by(destid) %>% 
  mutate(t = row_number())

mnn$ft = as.factor(mnn$t)



# mwi = mnn %>% filter(substr(destid, 1,2) %in% c(55,27,17,19)) %>% #wi, mn, il, ia
#   filter(!is.na(rate_d1)) 

cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)

nyears = mnn %>% group_by(destid) %>% 
  count()

#there are two counties with 7 years of data only....
# is it better to remove the two counties or remove the 8th year....
# easier to remove the two counties because it turns out that they don't have the same 7 years of data 



cpmnn = merge(cp, mnn, by.x = "GEOID", by.y = "destid")



mwi_bal = cpmnn %>% group_by(GEOID) %>% 
  filter(year %in% c(2012:2017)) %>%  
  filter(sum(!is.na(rate_d0))==6 & sum(!is.na(migterm)) == 6 & sum(!is.na(rate_d1))==6 & sum(!is.na(ft)) == 6) %>% 
  filter(n() == 6) %>% 
  select(rate_d1, ft, rate_d0, migterm, geometry, t, GEOID)



spsub= mwi_bal %>% select(GEOID, geometry) %>% distinct()
queen = spdep::poly2nb(spsub, row.names = "GEOID", queen = T)
queenw = spdep::nb2listw(queen, style = "W", zero.policy = TRUE)

mwi_bal = plm::pdata.frame(mwi_bal, index = c("GEOID", "ft"))
mwi_bal$ft = factor(mwi_bal$ft, levels = c(1:6))


library(splm)
fm = rate_d1 ~ ft + rate_d0 + migterm

fff = rate_d1 ~ ft + ns(rate_d0, df = 4) + (1 + t | destid) + ns(migterm, df = 4)


wmod = splm::spml(formula = rate_d1 ~ ft + rate_d0 + migterm, data =mwi_bal, model = "random", 
                  listw = queenw, lag = TRUE, spatial.error= "none")

wmodr = splm::spreml(formula = fm, data = mwi_bal, model = "error", effects ="tpfe", 
                     w = queenw, lag = TRUE)





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

bics = c("withoutspatial" = BICsplm(wmod),"withspatial" = BICsplm(wmodr))
# the model w spatial random intercept is a better fit 

#save(bics) 


# dependencies = renv::dependencies(path = getwd()) %>% select(Package) %>% distinct()
