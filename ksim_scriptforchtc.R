
#at top of script: 
args <- commandArgs(trailingOnly = TRUE)
i <- args[1]
j <- args[2]
k <- args[3]
l <- args[4]

#this means i can remove the forloop 


# library(tidycensus)
# library(tidyverse)
# library(splines)
# library(splm)
# library(plm)

load("data_processed/imputed_for_ksim.Rdata") 
#this dataset is called natmorturb_imp
# it has natality, death, population, premature mortality rates, migration flows, rural/urb codes but no migterms (yet)
# it is balanced and has no missing rates (except for initial years)

urbcodes = haven::read_sas("data_raw/nchs_urbanicity_wlabels.sas7bdat") %>% 
  select(fipscode, rural) %>% 
  rename(rural_orig = rural)
#add rural/urb for origin 
nm = natmorturb_imp %>% rename(rural_dest = rural) %>% 
  left_join(urbcodes, by = join_by(origid == fipscode)) %>% 
  mutate(migtype = ifelse(!is.na(rural_dest)& !is.na(rural_orig), paste0(rural_dest, rural_orig), NA)) 



# this formula produced lowest bic for spatial and non spatial models as shown in "spatial mod selection.R" 
# note: changed "rural" to "rural_dest" to match new var names 
# kind of unsure about this strat versus nonstrat approach 
fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural_dest) + ns(migterm, df = 4)*as.factor(rural_dest) + as.factor(rural_dest)


# get geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, output = "wide", variables = c(tpop = "B01003_001"), survey = "acs5", geometry = TRUE)





#load spatial weights matrix created previously 
load("data_processed/queenw.Rdata")



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


###############################################################################

bickij_int = data.frame()


#this previously was the loop - now goes into code 

nmk = nm %>% mutate(newk = case_when(migtype == "00" ~ kuu[l],
                                     migtype == "01" ~ kur[k],
                                     migtype == "10" ~ kru[j], 
                                     migtype == "11" ~ krr[i]))

# wrangling 
tempdf = nmk %>%
  group_by(destid, year) %>% 
  mutate(migterm = (sum(out_o *(rate_o0 + newk), na.rm = TRUE) + (rate_d0) * (as.numeric(pop_d0)-as.numeric(totout_d)))/(sum(out_o, na.rm = TRUE) + (as.numeric(pop_d0)- totout_d))) %>% 
  distinct(destid, migterm, year, rate_d0, rate_d1, .keep_all = TRUE)

tempdf$kuu = kuu[l]
tempdf$kur = kur[k]
tempdf$kru = kru[j]
tempdf$krr = krr[i]

cpij = merge(cp, tempdf, by.x = "GEOID", by.y = "destid", all.y = TRUE)

# commented out because queenw was saved and can now be loaded directly from data_processed 
# spall = cpij %>% select(GEOID, geometry) %>% distinct()
# queen = spdep::poly2nb(spall, row.names = "GEOID", queen = T)
# queenw = spdep::nb2listw(queen, style = "W", zero.policy = TRUE)

cpij = plm::pdata.frame(cpij, index = c("GEOID", "year"))
cpij = cpij %>% filter(!duplicated(.)) 
cpij$ft = as.factor(cpij$year)





# some ks throw fail to converge errors..... trycatch is trying to continue past those errors... 
tryCatch({
  #this is the best nonspatial model as determined during hypothesis 3 
  nospat_fff4 = splm::spml(formula = fff4, 
                           data =cpij, model = "random", listw = queenw, lag = FALSE, spatial.error= "none", na.rm = TRUE) 
  
  spat_fff4 = splm::spml(formula = fff4, 
                         data =cpij, model = "random", listw = queenw, lag = FALSE, spatial.error= "b", local = list(parallel = TRUE)) 
  
  
  bicspat= BICsplm(spat_fff4)
  bicnospat = BICsplm(nospat_fff4)
  
  outputtempdf = data.frame(kur = unique(cpij$kur), kru = unique(cpij$kru),
                            kuu = unique(cpij$kuu), krr = unique(cpij$krr),
                            bicspat = bicspat, 
                            bicnospat = bicnospat)
  
  save(outputtempdf, file = paste0("ksim_int_output/tempoutput", outputtempdf$kur, outputtempdf$kru, outputtempdf$kuu, outputtempdf$krr))
  
  bickij_int =  rbind(bickij_int, outputtempdf)
},

error = function(e){cat("ERROR :", conditionMessage(e))}) #this will just go to the .out file 



save(bickij_int, file = "data_processed/ksim_bic_interaction.Rdata")


#submit file needs to be set up to grab this output 

