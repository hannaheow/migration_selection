# data manipulation for second aim 


###################################################################################
# 1) premature mortality data 
#################################################################################
library(dplyr)

mort1 = read.delim(file = "data_raw/premature_norace1119.txt")

mort1 = mort1[!is.na(mort1$State.Code),]
mort1$state_fips = stringr::str_pad(mort1$State.Code, 2, pad = "0")
mort1$county_fips = substr(mort1$County.Code, nchar(mort1$County.Code)-3+1, nchar(mort1$County.Code))
mort1$fips = paste0(mort1$state_fips, mort1$county_fips)
mort1$agerate = as.numeric(mort1$Age.Adjusted.Rate)
mort = mort1 %>% select(Year, state_fips, county_fips, fips, agerate, Deaths, Population)

 
#no evidence of duplicates 
dups =mort %>% group_by(Year, fips) %>% filter(n()>1) 


#only run the following lines if want to change final premature mort file 
# save(mort, file = "data_processed/premature_norace_1119_processed.RData")





  ###################################################################################
### 2) migration data 
#################################################################################

library(data.table)
library(stringi)
library(haven)
library(dplyr)
cfips = haven::read_sas("data_raw/county_fips.sas7bdat")




infile_names <- list.files(path = "data_raw/IRSmig1120", pattern= 'countyinflow', full.names = F, recursive = F)
outfile_names <- list.files(path = "data_raw/IRSmig1120", pattern= 'countyoutflow', full.names = F, recursive = F)


years = c("1112", "1213", "1314", "1415", "1516", "1617", "1718", "1819", "1920")

inout = data.frame() 

for (i in 1:length(years)){
  in1 = read.csv(paste0("data_raw/IRSmig1120/", infile_names[i]))
  in1$y2_statefips = stringr::str_pad(in1$y2_statefips, 2, pad = "0")
  in1$y2_countyfips = stringr::str_pad(in1$y2_countyfips, 3, pad = "0")
  in1$y1_statefips = stringr::str_pad(in1$y1_statefips, 2, pad = "0")
  in1$y1_countyfips = stringr::str_pad(in1$y1_countyfips, 3, pad = "0")
  
  in1$destid = paste0(in1$y2_statefips, in1$y2_countyfips)
  in1$origid = paste0(in1$y1_statefips, in1$y1_countyfips)
  in1sub = in1[,c("n2", "destid", "origid")]
  in1sub$year = years[i]
  
  inout = rbind(in1sub, inout)
}


inout = inout[inout$destid <57000 & inout$origid <57000,] #removing all values that are for "total" / national migration..from documentation: 
# Foreign............................................................. 57
# Total Migration – US and Foreign........................... 96
# Total Migration – US............................................ 97
# Total Migration – Foreign...................................... 98
# Total Migration – Same State..................................97



inout$year = as.numeric(inout$year)

#only run the following line if want to change final inout file 
#save(inout, file = "data_processed/9yearsmigfixed.RData")

#####################################################################################
##### 3) calculate lags/changes from year to year ####################################
#################################################################################

# dest_mort dataset 
# First need to determine how much each destination changed from year 0 to year 1. This calculation only requires mortality datafiles.  
# 
# For each fipscode (destination) and each change in year I need:    
# -pop_d1
# -pop_d0
# -agerate_d1
# -agerate_d0
# -cyear (formatted like 1617 where 2016 = t0 and 2017 = t1)

library(ggplot2)
library(stringr)
#load("data_processed/premature_norace_1119_processed.RData") #the name of this dataset is mort if already loaded in environment 


dest_mort = mort %>% group_by(fips) %>% 
  mutate(pop_d0 = lag(Population, order_by = Year), 
         rate_d0 = lag(agerate, order_by = Year)) %>% 
  rename(pop_d1 = Population, 
         rate_d1 = agerate,
         year = Year,
         destid = fips,
         deaths_d1 = Deaths) %>% 
  select(year, destid,
         pop_d0, rate_d0,  
         pop_d1, rate_d1, deaths_d1)

dest_mort$cyear = ifelse(dest_mort$year == "1999", "NA99", 
                                       ifelse(dest_mort$year == "2000", "9900", 
                                              paste0(str_pad(as.numeric(substr(dest_mort$year,
                                                                               nchar(dest_mort$year)-1,
                                                                               nchar(dest_mort$year))) - 1, width = 2, side = "left", pad = "0"), 
                                                     str_pad(substr(dest_mort$year, nchar(dest_mort$year)-1,
                                                                    nchar(dest_mort$year)), width = 2, side = "left", pad = "0"))))





################################################################################
### 4) Create initial/ origin dataset 
#################################################################################

# initial_mort dataset
# In a separate dataset, I will collect  all "initial" county and year-specific mortality rates and populations.  
# 
# These initial populations and mortality rates will now be considered "origin" mortality rates and populations. These "initial" values will be from "year 0". This dataset will have the following columns: 
# -origid
# -year (t0)
# -totpop_o


initial_mort = mort %>% rename(origid = fips, 
                                    year = Year, 
                                    rate_o = agerate, 
                                    pop_o = Population) %>% 
  select(origid, year, pop_o, rate_o)

#####################################################################################
### 5) merge mortality data with migration flow 
##################################################################################

# final_mort dataset
# Then, using IRS inflow/outflow, I will merge **initial_mort** with **dest_mort** such that the following columns are present.   
# -destid
# -origid
# -pop_d1
# -pop_d0
# -cyear
# -pop_o0
# -agerate_o0
# -totin_d: the total individuals who migrated to destid in year 0 to year 1
# -out_o: the individuals who migrated to destid from origid in year 0 to year 1 
# -totout_d: the total individuals who migrated out of destid in year 0 to year 1


#first some manipulations of irs columns and creation of total inflow and outflow columns 

inout = inout %>% filter(destid != origid)
#remove migration counts when origin and destination are the same 


inout$n2 = ifelse(inout$n2 == -1, NA, inout$n2) #from IRS documentation: -1 indicates an unreliable/missing
inout = inout %>% group_by(destid, year) %>% 
  mutate(totin = sum(n2, na.rm = TRUE)) #add a column for total number of migrants to a destid 
outo = inout %>% group_by(origid, year) %>% 
  summarize(totout = sum(n2, na.rm = TRUE))#create a new dataset summed such that a column represents the total number of migrants out of a origid - this origid will then be merged with destid 
#dimensions are correct: 35176 observations ~ 3142 origid for 12 years

inouto = merge(inout, outo, by.x = c("destid", "year"), by.y = c("origid", "year"))

inouto[inouto$destid == "55025" & inouto$origid == "55079",] #mke to dane 
inouto[inouto$origid == "55025" & inouto$destid == "55079",] #dane to mke 
#in the datasets above, the totin is the total number of migrants to destid in year 
# and totout is the total number of migrants out of destid in year 
# n2 represents the flow from origid to destid (ie the proportion of the totin that is from that origid)
# i can't think of an efficient way to check this...but it appears correct at a glance 

#first add the dest_mort dataset to the inout dataset 
inoutd = merge(inouto, dest_mort, by.x = c("destid", "year"), by.y = c("destid", "cyear"))
inoutd = inoutd %>% rename(cyear = year, year = year.y)
inoutd$lagyear = inoutd$year - 1 #this is the "initial" / time 0 year 

#then add the intial_mort dataset to the inoutd dataset 

inoutdm = merge(inoutd, initial_mort, by.x = c("lagyear","origid"), by.y = c("year", "origid"))
final_mort = inoutdm %>% rename(out_o = n2, 
                                              totin_d = totin, 
                                              totout_d = totout, 
                                              pop_o0 = pop_o,
                                              rate_o0 = rate_o)



################################################################################
#### 6) add natality data for sensitivity analysis 
###################################################################################

nat_wonder = read.delim("data_raw/nat1621.txt")


nat = nat_wonder[!is.na(nat_wonder$State.of.Residence.Code),]
nat$state_fips = stringr::str_pad(nat$State.of.Residence.Code, 2, pad = "0")
nat$county_fips = substr(nat$County.of.Residence.Code, nchar(nat$County.of.Residence.Code)-3+1, nchar(nat$County.of.Residence.Code))
nat$fips = paste0(nat$state_fips, nat$county_fips)
nat$births = as.numeric(nat$Births)
nat = nat %>% select(Year, fips, births) #when merging with destid we don't really want state and county fips anymore 

 

#now merge with final_mort
natmort = merge(final_mort, nat, by.x = c("destid", "lagyear"), by.y = c("fips", "Year"), all.x = TRUE)
#this attaches births to each destination at the initial time period... I think this is what we want, but this may change as i do more thinking and as I begin including natality in my calcs 
natmort = natmort %>% rename(births_d0 = births)

#this dataset has premature mortality, migration flow, and natality estimates for 2011 through 2019 
#save(natmort, file ="data_processed/mort_mig_nat.Rdata")



###################################################################################
#### 7) calculate migration term 
#####################################################################################
##################################################################################
# migterm has no natality data and is therefore available for the majority of counties 
# migterm_nat includes natality data and is therefore missing for many counties 


migterm = natmort %>% group_by(destid, year) %>% 
  mutate(migterm = (sum(out_o *rate_o0) + rate_d0 * (as.numeric(pop_d0)-as.numeric(totout_d)))/(sum(out_o) + (as.numeric(pop_d0)-totout_d)),
         migterm_nat = (sum(out_o *rate_o0) + rate_d0 * (as.numeric(pop_d0) + births_d0 - as.numeric(deaths_d1)-as.numeric(totout_d)))/(sum(out_o) + (as.numeric(pop_d0)+births_d0-as.numeric(deaths_d1)-totout_d)),
         migdiff = migterm_nat - migterm) %>% 
  distinct(destid, migterm, year, rate_d0, rate_d1, .keep_all = TRUE)

#many missings introduced when migterm is calcd; almost triple 
#differences are small when comparing migterms with natality to migterms without natality!!! 

summary(migterm$migterm[!is.na(migterm$births_d0)])
summary(migterm$migterm_nat)


#save(migterm, file = "data_processed/migterm_premature.Rdata")

##############################################################################
#### 8) add urbanicity code
##############################################################################
urbcodes = haven::read_sas("data_raw/nchs_urbanicity_wlabels.sas7bdat")
migurb = merge(migterm, urbcodes, by.x = "destid", by.y = "fipscode")
nojoin = dplyr::anti_join(migterm,urbcodes, by = join_by("destid"== "fipscode"))

save(migurb, file = "data_processed/migterm_premature_urb.Rdata")
