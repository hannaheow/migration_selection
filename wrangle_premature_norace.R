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





#ADD IRS DATA FOR 2008 thru 2011 
#(still need 1990-2008....many files, very messy) 

#0809, 0910 are saved the same way as the latest datasets, but they have different col names 
years811 = c("0809", "0910", "1011")
for (i in 1:length(years811)) {
  in1 = read.csv(paste0("data_raw/IRSmig0811/countyinflow",years811[i], ".csv"))
  in1$y2_statefips = stringr::str_pad(in1$State_Code_Dest, 2, pad = "0")
  in1$y2_countyfips = stringr::str_pad(in1$County_Code_Dest, 3, pad = "0")
  in1$y1_statefips = stringr::str_pad(in1$State_Code_Origin, 2, pad = "0")
  in1$y1_countyfips = stringr::str_pad(in1$County_Code_Origin, 3, pad = "0")
  
  in1$destid = paste0(in1$y2_statefips, in1$y2_countyfips)
  in1$origid = paste0(in1$y1_statefips, in1$y1_countyfips)
  
  in1 = in1[!(in1$y1_statefips>57 | in1$y1_statefips == 00),]
  in1 = in1[!(in1$y2_statefips>57 | in1$y2_statefips == 00),]
 
  in1sub = in1[,c("destid", "Exmpt_Num", "origid")]
  
  in1sub$year = years811[i]
  in1sub = in1sub %>% rename(n2 = Exmpt_Num)
  
  in1sub = in1sub[!endsWith(in1sub$origid, "000"),]
  
  
  inout = rbind(inout,in1sub)
  
}


inout$year = as.numeric(inout$year)

#only run the following line if want to change final inout file 
#save(inout, file = "data_processed/12yearsmigfixed.RData")

#####################################################################################
##### 3) calculate changes from year to year ####################################
#################################################################################

# dest_mort_change_norace dataset 
# First need to determine how much each destination changed from year 0 to year 1. This calculation only requires mortality datafiles.  
# 
# For each fipscode (destination) and each change in year I need:    
# change_totpop_d = totpop_d1 - totpop_d0,   
# change_totrate_d = totrate_d1 - totrate_d0.   
#   
# The dest_mort_change dataset will have the following columns: 
# -change_totpop_d
# -change_totrate_d
# -totpop_d1
# -totpop_d0
# -totrate_d1
# -totrate_d0
# -cyear (formatted like 1617 where 2016 = t0 and 2017 = t1)

library(ggplot2)
library(stringr)
#load("data_processed/premature_norace_1119_processed.RData") #the name of this dataset is mort if already loaded in environment 


dest_mort_change_norace = mort %>% group_by(fips) %>% 
  mutate(pop_d0 = lag(Population, order_by = Year), 
         rate_d0 = lag(agerate, order_by = Year)) %>% 
  rename(pop_d1 = Population, 
         rate_d1 = agerate,
         year = Year,
         destid = fips) %>% 
  select(year, destid,
         pop_d0, rate_d0,  
         pop_d1, rate_d1)

dest_mort_change_norace$cyear = ifelse(dest_mort_change_norace$year == "1999", "NA99", 
                                       ifelse(dest_mort_change_norace$year == "2000", "9900", 
                                              paste0(str_pad(as.numeric(substr(dest_mort_change_norace$year,
                                                                               nchar(dest_mort_change_norace$year)-1,
                                                                               nchar(dest_mort_change_norace$year))) - 1, width = 2, side = "left", pad = "0"), 
                                                     str_pad(substr(dest_mort_change_norace$year, nchar(dest_mort_change_norace$year)-1,
                                                                    nchar(dest_mort_change_norace$year)), width = 2, side = "left", pad = "0"))))

dest_mort_change_norace$change_pop_d = as.numeric(dest_mort_change_norace$pop_d1) - as.numeric(dest_mort_change_norace$pop_d0)   
dest_mort_change_norace$change_rate_d = as.numeric(dest_mort_change_norace$rate_d1) - as.numeric(dest_mort_change_norace$rate_d0)   




################################################################################
### 4) Create initial/ origin dataset 
#################################################################################

# initial_mort_norace dataset
# In a separate dataset, I will collect  all "initial" county and year-specific mortality rates and populations.  
# 
# These initial populations and mortality rates will now be considered "origin" mortality rates and populations. These "initial" values will be from "year 0". This dataset will have the following columns: 
# -origid
# -year (t0)
# -totpop_o


initial_mort_norace = mort %>% rename(origid = fips, 
                                    year = Year, 
                                    rate_o = agerate, 
                                    pop_o = Population) %>% 
  select(origid, year, pop_o, rate_o)

#####################################################################################
### 5) merge mortality data with migration flow 
##################################################################################

# final_mort_norace dataset
# Then, using IRS inflow/outflow, I will merge **initial_mort_norace** with **dest_mort_change_norace** such that the following columns are present.   
# -destid
# -origid
# -change_totpop_d
# -totpop_d1
# -totpop_d0
# -cyear
# -totpop_o0
# -totrate_o0
# -totin_d: the total individuals who migrated to destid in year 0 to year 1
# -out_o: the individuals who migrated to destid from origid in year 0 to year 1 
# -totout_d: the total individuals who migrated out of destid in year 0 to year 1
# 


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

# inouto[inouto$destid == "55025" & inouto$origid == "55099",]
# inouto[inouto$origid == "55025" & inouto$destid == "55099",]

#first add the dest_mort_change dataset to the inout dataset 
inoutd_norace = merge(inouto, dest_mort_change_norace, by.x = c("destid", "year"), by.y = c("destid", "cyear"))
inoutd_norace = inoutd_norace %>% rename(cyear = year, 
                                         year = year.y)
inoutd_norace$lagyear = inoutd_norace$year - 1 #this is the "initial" / time 0 year 

#then add the intial_mort dataset to the inoutd dataset 

inoutdm_norace = merge(inoutd_norace, initial_mort_norace, by.x = c("lagyear","origid"), by.y = c("year", "origid"))
final_mort_norace = inoutdm_norace %>% rename(out_o = n2, 
                                              totin_d = totin, 
                                              totout_d = totout, 
                                              pop_o0 = pop_o,
                                              rate_o0 = rate_o)

###################################################################################
#### 6) calculate migration term 
#####################################################################################
##################################################################################
# NON RACE-SPECIFIC MIGTERM 


migterm = final_mort_norace %>% group_by(destid, year) %>% 
  mutate(migterm = (sum(out_o *rate_o0) + rate_d0 * (as.numeric(pop_d0)-as.numeric(totout_d)))/(sum(out_o) + (as.numeric(pop_d0)-totout_d))) %>% 
  distinct(destid, year, rate_d0, rate_d1, .keep_all = TRUE)



save(migterm, file = "data_processed/migterm_premature.Rdata")



################################################################################
#### 7) add natality data for sensitivity analysis 
###################################################################################

nat_wonder = read.delim("data_raw/nat1621.txt")


nat = nat_wonder[!is.na(nat_wonder$State.of.Residence.Code),]
nat$state_fips = stringr::str_pad(nat$State.of.Residence.Code, 2, pad = "0")
nat$county_fips = substr(nat$County.of.Residence.Code, nchar(nat$County.of.Residence.Code)-3+1, nchar(nat$County.of.Residence.Code))
nat$fips = paste0(nat$state_fips, nat$county_fips)
nat$births = as.numeric(nat$Births)
nat = nat %>% select(Year, fips, births) #when merging with destid we don't really want state and county fips anymore 

 

#now merge with migterm 
natmig = merge(migterm, nat, by.x = c("destid", "lagyear"), by.y = c("fips", "Year"), all.x = TRUE)
#this attaches births to each destination at the initial time period... I think this is what we want, but this may change as i do more thinking and as I begin including natality in my calcs 

#no evidence of duplicates 
dups =natmig %>% group_by(year, destid) %>% filter(n()>1) 

save(natmig, file ="data_processed/migterm_natality.Rdata")
