# data manipulation and calculations for 3mods 
# note that the file paths in this file have not been tested since project structure was created - look to file paths first for errors (if any)


###################################################################################
# 1) race specific mortality data 
#################################################################################
library(dplyr)

rawnames <- list.files(path = "data_raw/", pattern= 'mort', full.names = F, recursive = F)
rawnames = rawnames[!rawnames == "scmort.txt"]
rawnamesh = list.files(pattern = "mh", full.names = F, recursive = F)
# the mort files contain hisp and non hisp. need nonhisp rows only from these files
# the mh files contain hisp only. all rows are useful in these files.
# we need age-adjusted rates....so it's easier to get them directly from cdc wonder than to try to back calculate 



nhmort = data.frame()

for (i in 1:length(rawnames)){
  mort1 = read.delim(rawnames[i])
  #mort1$mortyear = mortyears[i]
  #mort1$year = myear[i]
  mort1 = mort1[!is.na(mort1$State.Code),]
  mort1$state_fips = stringr::str_pad(mort1$State.Code, 2, pad = "0")
  mort1$county_fips = substr(mort1$County.Code, nchar(mort1$County.Code)-3+1, nchar(mort1$County.Code))
  mort1$fips = paste0(mort1$state_fips, mort1$county_fips)
  mort1$Crude.Rate = as.numeric(mort1$Crude.Rate)
  #mort1 = mort1[mort1$Hispanic.Origin != "Hispanic or Latino",] #get rows for nonhisp only 
 
  mort1 = mort1[mort1$Hispanic.Origin == "Not Hispanic or Latino",] #changed to specify nonhispanic or latino to eliminate potential for nonspecified rows to be included 
  mort1 = mort1[,c("Year", "Race", "state_fips", "county_fips", "fips", "Age.Adjusted.Rate", "Population", "Deaths", "X..of.Total.Deaths")]
  nhmort = rbind(nhmort, mort1)
  
}

#nh2 = nhmort %>% distinct(Year, fips, .keep_all = TRUE)



hmort = data.frame()
for (i in 1:length(rawnamesh)){
  mort1 = read.delim(rawnamesh[i])
  #mort1$mortyear = mortyears[i]
  #mort1$year = myear[i]
  mort1 = mort1[!is.na(mort1$State.Code),]
  mort1$state_fips = stringr::str_pad(mort1$State.Code, 2, pad = "0")
  mort1$county_fips = substr(mort1$County.Code, nchar(mort1$County.Code)-3+1, nchar(mort1$County.Code))
  mort1$fips = paste0(mort1$state_fips, mort1$county_fips)
  mort1$Crude.Rate = as.numeric(mort1$Crude.Rate)
  mort1 = mort1[,c("Year", "state_fips", "county_fips", "fips", "Age.Adjusted.Rate", "Population", "Deaths", "X..of.Total.Deaths")]
  mort1$Race = "Hispanic" #for simplicity..... 
  hmort = rbind(hmort, mort1)
  
}

nhhmort = rbind(nhmort, hmort)

#for state and county total age adjusted mort rates....... 
sc = read.delim("data_raw/scmort.txt")
sc = sc[!is.na(sc$State.Code),]
sc$state_fips = stringr::str_pad(sc$State.Code, 2, pad = "0")
sc$county_fips = substr(sc$County.Code, nchar(sc$County.Code)-3+1, nchar(sc$County.Code))
sc$fips = paste0(sc$state_fips, sc$county_fips)
sc$totrate = as.numeric(sc$Age.Adjusted.Rate)
sc$totpop = as.numeric(sc$Population)
sc = sc[,c("Year", "state_fips", "county_fips", "fips", "totrate", "totpop")]

#dups were eliminated with specification of hispanic.origin == not hispanic or latino in line 30!!!!!! 
nodup = nhmort %>% select(Year, Race, fips) %>% distinct() 
noduph = hmort %>% select(Year, Race, fips) %>% distinct()
#dups previously occurred in nhmort file 


dups = nhmort %>% group_by(Year, Race, fips) %>% filter(n()>1)
dups = nhmort[duplicated(nhmort),]

mort = merge(nhhmort, sc, by = c("Year", "fips"), all.x = TRUE) 

md = mort %>% select(fips, Year, Race) %>% distinct()

library(dplyr)
mortn = mort %>% select(-c(Race, state_fips.y, county_fips.y)) %>% mutate_if(is.character, as.numeric)
mortn$Race = mort$Race

mortn$fips = stringr::str_pad(mortn$fips, width = 5, side = "left", pad = "0")
mortn = data.frame(mortn)

mortn$agerate = mortn$Age.Adjusted.Rate

dups =mortn %>% group_by(Year, Race, fips) %>% filter(n()>1) #no dups in final dataset either 



#only run the following lines if want to change final mortn file 
#save(mortn, file = "H:/amy/migration/3mods/22yearsmort.RData")
save(sc, file = "data_processed/22yearsmort_norace_temp.RData")





  ###################################################################################
### 2) migration data 
#################################################################################

library(data.table)
library(stringi)
library(usmap)
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



