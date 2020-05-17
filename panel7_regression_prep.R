library(stringdist)
library(dplyr)
library(data.table)
library(tidyr)
library(foreign)
library(plm)
library(photobiology)
library(stringr)
## remove missing values
#panel <- na.omit(panel)


## reformatting the panel for better recognition
fips_code = as.data.table(fips_code)
fips_code = fips_code[ , County.Name := str_replace(County.Name, "SAINT", "ST."), ]
fips_code = fips_code[ , County.Name := str_replace(County.Name, "LA SALLE", "LASALLE"), ]
fips_code = fips_code[ , County.Name := str_replace(County.Name, "DE SOTO", "DESOTO"), ]
fips_code = fips_code[ , County.Name := str_replace(County.Name, "DE WITT", "DEWITT"), ]

panel = panel[, BUYER_GEO_ID := str_replace(BUYER_GEO_ID , "SAINT", "ST.") ,  ]
panel = panel[, BUYER_GEO_ID := str_replace(BUYER_GEO_ID , "LA SALLE", "LASALLE") ,  ]
panel = panel[, BUYER_GEO_ID := str_replace(BUYER_GEO_ID , "DE SOTO", "DESOTO") ,  ]
panel = panel[, BUYER_GEO_ID := str_replace(BUYER_GEO_ID , "DE WITT", "DEWITT") ,  ]



panel$OLD_BUYER_GEO_ID = panel$BUYER_GEO_ID
panel = panel[, BUYER_GEO_ID := strsplit(BUYER_GEO_ID , split = "_") , ]


## CONVERT them to FIPS
catch_buyer_fips = function(x){
  
  try( fips_code$fips[ fips_code$State == x[[2]] & ( grepl(x[[1]], fips_code$County.Name, ignore.case = TRUE) | stringsim(x[[1]], fips_code$County.Name) == max(stringsim(x[[1]], fips_code$County.Name))  ) ][[1]], silent = TRUE )
  
}

panel$BUYER_FIPS = sapply(panel$BUYER_GEO_ID, catch_buyer_fips)

setorderv(panel, c("BUYER_FIPS", "YEAR"))





###### Check duplicates and combine duplicates  #####
# table(index(panel), useNA = "ifany")

duplicates = data.frame("FIPS" = levels(factor(panel$BUYER_FIPS)) )
duplicates$year_count = sapply(duplicates$FIPS, function(x){length(panel$YEAR[panel$BUYER_FIPS == x])})
#duplicates$unique_year_count = sapply(duplicates$FIPS, function(x){length(unique(panel$YEAR[panel$BUYER_FIPS == x]))})
# find the FIPS with more than 9 years attached
dup = duplicates$FIPS[duplicates$year_count > 9 ] %>% as.vector(.)
dup


# correct duplicated rows manually
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_CA") , ]
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_CT") , ]
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_GA") , ]
panel$BUYER_FIPS[panel$OLD_BUYER_GEO_ID == "FORD_IL"] = "17053"
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_IN") , ]
panel$BUYER_FIPS[panel$OLD_BUYER_GEO_ID == "FORD_KS"] = "20057"
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_MA") , ]
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_NV") , ]
panel$BUYER_FIPS[panel$OLD_BUYER_GEO_ID == "SMITH_TX"  ] = "48423"
panel$BUYER_FIPS[panel$OLD_BUYER_GEO_ID == "HILL_TX"] = "48217"
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_PR") , ] 	
panel = panel[-which(panel$OLD_BUYER_GEO_ID == "_OH") , ] 


## balance the unbalanced panel
panel <- pdata.frame(panel, index = c("BUYER_FIPS", "YEAR"), row.names = FALSE)

panel = make.pbalanced(
  panel,
  balance.type = c("shared.individuals")
)

panel = as.data.table(panel)


# panel$YEAR = as.integer(panel$YEAR)
# 
# panel$YEAR = panel$YEAR + 2005


panel$BUYER_GEO_ID = NULL

panel = panel[, c( "BUYER_FIPS", "YEAR", "OPI_CHAIN_SELF_DISTR", "OPI_CHAIN_EXTERNAL_DISTR",
                   "OPI_INDEP_DISTR", "OPI_SELF_DISTR" , "OPI_EXTERNAL_DISTR"  )]


panel = as.data.table(panel)

fwrite(panel, file = "D:/Research with Jorge and Jonathan since Nov 2019/SLCG/panel7.csv")
panel = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/panel7.csv")


#population$FIPS = as.integer(population$FIPS)


# fix the unrecognized Geo IDs
#panel$BUYER_GEO_ID[is.na(panel$BUYER_FIPS)]

  
#panel$BUYER_GEO_ID = NULL

# remove NA
# panel[is.na(panel)] <- 0








######################################################################################
######## the following duplicate checking process has serious problems!!! ############
############################## it makes correlations all 1 ###########################
# for(i in dup){
#   
#   year_temp = panel[BUYER_FIPS == i , YEAR , ] %>% table(.)
#   year_dup = names(year_temp[year_temp>=2])
#   
#   for(j in year_dup){
#     row_indecies = which(panel$BUYER_FIPS == i & panel$YEAR == j)
#     panel_add_row = data.frame("BUYER_FIPS" = i , "YEAR" = j)
#     to_add = panel[BUYER_FIPS == i & YEAR == j, colSums(panel[,3:86])] %>% as.data.table(.)
#     to_add = to_add %>% transpose(.)
#     colnames(to_add) = c( "BUPRENORPHINE_CHAIN_DISTR","BUPRENORPHINE_CHAIN_SELF_DISTR" ,"BUPRENORPHINE_CHAIN_MANUF","BUPRENORPHINE_INDEP_DISTR",
#                           "BUPRENORPHINE_INDEP_MANUF","BUPRENORPHINE_RETURN" ,"CODEINE_CHAIN_DISTR","CODEINE_CHAIN_SELF_DISTR"  ,"CODEINE_CHAIN_MANUF","CODEINE_INDEP_DISTR",
#                           "CODEINE_INDEP_MANUF","CODEINE_RETURN" ,"DIHYDROCODEINE_CHAIN_DISTR","DIHYDROCODEINE_CHAIN_SELF_DISTR","DIHYDROCODEINE_CHAIN_MANUF","DIHYDROCODEINE_INDEP_DISTR",
#                           "DIHYDROCODEINE_INDEP_MANUF","DIHYDROCODEINE_RETURN","FENTANYL_CHAIN_DISTR","FENTANYL_CHAIN_SELF_DISTR" ,"FENTANYL_CHAIN_MANUF","FENTANYL_INDEP_DISTR" ,
#                           "FENTANYL_INDEP_MANUF","FENTANYL_RETURN", "HYDROCODONE_CHAIN_DISTR","HYDROCODONE_CHAIN_SELF_DISTR","HYDROCODONE_CHAIN_MANUF","HYDROCODONE_INDEP_DISTR",
#                           "HYDROCODONE_INDEP_MANUF","HYDROCODONE_RETURN","HYDROMORPHONE_CHAIN_DISTR","HYDROMORPHONE_CHAIN_SELF_DISTR","HYDROMORPHONE_CHAIN_MANUF","HYDROMORPHONE_INDEP_DISTR",
#                           "HYDROMORPHONE_INDEP_MANUF","HYDROMORPHONE_RETURN","LEVORPHANOL_CHAIN_DISTR","LEVORPHANOL_CHAIN_SELF_DISTR" ,"LEVORPHANOL_CHAIN_MANUF","LEVORPHANOL_INDEP_DISTR",
#                           "LEVORPHANOL_INDEP_MANUF","LEVORPHANOL_RETURN","MEPERIDINE_CHAIN_DISTR","MEPERIDINE_CHAIN_SELF_DISTR" ,"MEPERIDINE_CHAIN_MANUF","MEPERIDINE_INDEP_DISTR","MEPERIDINE_INDEP_MANUF",
#                           "MEPERIDINE_RETURN","METHADONE_CHAIN_DISTR","METHADONE_CHAIN_SELF_DISTR","METHADONE_CHAIN_MANUF","METHADONE_INDEP_DISTR","METHADONE_INDEP_MANUF","METHADONE_RETURN" ,
#                           "MORPHINE_CHAIN_DISTR","MORPHINE_CHAIN_SELF_DISTR" ,"MORPHINE_CHAIN_MANUF","MORPHINE_INDEP_DISTR", "MORPHINE_INDEP_MANUF","MORPHINE_RETURN","OPIUM_POWDERED_CHAIN_DISTR",
#                           "OPIUM_POWDERED_CHAIN_SELF_DISTR", "OPIUM_POWDERED_CHAIN_MANUF","OPIUM_POWDERED_INDEP_DISTR" ,"OPIUM_POWDERED_INDEP_MANUF","OPIUM_POWDERED_RETURN","OXYCODONE_CHAIN_DISTR",
#                           "OXYCODONE_CHAIN_SELF_DISTR" ,"OXYCODONE_CHAIN_MANUF","OXYCODONE_INDEP_DISTR","OXYCODONE_INDEP_MANUF","OXYCODONE_RETURN","OXYMORPHONE_CHAIN_DISTR","OXYMORPHONE_CHAIN_SELF_DISTR" ,
#                           "OXYMORPHONE_CHAIN_MANUF","OXYMORPHONE_INDEP_DISTR","OXYMORPHONE_INDEP_MANUF","OXYMORPHONE_RETURN","TAPENTADOL_CHAIN_DISTR","TAPENTADOL_CHAIN_SELF_DISTR" ,
#                           "TAPENTADOL_CHAIN_MANUF","TAPENTADOL_INDEP_DISTR","TAPENTADOL_INDEP_MANUF","TAPENTADOL_RETURN"  )
#     panel_add_row = cbind(panel_add_row, to_add)
#     panel = rbind(panel, panel_add_row)
#     panel = panel[-row_indecies, ]
#   }
#   
# }



panel$POP = mapply(function(i,t){population[population$FIPS == i , toString(t) ] }, panel$BUYER_FIPS, panel$YEAR  )

# # unlist the column in order to convert it to .dta file
# pop1 = unlist(panel$POP)
# panel = cbind(panel, pop1)
# panel$POP = NULL
# rm(pop1)
# names(panel)[names(panel) == 'pop1'] <- 'POP'

## add unemployment rate

# unemployment_rate = read.csv(file="D://Research with Jorge and Jonathan since Nov 2019//WaPo//unemployment rate//un_rate 2006 to 2014.csv")
# 
# colnames(unemployment_rate) = c("FIPS", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014" )

# unemployment_rate06 = read.csv(file="D://Research with Jorge and Jonathan since Nov 2019//WaPo//unemployment rate//un_rate 2006.csv")
# 
# colnames(unemployment_rate06) = c("FIPS", "2006" )

panel$UN_RATE = mapply(function(i,t){ unemployment_rate[unemployment_rate$FIPS == i, toString(t)] } , panel$BUYER_FIPS, panel$YEAR   )
# replace the missing value by its time average
#panel$UN_RATE[is.na(panel$UN_RATE)] = sum(panel$UN_RATE[panel$BUYER_FIPS == panel$BUYER_FIPS[is.na(panel$UN_RATE)]])/


# unnest(panel, UN_RATE)

# unlist the column in order to convert it to .dta file
# UN_RATE1 = unlist(panel$UN_RATE)
# panel = cbind(panel, UN_RATE1)
# panel$UN_RATE = NULL
# rm(UN_RATE1)
# names(panel)[names(panel) == 'UN_RATE1'] <- 'UN_RATE'

# panel$MME_PCAP = panel$Total_MME / panel$POP
# panel$Total_MME_opi_PCAP = panel$Total_MME_opi / panel$POP
# panel$Total_MME_anti_PCAP = panel$Total_MME_anti / panel$POP
# 


## For some fips identities, there are year duplications

# n_years = data.table(FIPS = levels(factor(panel$BUYER_FIPS)) )
# 
# n_years$counts = sapply(n_years$FIPS, function(x){ length(panel$YEAR[panel$BUYER_FIPS == x]  )   } )
# 
# n_years$FIPS[n_years$counts> 9]
# 
# # These fips' have duplicated years:
# # [1] "2013"  "6001"  "8001"  "9001"  "12001" "13001" "17001"
# # [8] "17033" "18001" "19001" "20037" "24001" "25001" "26001"
# # [15] "28001" "32001" "34001" "36001" "39001" "40001" "48001"
# # [22] "48117" "48137" "48211"
# 
# # Remove these fips' from panel temporarily.
# 
# panel = panel[-which(panel$BUYER_FIPS ==  "2013")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "6001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "8001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "9001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "12001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "13001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "17001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "17033")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "18001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "19001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "20037")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "24001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "25001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "26001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "28001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "32001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "34001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "36001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "39001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "40001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "48001")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "48117")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "48137")  , ]
# panel = panel[-which(panel$BUYER_FIPS ==  "48211")  , ]
# 
# 
# 
# write.dta(panel, file = "D:/Research with Jorge and Jonathan since Nov 2019/SLCG/STATA regression/panel.dta")

## add Uninsured rate

insurance = read.csv("D:/Emprical Research/Social-Economic Data Source/SAHIE/SAHIE_19FEB20_08_46_58_63.csv", stringsAsFactors=FALSE)

insurance$Uninsured_rate = as.numeric(insurance$Uninsured_rate)

panel$UNINSUR_RATE = mapply(function(i,t){insurance$Uninsured_rate[insurance$ID == i & insurance$Year == t] }, panel$BUYER_FIPS, panel$YEAR)

# unnest(panel, UNINSUR_RATE)


###### add poverty rate and income ######

# # 2006
# poverty06 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est06all.csv", stringsAsFactors=FALSE)
# poverty06$County.FIPS = as.character(poverty06$County.FIPS)
# poverty06$ï..State.FIPS = sapply(poverty06$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
# poverty06$County.FIPS = sapply(poverty06$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
# poverty06$FIPS = paste0(poverty06$ï..State.FIPS, poverty06$County.FIPS) %>% as.integer(.)
# poverty06$YEAR = 2006
# panel$POV_RATE = 0
# panel$MED_INCOME = 0
# panel$POV_RATE[panel$YEAR == 2006] = mapply(function(i,t){tryCatch( poverty06$Poverty.Percent.All.Ages[poverty06$FIPS == i & poverty06$YEAR == t], silent = TRUE) }, panel$BUYER_FIPS[panel$YEAR == 2006], panel$YEAR[panel$YEAR == 2006] )
# panel$MED_INCOME[panel$YEAR == 2006] = mapply(function(i,t){tryCatch( poverty06$Median.Household.Income[poverty06$FIPS == i & poverty06$YEAR == t], silent = TRUE) }, panel$BUYER_FIPS[panel$YEAR == 2006], panel$YEAR[panel$YEAR == 2006] )
# 
# # 2007
# poverty07 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est07all.csv", stringsAsFactors=FALSE)
# poverty07$County.FIPS = as.character(poverty07$County.FIPS)
# poverty07$ï..State.FIPS = sapply(poverty07$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
# poverty07$County.FIPS = sapply(poverty07$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x} , silent = TRUE )})
# poverty07$FIPS = paste0(poverty07$ï..State.FIPS, poverty07$County.FIPS) %>% as.integer(.)
# poverty07$YEAR = 2007
# panel$POV_RATE[panel$YEAR == 2007] = mapply(function(i,t){tryCatch( poverty07$Poverty.Percent.All.Ages[poverty07$FIPS == i & poverty07$YEAR == t] , silent = TRUE)}, panel$BUYER_FIPS[panel$YEAR == 2007], panel$YEAR[panel$YEAR == 2007] )
# panel$MED_INCOME[panel$YEAR == 2007] = mapply(function(i,t){tryCatch( poverty07$Median.Household.Income[poverty07$FIPS == i & poverty07$YEAR == t], silent = TRUE) }, panel$BUYER_FIPS[panel$YEAR == 2007], panel$YEAR[panel$YEAR == 2007] )
# 
# # 2008
# poverty08 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est08all.csv", stringsAsFactors=FALSE)
# poverty08$County.FIPS = as.character(poverty08$County.FIPS)
# poverty08$ï..State.FIPS = sapply(poverty08$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
# poverty08$County.FIPS = sapply(poverty08$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x} , silent = TRUE )})
# poverty08$FIPS = paste0(poverty08$ï..State.FIPS, poverty08$County.FIPS) %>% as.integer(.)
# poverty08$YEAR = 2008
# panel$POV_RATE[panel$YEAR == 2008] = mapply(function(i,t){tryCatch( poverty08$Poverty.Percent.All.Ages[poverty08$FIPS == i & poverty08$YEAR == t] , silent = TRUE)}, panel$BUYER_FIPS[panel$YEAR == 2008], panel$YEAR[panel$YEAR == 2008] )
# panel$MED_INCOME[panel$YEAR == 2008] = mapply(function(i,t){tryCatch( poverty08$Median.Household.Income[poverty08$FIPS == i & poverty08$YEAR == t], silent = TRUE) }, panel$BUYER_FIPS[panel$YEAR == 2008], panel$YEAR[panel$YEAR == 2008] )
# 
# 
# 
# # 2009 !!!!!!!!!!!!! does not work
# poverty09 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est09all.csv", stringsAsFactors=FALSE)
# poverty09$County.FIPS = as.character(poverty09$County.FIPS)
# poverty09$ï..State.FIPS = sapply(poverty09$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
# poverty09$County.FIPS = sapply(poverty09$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x} , silent = TRUE )})
# poverty09$FIPS = paste0(poverty09$ï..State.FIPS, poverty09$County.FIPS) %>% as.integer(.)
# poverty09$YEAR = 2009
# panel$POV_RATE[panel$YEAR == 2009] = mapply(function(i,t){tryCatch( poverty09$Poverty.Percent.All.Ages [poverty09$FIPS == i & poverty09$YEAR == t] , silent = TRUE)}, panel$BUYER_FIPS[panel$YEAR == 2009], panel$YEAR[panel$YEAR == 2009] )
# panel$MED_INCOME[panel$YEAR == 2009] = mapply(function(i,t){tryCatch( poverty09$Median.Household.Income[poverty09$FIPS == i & poverty09$YEAR == t], silent = TRUE) }, panel$BUYER_FIPS[panel$YEAR == 2009], panel$YEAR[panel$YEAR == 2009] )


## another approach to add poverty rate and income

poverty06 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est06all.csv", stringsAsFactors=FALSE)
poverty07 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est07all.csv", stringsAsFactors=FALSE)
poverty08 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est08all.csv", stringsAsFactors=FALSE)
poverty09 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est09all.csv", stringsAsFactors=FALSE)
poverty10 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est10all.csv", stringsAsFactors=FALSE)
poverty11 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est11all.csv", stringsAsFactors=FALSE)
poverty12 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est12all.csv", stringsAsFactors=FALSE)
poverty13 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est13all.csv", stringsAsFactors=FALSE)
poverty14 = read.csv("D:/Emprical Research/Social-Economic Data Source/SAIPE/est14all.csv", stringsAsFactors=FALSE)



poverty06$County.FIPS = as.character(poverty06$County.FIPS)
poverty06$ï..State.FIPS = sapply(poverty06$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty06$County.FIPS = sapply(poverty06$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty06$FIPS = paste0(poverty06$ï..State.FIPS, poverty06$County.FIPS) %>% as.integer(.)
poverty06$ï..State.FIPS = NULL
poverty06$County.FIPS = NULL

poverty07$County.FIPS = as.character(poverty07$County.FIPS)
poverty07$ï..State.FIPS = sapply(poverty07$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty07$County.FIPS = sapply(poverty07$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty07$FIPS = paste0(poverty07$ï..State.FIPS, poverty07$County.FIPS) %>% as.integer(.)
poverty07$ï..State.FIPS = NULL
poverty07$County.FIPS = NULL


poverty08$County.FIPS = as.character(poverty08$County.FIPS)
poverty08$ï..State.FIPS = sapply(poverty08$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty08$County.FIPS = sapply(poverty08$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty08$FIPS = paste0(poverty08$ï..State.FIPS, poverty08$County.FIPS) %>% as.integer(.)
poverty08$ï..State.FIPS = NULL
poverty08$County.FIPS = NULL


poverty09$County.FIPS = as.character(poverty09$County.FIPS)
poverty09$ï..State.FIPS = sapply(poverty09$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty09$County.FIPS = sapply(poverty09$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty09$FIPS = paste0(poverty09$ï..State.FIPS, poverty09$County.FIPS) %>% as.integer(.)
poverty09$ï..State.FIPS = NULL
poverty09$County.FIPS = NULL

poverty10$County.FIPS = as.character(poverty10$County.FIPS)
poverty10$ï..State.FIPS = sapply(poverty10$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty10$County.FIPS = sapply(poverty10$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty10$FIPS = paste0(poverty10$ï..State.FIPS, poverty10$County.FIPS) %>% as.integer(.)
poverty10$ï..State.FIPS = NULL
poverty10$County.FIPS = NULL


poverty11$County.FIPS = as.character(poverty11$County.FIPS)
poverty11$ï..State.FIPS = sapply(poverty11$ï..State.FIPS, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty11$County.FIPS = sapply(poverty11$County.FIPS, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty11$FIPS = paste0(poverty11$ï..State.FIPS, poverty11$County.FIPS) %>% as.integer(.)
poverty11$ï..State.FIPS = NULL
poverty11$County.FIPS = NULL


poverty12$County.FIPS.Code = as.character(poverty12$County.FIPS.Code )
poverty12$ï..State.FIPS.Code = sapply(poverty12$ï..State.FIPS.Code , function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty12$County.FIPS.Code = sapply(poverty12$County.FIPS.Code , function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty12$FIPS = paste0(poverty12$ï..State.FIPS.Code , poverty12$County.FIPS.Code ) %>% as.integer(.)
poverty12$ï..State.FIPS.Code  = NULL
poverty12$County.FIPS.Code = NULL


poverty13$County.FIPS.Code = as.character(poverty13$County.FIPS.Code)
poverty13$ï..State.FIPS.Code = sapply(poverty13$ï..State.FIPS.Code, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty13$County.FIPS.Code = sapply(poverty13$County.FIPS.Code, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty13$FIPS = paste0(poverty13$ï..State.FIPS.Code, poverty13$County.FIPS.Code) %>% as.integer(.)
poverty13$ï..State.FIPS.Code = NULL
poverty13$County.FIPS.Code = NULL


poverty14$County.FIPS.Code = as.character(poverty14$County.FIPS.Code )
poverty14$ï..State.FIPS.Code = sapply(poverty14$ï..State.FIPS.Code, function(x){if(nchar(x)==1){paste0("0",x)} else{x} })
poverty14$County.FIPS.Code= sapply(poverty14$County.FIPS.Code, function(x){try( if(nchar(x)==1){ paste0("00",x) } else if(nchar(x)==2){ paste0("0",x) } else{x}, silent = TRUE ) })
poverty14$FIPS = paste0(poverty14$ï..State.FIPS.Code, poverty14$County.FIPS.Code) %>% as.integer(.)
poverty14$ï..State.FIPS.Code = NULL
poverty14$County.FIPS.Code = NULL


poverty = poverty06 %>% 
  full_join(., poverty07, by = "FIPS") %>% 
  full_join(., poverty08, by = "FIPS") %>% 
  full_join(., poverty09, by = "FIPS") %>% 
  full_join(., poverty10, by = "FIPS") %>% 
  full_join(., poverty11, by = "FIPS") %>% 
  full_join(., poverty12, by = "FIPS") %>% 
  full_join(., poverty13, by = "FIPS") %>% 
  full_join(., poverty14, by = "FIPS")


panel$POV_RATE = mapply(function(i,t){poverty[poverty$FIPS == i, paste0("Poverty", t) ] }, panel$BUYER_FIPS, panel$YEAR )
panel$MED_INCOME = mapply(function(i,t){poverty[poverty$FIPS == i ,  paste0("Income", t) ]}, panel$BUYER_FIPS, panel$YEAR)
panel$MED_INCOME = sub(",", "", panel$MED_INCOME)


panel$MED_INCOME = as.numeric(panel$MED_INCOME)
panel$POV_RATE = as.numeric(panel$POV_RATE)


##################################################
######### Import opioid prescribing rate #########
########  x prescriptions per 100 persons ########
##################################################

prescribing_2006 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2006.csv", stringsAsFactors=FALSE)
colnames(prescribing_2006) = c("FIPS" , "2006")

prescribing_2007 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2007.csv", stringsAsFactors=FALSE)
colnames(prescribing_2007) = c("FIPS" , "2007")

prescribing_2008 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2008.csv", stringsAsFactors=FALSE)
colnames(prescribing_2008) = c("FIPS" , "2008")

prescribing_2009 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2009.csv", stringsAsFactors=FALSE)
colnames(prescribing_2009) = c("FIPS" , "2009")

prescribing_2010 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2010.csv", stringsAsFactors=FALSE)
colnames(prescribing_2010) = c("FIPS" , "2010")

prescribing_2011 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2011.csv", stringsAsFactors=FALSE)
colnames(prescribing_2011) = c("FIPS" , "2011")

prescribing_2012 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2012.csv", stringsAsFactors=FALSE)
colnames(prescribing_2012) = c("FIPS" , "2012")

prescribing_2013 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2013.csv", stringsAsFactors=FALSE)
colnames(prescribing_2013) = c("FIPS" , "2013")

prescribing_2014 = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/U.S. County Prescribing Rates 2014.csv", stringsAsFactors=FALSE)
colnames(prescribing_2014) = c("FIPS" , "2014")


prescribing = prescribing_2006 %>%
  full_join(., prescribing_2007 , by = "FIPS") %>%
  full_join(., prescribing_2008 , by = "FIPS") %>%
  full_join(., prescribing_2009 , by = "FIPS") %>%
  full_join(., prescribing_2010 , by = "FIPS") %>%
  full_join(., prescribing_2011 , by = "FIPS") %>%
  full_join(., prescribing_2012 , by = "FIPS") %>%
  full_join(., prescribing_2013 , by = "FIPS") %>%
  full_join(., prescribing_2014 , by = "FIPS")


panel$Prescribing= mapply(function(i,t){prescribing[prescribing$FIPS == i, toString(t) ] }, panel$BUYER_FIPS, panel$YEAR )
panel$Prescribing = as.numeric(panel$Prescribing)


## Delete empty rows 
panel = panel[-(26686:27414), ]

## delete rows with na under "Prescribing". There are 2946 unique FIPS left
#panel <- na.omit(panel, cols=c("Prescribing"))





## manually de-duplicate
# panel[BUYER_FIPS == "6001" , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS == "6001" & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "6001" & panel$YEAR == 2006)[[2]], ]
# 
# 
# panel[BUYER_FIPS == "8001"  , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS ==  "8001" & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "8001" & panel$YEAR == 2006)[[2]], ]
# 
# 
# panel[BUYER_FIPS == "9001"  , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS ==  "9001" & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "9001" & panel$YEAR == 2006)[[2]], ]
# which(panel$BUYER_FIPS ==  "9001" & panel$YEAR == 2007)
# panel = panel[-which(panel$BUYER_FIPS == "9001" & panel$YEAR == 2007)[[2]], ]
# which(panel$BUYER_FIPS ==  "9001" & panel$YEAR == 2008)
# panel = panel[-which(panel$BUYER_FIPS == "9001" & panel$YEAR == 2008)[[2]], ]
# 
# 
# panel[BUYER_FIPS == "13001"  , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS ==  "13001" & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "13001" & panel$YEAR == 2006)[[2]], ]
# 
# 
# panel = panel[-which(panel$BUYER_FIPS == "13001" ), ]
# 
# 
# panel = panel[-which(panel$BUYER_FIPS == "17033" ), ]
# 
# 
# 
# panel[BUYER_FIPS == "18001"  , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS ==  "18001"  & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "18001" & panel$YEAR == 2006)[[2]], ]
# 
# 
# panel = panel[-which(panel$BUYER_FIPS == "20037" ), ]
# 
# 
# 
# 
# panel[BUYER_FIPS == "25001"  , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS ==  "25001"  & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "25001" & panel$YEAR == 2006)[[1]], ]
# 
# 
# 
# panel[BUYER_FIPS] %>% table(.)
# which(panel$BUYER_FIPS ==  "32001"  & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "32001" & panel$YEAR == 2006)[[1]], ]
# panel = panel[-which(panel$BUYER_FIPS == "32001" & panel$YEAR == 2007)[[1]], ]
# panel = panel[-which(panel$BUYER_FIPS == "32001" & panel$YEAR == 2008)[[1]], ]
# 
# 
# panel[BUYER_FIPS == "39001"  , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS ==  "39001"  & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "39001" & panel$YEAR == 2006)[[2]], ]
# 
# 
# 
# panel[BUYER_FIPS == "40001" , YEAR , ] %>% table(.)
# which(panel$BUYER_FIPS ==  "40001"  & panel$YEAR == 2006)
# panel = panel[-which(panel$BUYER_FIPS == "40001" & panel$YEAR == 2006)[[2]], ]
# 
# 
# 
# panel = panel[-which(panel$BUYER_FIPS == "48117" ), ]
# 
# 
# 
# panel = panel[-which(panel$BUYER_FIPS == "48211" ), ]






## import infant mortality.............. FAIL: the dataset only contains 413 counties, which is far less than what we need!
# infant_mort = fread("D:/Emprical Research/Social-Economic Data Source/Infant Mortality/Compressed Mortality, 2006-2014.csv")
# 
# infant_mort$rate = sapply(infant_mort$`Crude Rate`, function(x){regmatches(x, regexpr("\\d(.\\d)?", x) ) } )
# 
# infant_mort$rate = as.numeric(infant_mort$rate)
# 
# panel$INFANT_MORT = mapply(function(i,t){infant_mort$rate[infant_mort$`County Code` == i & infant_mort$Year == t ][[1]] }, panel$BUYER_FIPS, panel$YEAR )
# 
# panel$INFANT_MORT = NULL

############## REGRESSION ################



## replace missing value by 0 
# remove NA
# panel[is.na(panel)] <- 0

# check missing values
# library(ggplot2)
# library(naniar)
# gg_miss_var(panel)

####  fill in the missing values by average
# for(i in 1:length(panel$MED_INCOME)){
# 
#     if(is.na(panel$MED_INCOME[[i]])){
# 
#       panel$MED_INCOME[[i]] = sum(panel$MED_INCOME[panel$BUYER_FIPS == panel$BUYER_FIPS[[i]] ], na.rm = TRUE) / length(panel$YEAR[panel$BUYER_FIPS == panel$BUYER_FIPS[[i]]])
#     }
# }
# 
# #
# # 
# for(i in 1:length(panel$UN_RATE)){
#     if( is.na(panel$UN_RATE[[i]]) ){
#       
#       # panel$UN_RATE[[i]] = sum(panel$UN_RATE[panel$BUYER_FIPS == panel$BUYER_FIPS[[i]] ], na.rm = TRUE) / length(panel$YEAR[panel$BUYER_FIPS == panel$BUYER_FIPS[[i]]])
#       panel$UN_RATE[[i]] = 0
#     }
# }
# # 
# # 
# for(i in 1:length(panel$POV_RATE)){
#  
#     if(is.na(panel$POV_RATE[[i]])){
#       
#       panel$POV_RATE[[i]] = sum(panel$POV_RATE[panel$BUYER_FIPS == panel$BUYER_FIPS[[i]] ], na.rm = TRUE) / length(panel$YEAR[panel$BUYER_FIPS == panel$BUYER_FIPS[[i]]])
#       
#     }
#  
# } 


# unnest(panel, UNINSUR_RATE)
# UNINSUR_RATE1 = unlist(panel$UNINSUR_RATE)
# panel = cbind(panel, UNINSUR_RATE1)
# panel$UN_RATE = NULL
# rm(UNINSUR_RATE1)
# names(panel)[names(panel) == 'UNINSUR_RATE1'] <- 'UNINSUR_RATE'
# 
# 
# #panel$UN_RATE[is.na(panel$UN_RATE)] = sapply(panel$UN_RATE[is.na(panel$UN_RATE)], function(x){sum()})
# 
# 
# panel_reg <- pdata.frame(panel, index=c("BUYER_FIPS", "YEAR"))
# 
# library(foreign)


# setorderv(panel, c("BUYER_FIPS", "YEAR"))


## simplify colnames
# colnames(panel) = c("FIPS", "YEAR", "BUPR_CHAIN_DISTR","BUPR_CHAIN_SELF_DISTR",
#                     "BUPR_CHAIN_MANUF","BUPR_INDEP_DISTR" ,"BUPR_INDEP_MANUF",
#                     "BUPR_RETURN", "COD_CHAIN_DISTR","COD_CHAIN_SELF_DISTR" ,
#                     "COD_CHAIN_MANUF","COD_INDEP_DISTR" , "COD_INDEP_MANUF","COD_RETURN",
#                     "FENT_CHAIN_DISTR","FENT_CHAIN_SELF_DISTR",
#                     "FENT_CHAIN_MANUF","FENT_INDEP_DISTR","FENT_INDEP_MANUF","FENT_RETURN",
#                     "HYDROC_CHAIN_DISTR","HYDROC_CHAIN_SELF_DISTR","HYDROC_CHAIN_MANUF","HYDROC_INDEP_DISTR",
#                     "HYDROC_INDEP_MANUF","HYDROC_RETURN","HYDROM_CHAIN_DISTR","HYDROM_CHAIN_SELF_DISTR" ,
#                     "HYDROM_CHAIN_MANUF","HYDROM_INDEP_DISTR","HYDROM_INDEP_MANUF","HYDROM_RETURN",
#                     "MEP_CHAIN_DISTR","MEP_CHAIN_SELF_DISTR",
#                     "MEP_CHAIN_MANUF","MEP_INDEP_DISTR","MEP_INDEP_MANUF","MEP_RETURN" ,
#                     "METH_CHAIN_DISTR","METH_CHAIN_SELF_DISTR" ,"METH_CHAIN_MANUF","METH_INDEP_DISTR",
#                     "METH_INDEP_MANUF","METH_RETURN","MORPH_CHAIN_DISTR","MORPH_CHAIN_SELF_DISTR" ,
#                     "MORPH_CHAIN_MANUF","MORPH_INDEP_DISTR" , "MORPH_INDEP_MANUF","MORPH_RETURN",
#                     "OXYC_CHAIN_DISTR","OXYC_CHAIN_SELF_DISTR",
#                     "OXYC_CHAIN_MANUF","OXYC_INDEP_DISTR" ,"OXYC_INDEP_MANUF","OXYC_RETURN",
#                     "OXYM_CHAIN_DISTR","OXYM_CHAIN_SELF_DISTR" , "OXYM_CHAIN_MANUF","OXYM_INDEP_DISTR" ,
#                     "OXYM_INDEP_MANUF","OXYM_RETURN" ,"TAP_CHAIN_DISTR","TAP_CHAIN_SELF_DISTR" ,
#                     "TAP_CHAIN_MANUF","TAP_INDEP_DISTR","TAP_INDEP_MANUF","TAP_RETURN",
#                     "POP","UN_RATE", "UNINSUR_RATE","POV_RATE","MED_INCOME" )


#######################################################################
### 以下部分有错：CHAIN_DISTR 包含了 SELF_DISTR. 所以 double count ####
#######################################################################

## add a few more columns
# panel$BUPR_IN = panel$BUPR_CHAIN_DISTR + panel$BUPR_CHAIN_SELF_DISTR + panel$BUPR_CHAIN_MANUF + panel$BUPR_INDEP_DISTR + panel$BUPR_INDEP_MANUF
# panel$BUPR = panel$BUPR_IN - panel$BUPR_RETURN
# panel$BUPR_CHAIN_DISTR_percent = panel$BUPR_CHAIN_DISTR / panel$BUPR_IN
# panel$BUPR_CHAIN_SELF_DISTR_percent = panel$BUPR_CHAIN_SELF_DISTR / panel$BUPR_IN
# panel$BUPR_CHAIN_MANUF_percent = panel$BUPR_CHAIN_MANUF / panel$BUPR_IN
# panel$BUPR_INDEP_DISTR_percent =  panel$BUPR_INDEP_DISTR / panel$BUPR_IN
# panel$BUPR_INDEP_MANUF_percent =  panel$BUPR_INDEP_MANUF / panel$BUPR_IN
# # panel$BUPR_RETURN_percent = panel$BUPR_RETURN / panel$BUPR_IN
# # panel$BUPR_COMSUM_percent = panel$BUPR / panel$BUPR_IN
# 
# 
# panel$COD_IN = panel$COD_CHAIN_DISTR + panel$COD_CHAIN_SELF_DISTR + panel$COD_CHAIN_MANUF + panel$COD_INDEP_DISTR + panel$COD_INDEP_MANUF
# panel$COD = panel$COD_IN - panel$COD_RETURN
# panel$COD_CHAIN_DISTR_percent = panel$COD_CHAIN_DISTR / panel$COD_IN
# panel$COD_CHAIN_SELF_DISTR_percent = panel$COD_CHAIN_SELF_DISTR / panel$COD_IN
# panel$COD_CHAIN_MANUF_percent = panel$COD_CHAIN_MANUF / panel$COD_IN
# panel$COD_INDEP_DISTR_percent =  panel$COD_INDEP_DISTR / panel$COD_IN
# panel$COD_INDEP_MANUF_percent =  panel$COD_INDEP_MANUF / panel$COD_IN
# # panel$COD_RETURN_percent = panel$COD_RETURN / panel$COD_IN
# # panel$COD_COMSUM_percent = panel$COD / panel$COD_IN
# 
# 
# # dihydr has weird consumption and low incoming based on DIHYDR.png
# # panel$DIHYDR_IN = panel$DIHYDR_CHAIN_DISTR + panel$DIHYDR_CHAIN_SELF_DISTR + panel$DIHYDR_CHAIN_MANUF + panel$DIHYDR_INDEP_DISTR + panel$DIHYDR_INDEP_MANUF
# # panel$DIHYDR = panel$DIHYDR_IN - panel$DIHYDR_RETURN
# # panel$DIHYDR_CHAIN_DISTR_percent = panel$DIHYDR_CHAIN_DISTR / panel$DIHYDR_IN
# # panel$DIHYDR_CHAIN_SELF_DISTR_percent = panel$DIHYDR_CHAIN_SELF_DISTR / panel$DIHYDR_IN
# # panel$DIHYDR_CHAIN_MANUF_percent = panel$DIHYDR_CHAIN_MANUF / panel$DIHYDR_IN
# # panel$DIHYDR_INDEP_DISTR_percent =  panel$DIHYDR_INDEP_DISTR / panel$DIHYDR_IN
# # panel$DIHYDR_INDEP_MANUF_percent =  panel$DIHYDR_INDEP_MANUF / panel$DIHYDR_IN
# # panel$DIHYDR_RETURN_percent = panel$DIHYDR_RETURN / panel$DIHYDR_IN
# # panel$DIHYDR_COMSUM_percent = panel$DIHYDR / panel$DIHYDR_IN
# 
# 
# panel$FENT_IN = panel$FENT_CHAIN_DISTR + panel$FENT_CHAIN_SELF_DISTR + panel$FENT_CHAIN_MANUF + panel$FENT_INDEP_DISTR + panel$FENT_INDEP_MANUF
# panel$FENT = panel$FENT_IN - panel$FENT_RETURN
# panel$FENT_CHAIN_DISTR_percent = panel$FENT_CHAIN_DISTR / panel$FENT_IN
# panel$FENT_CHAIN_SELF_DISTR_percent = panel$FENT_CHAIN_SELF_DISTR / panel$FENT_IN
# panel$FENT_CHAIN_MANUF_percent = panel$FENT_CHAIN_MANUF / panel$FENT_IN
# panel$FENT_INDEP_DISTR_percent =  panel$FENT_INDEP_DISTR / panel$FENT_IN
# panel$FENT_INDEP_MANUF_percent =  panel$FENT_INDEP_MANUF / panel$FENT_IN
# # panel$FENT_RETURN_percent = panel$FENT_RETURN / panel$FENT_IN
# # panel$FENT_COMSUM_percent = panel$FENT / panel$FENT_IN
# 
# 
# panel$HYDROC_IN = panel$HYDROC_CHAIN_DISTR + panel$HYDROC_CHAIN_SELF_DISTR + panel$HYDROC_CHAIN_MANUF + panel$HYDROC_INDEP_DISTR + panel$HYDROC_INDEP_MANUF
# panel$HYDROC = panel$HYDROC_IN - panel$HYDROC_RETURN
# panel$HYDROC_CHAIN_DISTR_percent = panel$HYDROC_CHAIN_DISTR / panel$HYDROC_IN
# panel$HYDROC_CHAIN_SELF_DISTR_percent = panel$HYDROC_CHAIN_SELF_DISTR / panel$HYDROC_IN
# panel$HYDROC_CHAIN_MANUF_percent = panel$HYDROC_CHAIN_MANUF / panel$HYDROC_IN
# panel$HYDROC_INDEP_DISTR_percent =  panel$HYDROC_INDEP_DISTR / panel$HYDROC_IN
# panel$HYDROC_INDEP_MANUF_percent =  panel$HYDROC_INDEP_MANUF / panel$HYDROC_IN
# # panel$HYDROC_RETURN_percent = panel$HYDROC_RETURN / panel$HYDROC_IN
# # panel$HYDROC_COMSUM_percent = panel$HYDROC / panel$HYDROC_IN
# 
# 
# panel$HYDROM_IN = panel$HYDROM_CHAIN_DISTR + panel$HYDROM_CHAIN_SELF_DISTR + panel$HYDROM_CHAIN_MANUF + panel$HYDROM_INDEP_DISTR + panel$HYDROM_INDEP_MANUF
# panel$HYDROM = panel$HYDROM_IN - panel$HYDROM_RETURN
# panel$HYDROM_CHAIN_DISTR_percent = panel$HYDROM_CHAIN_DISTR / panel$HYDROM_IN
# panel$HYDROM_CHAIN_SELF_DISTR_percent = panel$HYDROM_CHAIN_SELF_DISTR / panel$HYDROM_IN
# panel$HYDROM_CHAIN_MANUF_percent = panel$HYDROM_CHAIN_MANUF / panel$HYDROM_IN
# panel$HYDROM_INDEP_DISTR_percent =  panel$HYDROM_INDEP_DISTR / panel$HYDROM_IN
# panel$HYDROM_INDEP_MANUF_percent =  panel$HYDROM_INDEP_MANUF / panel$HYDROM_IN
# # panel$HYDROM_RETURN_percent = panel$HYDROM_RETURN / panel$HYDROM_IN
# # panel$HYDROM_COMSUM_percent = panel$HYDROM / panel$HYDROM_IN
# 
# 
# # lev is also weird and low
# # panel$LEV_IN = panel$LEV_CHAIN_DISTR + panel$LEV_CHAIN_SELF_DISTR + panel$LEV_CHAIN_MANUF + panel$LEV_INDEP_DISTR + panel$LEV_INDEP_MANUF
# # panel$LEV = panel$LEV_IN - panel$LEV_RETURN
# # panel$LEV_CHAIN_DISTR_percent = panel$LEV_CHAIN_DISTR / panel$LEV_IN
# # panel$LEV_CHAIN_SELF_DISTR_percent = panel$LEV_CHAIN_SELF_DISTR / panel$LEV_IN
# # panel$LEV_CHAIN_MANUF_percent = panel$LEV_CHAIN_MANUF / panel$LEV_IN
# # panel$LEV_INDEP_DISTR_percent =  panel$LEV_INDEP_DISTR / panel$LEV_IN
# # panel$LEV_INDEP_MANUF_percent =  panel$LEV_INDEP_MANUF / panel$LEV_IN
# # panel$LEV_RETURN_percent = panel$LEV_RETURN / panel$LEV_IN
# # panel$LEV_COMSUM_percent = panel$LEV / panel$LEV_IN
# 
# 
# panel$MEP_IN = panel$MEP_CHAIN_DISTR + panel$MEP_CHAIN_SELF_DISTR + panel$MEP_CHAIN_MANUF + panel$MEP_INDEP_DISTR + panel$MEP_INDEP_MANUF
# panel$MEP = panel$MEP_IN - panel$MEP_RETURN
# panel$MEP_CHAIN_DISTR_percent = panel$MEP_CHAIN_DISTR / panel$MEP_IN
# panel$MEP_CHAIN_SELF_DISTR_percent = panel$MEP_CHAIN_SELF_DISTR / panel$MEP_IN
# panel$MEP_CHAIN_MANUF_percent = panel$MEP_CHAIN_MANUF / panel$MEP_IN
# panel$MEP_INDEP_DISTR_percent =  panel$MEP_INDEP_DISTR / panel$MEP_IN
# panel$MEP_INDEP_MANUF_percent =  panel$MEP_INDEP_MANUF / panel$MEP_IN
# # panel$MEP_RETURN_percent = panel$MEP_RETURN / panel$MEP_IN
# # panel$MEP_COMSUM_percent = panel$MEP / panel$MEP_IN
# 
# 
# panel$METH_IN = panel$METH_CHAIN_DISTR + panel$METH_CHAIN_SELF_DISTR + panel$METH_CHAIN_MANUF + panel$METH_INDEP_DISTR + panel$METH_INDEP_MANUF
# panel$METH = panel$METH_IN - panel$METH_RETURN
# panel$METH_CHAIN_DISTR_percent = panel$METH_CHAIN_DISTR / panel$METH_IN
# panel$METH_CHAIN_SELF_DISTR_percent = panel$METH_CHAIN_SELF_DISTR / panel$METH_IN
# panel$METH_CHAIN_MANUF_percent = panel$METH_CHAIN_MANUF / panel$METH_IN
# panel$METH_INDEP_DISTR_percent =  panel$METH_INDEP_DISTR / panel$METH_IN
# panel$METH_INDEP_MANUF_percent =  panel$METH_INDEP_MANUF / panel$METH_IN
# # panel$METH_RETURN_percent = panel$METH_RETURN / panel$METH_IN
# # panel$METH_COMSUM_percent = panel$METH / panel$METH_IN
# 
# 
# panel$MORPH_IN = panel$MORPH_CHAIN_DISTR + panel$MORPH_CHAIN_SELF_DISTR + panel$MORPH_CHAIN_MANUF + panel$MORPH_INDEP_DISTR + panel$MORPH_INDEP_MANUF
# panel$MORPH = panel$MORPH_IN - panel$MORPH_RETURN
# panel$MORPH_CHAIN_DISTR_percent = panel$MORPH_CHAIN_DISTR / panel$MORPH_IN
# panel$MORPH_CHAIN_SELF_DISTR_percent = panel$MORPH_CHAIN_SELF_DISTR / panel$MORPH_IN
# panel$MORPH_CHAIN_MANUF_percent = panel$MORPH_CHAIN_MANUF / panel$MORPH_IN
# panel$MORPH_INDEP_DISTR_percent =  panel$MORPH_INDEP_DISTR / panel$MORPH_IN
# panel$MORPH_INDEP_MANUF_percent =  panel$MORPH_INDEP_MANUF / panel$MORPH_IN
# # panel$MORPH_RETURN_percent = panel$MORPH_RETURN / panel$MORPH_IN
# # panel$MORPH_COMSUM_percent = panel$MORPH / panel$MORPH_IN
# 
# # opium_powdered is also weird
# # panel$OPIUM_IN = panel$OPIUM_CHAIN_DISTR + panel$OPIUM_CHAIN_SELF_DISTR + panel$OPIUM_CHAIN_MANUF + panel$OPIUM_INDEP_DISTR + panel$OPIUM_INDEP_MANUF
# # panel$OPIUM = panel$OPIUM_IN - panel$OPIUM_RETURN
# # panel$OPIUM_CHAIN_DISTR_percent = panel$OPIUM_CHAIN_DISTR / panel$OPIUM_IN
# # panel$OPIUM_CHAIN_SELF_DISTR_percent = panel$OPIUM_CHAIN_SELF_DISTR / panel$OPIUM_IN
# # panel$OPIUM_CHAIN_MANUF_percent = panel$OPIUM_CHAIN_MANUF / panel$OPIUM_IN
# # panel$OPIUM_INDEP_DISTR_percent =  panel$OPIUM_INDEP_DISTR / panel$OPIUM_IN
# # panel$OPIUM_INDEP_MANUF_percent =  panel$OPIUM_INDEP_MANUF / panel$OPIUM_IN
# # panel$OPIUM_RETURN_percent = panel$OPIUM_RETURN / panel$OPIUM_IN
# # panel$OPIUM_COMSUM_percent = panel$OPIUM / panel$OPIUM_IN
# 
# 
# panel$OXYC_IN = panel$OXYC_CHAIN_DISTR + panel$OXYC_CHAIN_SELF_DISTR + panel$OXYC_CHAIN_MANUF + panel$OXYC_INDEP_DISTR + panel$OXYC_INDEP_MANUF
# panel$OXYC = panel$OXYC_IN - panel$OXYC_RETURN
# panel$OXYC_CHAIN_DISTR_percent = panel$OXYC_CHAIN_DISTR / panel$OXYC_IN
# panel$OXYC_CHAIN_SELF_DISTR_percent = panel$OXYC_CHAIN_SELF_DISTR / panel$OXYC_IN
# panel$OXYC_CHAIN_MANUF_percent = panel$OXYC_CHAIN_MANUF / panel$OXYC_IN
# panel$OXYC_INDEP_DISTR_percent =  panel$OXYC_INDEP_DISTR / panel$OXYC_IN
# panel$OXYC_INDEP_MANUF_percent =  panel$OXYC_INDEP_MANUF / panel$OXYC_IN
# # panel$OXYC_RETURN_percent = panel$OXYC_RETURN / panel$OXYC_IN
# # panel$OXYC_COMSUM_percent = panel$OXYC / panel$OXYC_IN
# 
# 
# panel$OXYM_IN = panel$OXYM_CHAIN_DISTR + panel$OXYM_CHAIN_SELF_DISTR + panel$OXYM_CHAIN_MANUF + panel$OXYM_INDEP_DISTR + panel$OXYM_INDEP_MANUF
# panel$OXYM = panel$OXYM_IN - panel$OXYM_RETURN
# panel$OXYM_CHAIN_DISTR_percent = panel$OXYM_CHAIN_DISTR / panel$OXYM_IN
# panel$OXYM_CHAIN_SELF_DISTR_percent = panel$OXYM_CHAIN_SELF_DISTR / panel$OXYM_IN
# panel$OXYM_CHAIN_MANUF_percent = panel$OXYM_CHAIN_MANUF / panel$OXYM_IN
# panel$OXYM_INDEP_DISTR_percent =  panel$OXYM_INDEP_DISTR / panel$OXYM_IN
# panel$OXYM_INDEP_MANUF_percent =  panel$OXYM_INDEP_MANUF / panel$OXYM_IN
# # panel$OXYM_RETURN_percent = panel$OXYM_RETURN / panel$OXYM_IN
# # panel$OXYM_COMSUM_percent = panel$OXYM / panel$OXYM_IN
# 
# 
# panel$TAP_IN = panel$TAP_CHAIN_DISTR + panel$TAP_CHAIN_SELF_DISTR + panel$TAP_CHAIN_MANUF + panel$TAP_INDEP_DISTR + panel$TAP_INDEP_MANUF
# panel$TAP = panel$TAP_IN - panel$TAP_RETURN
# panel$TAP_CHAIN_DISTR_percent = panel$TAP_CHAIN_DISTR / panel$TAP_IN
# panel$TAP_CHAIN_SELF_DISTR_percent = panel$TAP_CHAIN_SELF_DISTR / panel$TAP_IN
# panel$TAP_CHAIN_MANUF_percent = panel$TAP_CHAIN_MANUF / panel$TAP_IN
# panel$TAP_INDEP_DISTR_percent =  panel$TAP_INDEP_DISTR / panel$TAP_IN
# panel$TAP_INDEP_MANUF_percent =  panel$TAP_INDEP_MANUF / panel$TAP_IN
# # panel$TAP_RETURN_percent = panel$TAP_RETURN / panel$TAP_IN
# # panel$TAP_COMSUM_percent = panel$TAP / panel$TAP_IN
# 
# 
# panel$MME_OPI_IN =  panel$COD_IN + panel$TAP_IN + panel$OXYM_IN + panel$OXYC_IN + panel$MORPH_IN + panel$MEP_IN + panel$HYDROM_IN + panel$HYDROC_IN  + panel$FENT_IN
# panel$MME_OPI_RETURN = panel$COD_RETURN + panel$TAP_RETURN + panel$OXYM_RETURN + panel$OXYC_RETURN + panel$MORPH_RETURN + panel$MEP_RETURN  + panel$HYDROM_RETURN + panel$HYDROC_RETURN  + panel$FENT_RETURN
# panel$MME_OPI = panel$MME_OPI_IN - panel$MME_OPI_RETURN
# panel$OPI_CHAIN_DISTR_percent = (panel$COD_CHAIN_DISTR + panel$TAP_CHAIN_DISTR + panel$OXYM_CHAIN_DISTR + panel$OXYC_CHAIN_DISTR + panel$MORPH_CHAIN_DISTR + panel$MEP_CHAIN_DISTR  +  panel$HYDROM_CHAIN_DISTR + panel$HYDROC_CHAIN_DISTR  + panel$FENT_CHAIN_DISTR ) / panel$MME_OPI_IN
# panel$OPI_CHAIN_SELF_DISTR_percent = (panel$COD_CHAIN_SELF_DISTR + panel$TAP_CHAIN_SELF_DISTR + panel$OXYM_CHAIN_SELF_DISTR + panel$OXYC_CHAIN_SELF_DISTR + panel$MORPH_CHAIN_SELF_DISTR + panel$MEP_CHAIN_SELF_DISTR  + panel$HYDROM_CHAIN_SELF_DISTR + panel$HYDROC_CHAIN_SELF_DISTR  + panel$FENT_CHAIN_SELF_DISTR ) / panel$MME_OPI_IN
# panel$OPI_CHAIN_MANUF_percent = (panel$COD_CHAIN_MANUF + panel$TAP_CHAIN_MANUF + panel$OXYM_CHAIN_MANUF + panel$OXYC_CHAIN_MANUF + panel$MORPH_CHAIN_MANUF + panel$MEP_CHAIN_MANUF  + panel$HYDROM_CHAIN_MANUF + panel$HYDROC_CHAIN_MANUF  + panel$FENT_CHAIN_MANUF)/ panel$MME_OPI_IN
# panel$OPI_INDEP_DISTR_percent = (panel$COD_INDEP_DISTR + panel$TAP_INDEP_DISTR + panel$OXYM_INDEP_DISTR + panel$OXYC_INDEP_DISTR + panel$MORPH_INDEP_DISTR + panel$MEP_INDEP_DISTR  + panel$HYDROM_INDEP_DISTR + panel$HYDROC_INDEP_DISTR  + panel$FENT_INDEP_DISTR) / panel$MME_OPI_IN
# panel$OPI_INDEP_MANUF_percent = (panel$COD_INDEP_MANUF + panel$TAP_INDEP_MANUF + panel$OXYM_INDEP_MANUF + panel$OXYC_INDEP_MANUF + panel$MORPH_INDEP_MANUF + panel$MEP_INDEP_MANUF  + panel$HYDROM_INDEP_MANUF + panel$HYDROC_INDEP_MANUF  + panel$FENT_INDEP_MANUF) / panel$MME_OPI_IN
# panel$MME_OPI_RETURN_percent = panel$MME_OPI_RETURN / panel$MME_OPI_IN 
# panel$MME_OPI_COMSUM_percent = panel$MME_OPI / panel$MME_OPI_IN 
# 
# 
# 
# panel$OPI_CHAIN_DISTR= (panel$COD_CHAIN_DISTR + panel$TAP_CHAIN_DISTR + panel$OXYM_CHAIN_DISTR + panel$OXYC_CHAIN_DISTR + panel$MORPH_CHAIN_DISTR + panel$MEP_CHAIN_DISTR  +  panel$HYDROM_CHAIN_DISTR + panel$HYDROC_CHAIN_DISTR  + panel$FENT_CHAIN_DISTR ) 
# panel$OPI_CHAIN_SELF_DISTR = (panel$COD_CHAIN_SELF_DISTR + panel$TAP_CHAIN_SELF_DISTR + panel$OXYM_CHAIN_SELF_DISTR + panel$OXYC_CHAIN_SELF_DISTR + panel$MORPH_CHAIN_SELF_DISTR + panel$MEP_CHAIN_SELF_DISTR  + panel$HYDROM_CHAIN_SELF_DISTR + panel$HYDROC_CHAIN_SELF_DISTR  + panel$FENT_CHAIN_SELF_DISTR ) 
# panel$OPI_CHAIN_MANUF = (panel$COD_CHAIN_MANUF + panel$TAP_CHAIN_MANUF + panel$OXYM_CHAIN_MANUF + panel$OXYC_CHAIN_MANUF + panel$MORPH_CHAIN_MANUF + panel$MEP_CHAIN_MANUF  + panel$HYDROM_CHAIN_MANUF + panel$HYDROC_CHAIN_MANUF  + panel$FENT_CHAIN_MANUF)
# panel$OPI_INDEP_DISTR= (panel$COD_INDEP_DISTR + panel$TAP_INDEP_DISTR + panel$OXYM_INDEP_DISTR + panel$OXYC_INDEP_DISTR + panel$MORPH_INDEP_DISTR + panel$MEP_INDEP_DISTR  + panel$HYDROM_INDEP_DISTR + panel$HYDROC_INDEP_DISTR  + panel$FENT_INDEP_DISTR) 
# panel$OPI_INDEP_MANUF= (panel$COD_INDEP_MANUF + panel$TAP_INDEP_MANUF + panel$OXYM_INDEP_MANUF + panel$OXYC_INDEP_MANUF + panel$MORPH_INDEP_MANUF + panel$MEP_INDEP_MANUF  + panel$HYDROM_INDEP_MANUF + panel$HYDROC_INDEP_MANUF  + panel$FENT_INDEP_MANUF) 
# panel$MME_OPI_RETURN_percent = panel$MME_OPI_RETURN
# panel$MME_OPI_COMSUM = panel$MME_OPI
# 
# 
# 
# panel$MME_ANTI_IN = panel$BUPR_IN + panel$METH_IN
# panel$MME_ANTI_RETURN = panel$BUPR_RETURN + panel$METH_RETURN
# panel$MME_ANTI = panel$MME_ANTI_IN  -  panel$MME_ANTI_RETURN 
# panel$ANTI_CHAIN_DISTR_percent = (panel$BUPR_CHAIN_DISTR+ panel$METH_CHAIN_DISTR) / panel$MME_ANTI_IN
# panel$ANTI_CHAIN_SELF_DISTR_percent = (panel$BUPR_CHAIN_SELF_DISTR + panel$METH_CHAIN_SELF_DISTR) / panel$MME_ANTI_IN
# panel$ANTI_CHAIN_MANUF_percent = (panel$BUPR_CHAIN_MANUF + panel$METH_CHAIN_MANUF) / panel$MME_ANTI_IN
# panel$ANTI_INDEP_DISTR_percent = (panel$BUPR_INDEP_DISTR + panel$METH_INDEP_DISTR) / panel$MME_ANTI_IN
# panel$ANTI_INDEP_MANUF_percent = (panel$BUPR_INDEP_MANUF + panel$METH_INDEP_MANUF) / panel$MME_ANTI_IN
# panel$MME_ANTI_RETURN_percent = panel$MME_ANTI_RETURN / panel$MME_ANTI_IN 
# panel$MME_ANTI_COMSUM_percent = panel$MME_ANTI / panel$MME_ANTI_IN 
# 
# 
# 
# panel$ANTI_CHAIN_DISTR = (panel$BUPR_CHAIN_DISTR+ panel$METH_CHAIN_DISTR) 
# panel$ANTI_CHAIN_SELF_DISTR = (panel$BUPR_CHAIN_SELF_DISTR + panel$METH_CHAIN_SELF_DISTR) 
# panel$ANTI_CHAIN_MANUF = (panel$BUPR_CHAIN_MANUF + panel$METH_CHAIN_MANUF)
# panel$ANTI_INDEP_DISTR = (panel$BUPR_INDEP_DISTR + panel$METH_INDEP_DISTR)
# panel$ANTI_INDEP_MANUF = (panel$BUPR_INDEP_MANUF + panel$METH_INDEP_MANUF) 
# panel$MME_ANTI_RETURN = panel$MME_ANTI_RETURN
# panel$MME_ANTI_COMSUM = panel$MME_ANTI 
# 
# 
# 
# 
# panel$MME_OPI_PCAP = panel$MME_OPI /panel$POP
# panel$MME_OPI_IN_PCAP = panel$MME_OPI_IN /panel$POP
# panel$MME_OPI_RETURN_PCAP = panel$MME_OPI_RETURN /panel$POP
# 
# 
# 
# panel$MME_ANTI_PCAP = panel$MME_ANTI /panel$POP
# panel$MME_ANTI_IN_PCAP = panel$MME_ANTI_IN /panel$POP
# panel$MME_ANTI_RETURN_PCAP = panel$MME_ANTI_RETURN /panel$POP


#for (j in 1:ncol(panel)) set(panel, which(is.infinite(panel[[j]])), j, NA)



panel = as.data.table(panel)

fwrite(panel, file = "D:/Research with Jorge and Jonathan since Nov 2019/SLCG/STATA regression/panel7.csv")


#######################################################
############### Create More Variables #################
#######################################################

panel = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/STATA regression/panel7.csv")
panel$OPI_IN_PCAP = (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_CHAIN_SELF_DISTR + panel$OPI_INDEP_DISTR) / panel$POP
panel$OPI_CHAIN_EXTERNAL_DISTR_pcap = panel$OPI_CHAIN_EXTERNAL_DISTR/ panel$POP
panel$OPI_CHAIN_SELF_DISTR_pcap = panel$OPI_CHAIN_SELF_DISTR / panel$POP
panel$OPI_INDEP_DISTR_pcap = panel$OPI_INDEP_DISTR / panel$POP

panel$OPI_WHOLESALER_pcap = (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_INDEP_DISTR) / panel$POP
panel$OPI_SELF_DISTR_pcap = panel$OPI_SELF_DISTR / panel$POP

panel$OPI_CHAIN_EXTERNAL_DISTR_perc = 100 * panel$OPI_CHAIN_EXTERNAL_DISTR / (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_CHAIN_SELF_DISTR + panel$OPI_INDEP_DISTR)
panel$OPI_CHAIN_SELF_DISTR_perc = 100* panel$OPI_CHAIN_SELF_DISTR / (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_CHAIN_SELF_DISTR + panel$OPI_INDEP_DISTR)
panel$OPI_INDEP_DISTR_perc = 100 * panel$OPI_INDEP_DISTR / (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_CHAIN_SELF_DISTR + panel$OPI_INDEP_DISTR)

panel$OPI_CHAIN_pcap = (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_CHAIN_SELF_DISTR )/ panel$POP


panel$OPI_WHOLESALER_perc = 100 * (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_INDEP_DISTR) / (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_CHAIN_SELF_DISTR + panel$OPI_INDEP_DISTR)
panel$OPI_SELF_DISTR_perc = 100 * panel$OPI_CHAIN_SELF_DISTR / (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_CHAIN_SELF_DISTR + panel$OPI_INDEP_DISTR)

panel$OPI_WHOLESALER_CHAIN_perc = 100 * panel$OPI_CHAIN_EXTERNAL_DISTR / (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_INDEP_DISTR)
panel$OPI_WHOLESALER_INDEP_perc = 100 * panel$OPI_INDEP_DISTR / (panel$OPI_CHAIN_EXTERNAL_DISTR + panel$OPI_INDEP_DISTR)


fwrite(panel, file = "D:/Research with Jorge and Jonathan since Nov 2019/SLCG/STATA regression/panel7.csv")








