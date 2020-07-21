library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)

chains_have_self = "(DISCOUNT DRUG( MART)*)|(CVS)|(HAPPY)|(AMERICAN DRUG)|(KROGER)|(MEIJER)|(WALGREEN(S)*)|(WAL-MART)|(WALMART)|(ALBERTSONS)|(BROOKSHIRE)|(DUANE( READE)*)|(ECKERD CORPORATION)|(HY-VEE)|(HARCO)|(HEB)|(KERR)|(LONGS)|(PUBLIX)|(PRICE CHOPPER)|(FAMILYMEDS)|(RITE.*AID)|(SMITH'S)|(THRIFTY PAYLESS)|((WINN )*DIXIE)"

corresponds = data[grepl("CHAIN", BUYER_BUS_ACT) ,  
                   
                   .(no_self_distr = length(unique(REPORTER_DEA_NO[ (grepl(chains_have_self, BUYER_NAME, ignore.case = TRUE) | grepl(chains_have_self, BUYER_ADDL_CO_INFO, ignore.case = TRUE) ) & (grepl(chains_have_self, REPORTER_NAME, ignore.case = TRUE)  | grepl(chains_have_self, REPORTER_ADDL_CO_INFO, ignore.case = TRUE) ) ])), 
                     
                     no_ext_distr = length(unique(REPORTER_DEA_NO[ (!grepl(chains_have_self, REPORTER_NAME, ignore.case = TRUE)  & !grepl(chains_have_self, REPORTER_ADDL_CO_INFO, ignore.case = TRUE) ) ])),
                     
                     no_total_distr = length(unique(REPORTER_DEA_NO))
                     
                     ),
                   
                   by = .(BUYER_DEA_NO, data_ARCOS_SLCG_YEAR ) ]


setorder(corresponds, BUYER_DEA_NO, data_ARCOS_SLCG_YEAR   )

#### any chain that only depends on self-distribution center? ####

# There are 47863 unique Chain DEA_NO
corresponds[, length(unique(BUYER_DEA_NO)) , ]

# There are 13339 unique Chain DEA_NO that had no internal distribution centers (only relied on external distribution centers)
corresponds1 = corresponds[, .(self_distr = sum(no_self_distr), 
                               ext_distr = sum(no_ext_distr),
                               total_distr = sum(no_total_distr)
                               
                               ) , 
                           
                           by = .(BUYER_DEA_NO)]


corresponds1[, length(BUYER_DEA_NO[self_distr == 0]) , ]


## Check more!
   
data = data[ , data_ARCOS_SLCG_BUYER_GEO_ID := NULL   , ]
data = data[ , BUYER_DEA_NO := NULL   , ]
data = data[ , REPORTER_DEA_NO := NULL   , ]

#play2 = data[(REPORTER_BUS_ACT == "DISTRIBUTOR") & ( (DRUG_NAME == "CODEINE") |  (DRUG_NAME == "FENTANYL") | (DRUG_NAME == "HYDROCODONE") | (DRUG_NAME == "HYDROMORPHONE") | (DRUG_NAME == "MEPERIDINE") | (DRUG_NAME == "MORPHINE") | (DRUG_NAME == "OXYCODONE") |  (DRUG_NAME == "OXYMORPHONE")  | (DRUG_NAME == "TAPENTADOL") )  , .(mme = sum(MME)) , .(by = TRANSACTION_CODE) ]

# Among these drugs that were processed by any types of pharmacies, 96.9% of them belongs Transaction Code S
play3 = data[( (DRUG_NAME == "CODEINE") |  (DRUG_NAME == "FENTANYL") | (DRUG_NAME == "HYDROCODONE") | (DRUG_NAME == "HYDROMORPHONE") | (DRUG_NAME == "MEPERIDINE") | (DRUG_NAME == "MORPHINE") | (DRUG_NAME == "OXYCODONE") |  (DRUG_NAME == "OXYMORPHONE")  | (DRUG_NAME == "TAPENTADOL") )  , .(mme = sum(MME)) , .(by = TRANSACTION_CODE) ]

# Among these drugs that were processed by chain or independent pharmacies, 97% of them belongs Transaction Code S
play4 = data[( (DRUG_NAME == "CODEINE") |  (DRUG_NAME == "FENTANYL") | (DRUG_NAME == "HYDROCODONE") | (DRUG_NAME == "HYDROMORPHONE") | (DRUG_NAME == "MEPERIDINE") | (DRUG_NAME == "MORPHINE") | (DRUG_NAME == "OXYCODONE") |  (DRUG_NAME == "OXYMORPHONE")  | (DRUG_NAME == "TAPENTADOL") ) & (BUYER_BUS_ACT == "CHAIN PHARMACY" | BUYER_BUS_ACT == "RETAIL PHARMACY")  , .(mme = sum(MME)) , .(by = TRANSACTION_CODE) ]

top_chains = "(CVS)|(KROGER)|(WALGREEN(S)*)|(WAL-MART)|(WALMART)|(RITE.*AID)"

# 686286441395
data[(grepl(top_chains, BUYER_NAME, ignore.case = TRUE) | grepl(top_chains, BUYER_ADDL_CO_INFO, ignore.case = TRUE) ) & ( (DRUG_NAME == "CODEINE") |  (DRUG_NAME == "FENTANYL") | (DRUG_NAME == "HYDROCODONE") | (DRUG_NAME == "HYDROMORPHONE") | (DRUG_NAME == "MEPERIDINE") | (DRUG_NAME == "MORPHINE") | (DRUG_NAME == "OXYCODONE") |  (DRUG_NAME == "OXYMORPHONE")  | (DRUG_NAME == "TAPENTADOL") )  , sum(MME) , ]

# 1.672803e+12
data[ ( (DRUG_NAME == "CODEINE") |  (DRUG_NAME == "FENTANYL") | (DRUG_NAME == "HYDROCODONE") | (DRUG_NAME == "HYDROMORPHONE") | (DRUG_NAME == "MEPERIDINE") | (DRUG_NAME == "MORPHINE") | (DRUG_NAME == "OXYCODONE") |  (DRUG_NAME == "OXYMORPHONE")  | (DRUG_NAME == "TAPENTADOL") ) & (BUYER_BUS_ACT == "CHAIN PHARMACY" | BUYER_BUS_ACT == "RETAIL PHARMACY")  , sum(MME) , ]


# 898146193511
data[ ( (DRUG_NAME == "CODEINE") |  (DRUG_NAME == "FENTANYL") | (DRUG_NAME == "HYDROCODONE") | (DRUG_NAME == "HYDROMORPHONE") | (DRUG_NAME == "MEPERIDINE") | (DRUG_NAME == "MORPHINE") | (DRUG_NAME == "OXYCODONE") |  (DRUG_NAME == "OXYMORPHONE")  | (DRUG_NAME == "TAPENTADOL") ) & (BUYER_BUS_ACT == "CHAIN PHARMACY")  , sum(MME) , ]


data[ ( (DRUG_NAME == "CODEINE") |  (DRUG_NAME == "FENTANYL") | (DRUG_NAME == "HYDROCODONE") | (DRUG_NAME == "HYDROMORPHONE") | (DRUG_NAME == "MEPERIDINE") | (DRUG_NAME == "MORPHINE") | (DRUG_NAME == "OXYCODONE") |  (DRUG_NAME == "OXYMORPHONE")  | (DRUG_NAME == "TAPENTADOL") ) & (BUYER_BUS_ACT == "CHAIN PHARMACY")  , sum(MME) , ]


# for plots
panel = read.csv("D:/Research with Jorge and Jonathan since Nov 2019/SLCG/STATA regression/panel7.csv")

panel = as.data.table(panel)

# Create a new column

nation_over_time = panel[,   .(CHAIN_SELF_DISTR = sum(OPI_CHAIN_SELF_DISTR), 
                                CHAIN_EXTERNAL_DISTR = sum(OPI_CHAIN_EXTERNAL_DISTR), 
                                INDEP_DISTR = sum(OPI_INDEP_DISTR), 
                                pop = sum(POP)) , 
                          by = .(YEAR)]


nation_over_time$CHAIN_SELF_DISTR_perc = nation_over_time$CHAIN_SELF_DISTR / (nation_over_time$CHAIN_SELF_DISTR + nation_over_time$CHAIN_EXTERNAL_DISTR + nation_over_time$INDEP_DISTR)
nation_over_time$CHAIN_EXTERNAL_DISTR_perc = nation_over_time$CHAIN_EXTERNAL_DISTR / (nation_over_time$CHAIN_SELF_DISTR + nation_over_time$CHAIN_EXTERNAL_DISTR + nation_over_time$INDEP_DISTR)
nation_over_time$INDEP_DISTR_perc = nation_over_time$INDEP_DISTR / (nation_over_time$CHAIN_SELF_DISTR + nation_over_time$CHAIN_EXTERNAL_DISTR + nation_over_time$INDEP_DISTR)

nation_over_time$CHAIN_SELF_DISTR_pcap = nation_over_time$CHAIN_SELF_DISTR / nation_over_time$pop
nation_over_time$CHAIN_EXTERNAL_DISTR_pcap = nation_over_time$CHAIN_EXTERNAL_DISTR / nation_over_time$pop
nation_over_time$INDEP_DISTR_pcap = nation_over_time$INDEP_DISTR / nation_over_time$pop

nation_over_time$opi_inflow_pcap = (nation_over_time$CHAIN_SELF_DISTR + nation_over_time$CHAIN_EXTERNAL_DISTR + nation_over_time$INDEP_DISTR) / nation_over_time$pop


nation_over_time_percent = melt(nation_over_time, id.vars = c("YEAR"), measure.vars = c("CHAIN_SELF_DISTR_perc", "CHAIN_EXTERNAL_DISTR_perc" , "INDEP_DISTR_perc"), variable.name = "percent")

nation_over_time_pcap = melt(nation_over_time, id.vars = c("YEAR"), measure.vars = c("CHAIN_SELF_DISTR_pcap", "CHAIN_EXTERNAL_DISTR_pcap", "INDEP_DISTR_pcap"), variable.name = "pcap")

nation_over_time_pcap2 = melt(nation_over_time, id.vars = c("YEAR"), measure.vars = c("opi_inflow_pcap"), variable.name = "pcap")


## Percentages over Year
ggplot(nation_over_time_percent) +
  aes(x = YEAR, y = value, fill = percent) +
  geom_area(size = 1L) +
  scale_fill_hue(labels = c("Chains' Internal Distribution", "Chains' External Distribution", "Independent Pharmacies")) +
  labs(x = "Year", y = "Percentage", title = "Percentage of Inventory Inflow through Channels over Time", fill = "Channels:") +
  ggthemes::theme_stata() +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 13),
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5))



ggplot(nation_over_time_pcap) +
  aes(x = YEAR, y = value, fill = pcap) +
  geom_area(size = 1L) +
  scale_fill_hue(labels = c("Chains' Internal Distribution", "Chains' External Distribution", "Independent Pharmacies")) +
  labs(x = "Year", y = "Opioids Per Capita", title = "Opioids through Channels over Time", fill = "Channels:") +
  ggthemes::theme_stata() + 
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 13),
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5))


ggplot(nation_over_time_pcap2) +
  aes(x = YEAR, y = value, fill = pcap) +
  geom_line(size = 1.3, colour = "#0c4c8a") +
  scale_fill_hue() +
  labs(x = "Year", y = "Opioids Per Capita", title = "Inventory Inflow over Time") +
  ggthemes::theme_stata()+
  ylim(0, 700) +
  theme(axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5))










