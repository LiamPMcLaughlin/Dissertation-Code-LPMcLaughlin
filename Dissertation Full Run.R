#######################################
##Liam McLaughlin - Dissertation data##
#######################################
###########Contents####################
# 1~Packages required                 #
# 2~Replication data                  #
# 3~Novel data                        #
# 4 Novel data only official          #
#######################################

#########################
##1 ~ Packages Required##
#########################

if(!require("plm")) install.packages("plm") 
library("plm")
if(!require("stargazer")) install.packages("stargazer") 
library("stargazer")
if(!require("MASS")) install.packages("MASS") 
library("MASS")
if(!require("dplyr")) install.packages("dplyr") 
library("dplyr")
if(!require("forcats")) install.packages("dforcats") 
library("forcats")

#######################
##2 ~Replication Data##
#######################
data <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Data from abroad.csv",header = FALSE)
library(tidyverse)
data<-data |>rename(Country = V1, Actual= V2, Predicted = V3)
data<-mutate(data, Country = as.character(Country), 
             Country = fct_recode(Country,"Argentina"="9","Australia"="10","Austria"="11", "Bangladesh"="16",
                                  "Bolivia"="19", "Brazil"="21", "Bulgaria"="27","Canada"="33",
                                  "Colombia"="44", "Democratic Republic of the Congo"="46",
                                  "Denmark"="54", "Dominican Republic"="56", "Ecuador"="58",
                                  "Egypt"="59", "El Salvador"="60", "Finland"="67", "France"="68",
                                  "Ghana"="81", "Honduras"="95","Hungary"="97" ,"Iceland"="99", "Indonesia"="101",
                                  "Iran"="102", "Ireland"="104", "Israel"="105", "Jamaica"="109",
                                  "Kenya"="114", "Cambodia"="115", "Laos"="120", "Lebanon"="121",
                                  "Malawi"="130", "Mozambique"= "144", "Namibia"="147", "Netherlands"="150",
                                  "Nicaragua"="157", "Norway"="162", "Paraguay"="169",
                                  "Peru"="170", "Poland"="173", "Zimbabwe"="181",
                                  "Romania"="183", "South Africa"="202", "Spain"="203",
                                  "Suriname"="207", "Sweden"="210", "Togo"="217",
                                  "Trinidad and Tobago"="220", "Tunisia"="222", "Turkey"="223",
                                  "USSR"="228", "United States"="231", "Burkina Faso"="233",
                                  "Venezuela"="236", "Yougoslavia"="248", "China"="351"))
levels(data$Country)
Crop<-rep(1:17,55)
data$Crop<-Crop
data <- mutate(data,  Crop = as.character(Crop),
               Crop = fct_recode(Crop, "Barley" = "1","Cabbages" = "2", "Carrots and Turnips"="3",
                                 "Cassava"="4","Coconuts"="5", "Seed Cotton"="6", "Groundnuts with shell"="7",
                                 "Maize"= "8", "Onions, dry" = "9", "Rice, paddy" = "10", "Sorghum"="11",
                                 "Soybeans"="12","Sugar Cane"= "13", "Sweet Potatoes" = "14", "Tomatoes"="15",
                                 "Wheat"="16", "Potatoes"="17"))
names(data)
data$Actual[data$Actual == 0]<-NA
data$Predicted[data$Predicted == 0]<-NA
data$lnactual<-log(data$Actual)
data$lnpredicted<-log(data$Predicted)

###############################
##2 ~ Replication Data Models##
###############################

model1a<-lm(lnactual~lnpredicted, data)
femodel1a<-plm(lnactual~ lnpredicted, data, index= c("Country"), model="within")
femodel2a<-plm(lnactual~ lnpredicted, data, index= c("Crop"), model="within")
model1b<-lm(lnactual~lnpredicted-1, data)
femodel1b<-plm(lnactual~ lnpredicted-1, data, index= c("Country"), model="within")
femodel2b<-plm(lnactual~ lnpredicted-1, data, index= c("Crop"), model="within")
stargazer(model1a,femodel1a,femodel2a,model1b,femodel1b,femodel2b,type="text", out = "table1.tex")

####################################
##2 ~ Repication Data Correlations##
####################################

by(data, data$Crop,function(data) cor.test(data$Predicted, data$Actual, use="pair", method="kendall"))
by(data, data$Crop,function(data) cor(data$Predicted, data$Actual, use="pair", method="pearson"))

##################
##3 ~ Novel Data##
##################

##########################
##3 ~ Actuals Novel Data##
##########################

actuals2000<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/FAOSTAT_data_en_1-8-2024.csv")
actuals2000<-actuals2000|> select(Item,Area,Year,Value,Flag.Description)
actuals2000<-mutate(actuals2000, Country = as.character(Area),
                    Country = fct_recode(Country,"AFG"="Afghanistan",
                                         "ALB"="Albania",
                                         "DZA"="Algeria",
                                         "AGO"="Angola",
                                         "ATG"="Antigua and Barbuda",
                                         "ARG"="Argentina",
                                         "ARM"="Armenia",
                                         "AUS"="Australia",
                                         "AUT"="Austria",
                                         "AZE"="Azerbaijan",
                                         "BHS"="Bahamas",
                                         "BHR"="Bahrain",
                                         "BGD"="Bangladesh",
                                         "BRB"="Barbados",
                                         "BLR"="Belarus",
                                         "BEL"="Belgium",
                                         "BLZ"="Belize",
                                         "BEN"="Benin",
                                         "BTN"="Bhutan",
                                         "BOL"="Bolivia (Plurinational State of)",
                                         "BIH"="Bosnia and Herzegovina",
                                         "BWA"="Botswana",
                                         "BRA"="Brazil",
                                         "BRN"="Brunei Darussalam",
                                         "BGR"="Bulgaria",
                                         "BFA"="Burkina Faso",
                                         "BDI"="Burundi",
                                         "CPV"="Cabo Verde",
                                         "KHM"="Cambodia",
                                         "CMR"="Cameroon",
                                         "CAN"="Canada",
                                         "CAF"="Central African Republic",
                                         "TCD"="Chad",
                                         "CHL"="Chile",
                                         "CHN"="China",
                                         "HKG"="China, Hong Kong SAR",
                                         "MAC"="China, Macao SAR",
                                         "CHN"="China, mainland",
                                         "TWN"="China, Taiwan Province of",
                                         "COL"="Colombia",
                                         "COM"="Comoros",
                                         "COG"="Congo",
                                         "COK"="Cook Islands",
                                         "CRI"="Costa Rica",
                                         "CIV"="Côte d'Ivoire",
                                         "HRV"="Croatia",
                                         "CUB"="Cuba",
                                         "CYP"="Cyprus",
                                         "CZE"="Czechia",
                                         "PRK"="Democratic People's Republic of Korea",
                                         "COD"="Democratic Republic of the Congo",
                                         "DEN"="Denmark",
                                         "DJI"="Djibouti",
                                         "DMA"="Dominica",
                                         "DOM"="Dominican Republic",
                                         "ECU"="Ecuador",
                                         "EGY"="Egypt",
                                         "SLV"="El Salvador",
                                         "GNQ"="Equatorial Guinea",
                                         "ERI"="Eritrea",
                                         "EST"="Estonia",
                                         "SWZ"="Eswatini",
                                         "ETH"="Ethiopia",
                                         "FRO"="Faroe Islands",
                                         "FJI"="Fiji",
                                         "FIN"="Finland",
                                         "FRA"="France",
                                         "PYF"="French Polynesia",
                                         "GAB"="Gabon",
                                         "GMB"="Gambia",
                                         "GEO"="Georgia",
                                         "DEU"="Germany",
                                         "GHA"="Ghana",
                                         "GRC"="Greece",
                                         "GRD"="Grenada",
                                         "GTM"="Guatemala",
                                         "GIN"="Guinea",
                                         "GNB"="Guinea-Bissau",
                                         "GUY"="Guyana",
                                         "HTI"="Haiti",
                                         "HND"="Honduras",
                                         "HUN"="Hungary",
                                         "ISL"="Iceland",
                                         "IND"="India",
                                         "IDN"="Indonesia",
                                         "IRN"="Iran (Islamic Republic of)",
                                         "IRQ"="Iraq",
                                         "IRL"="Ireland",
                                         "ISR"="Israel",
                                         "ITA"="Italy",
                                         "JAM"="Jamaica",
                                         "JPN"="Japan",
                                         "JOR"="Jordan",
                                         "KAZ"="Kazakhstan",
                                         "KEN"="Kenya",
                                         "KIR"="Kiribati",
                                         "KWT"="Kuwait",
                                         "KGZ"="Kyrgyzstan",
                                         "LAO"="Lao People's Democratic Republic",
                                         "LVA"="Latvia",
                                         "LBN"="Lebanon",
                                         "LSO"="Lesotho",
                                         "LBR"="Liberia",
                                         "LBY"="Libya",
                                         "LTU"="Lithuania",
                                         "LUX"="Luxembourg",
                                         "MDG"="Madagascar",
                                         "MWI"="Malawi",
                                         "MYS"="Malaysia",
                                         "MDV"="Maldives",
                                         "MLI"="Mali",
                                         "MLT"="Malta",
                                         "MHL"="Marshall Islands",
                                         "MRT"="Mauritania",
                                         "MUS"="Mauritius",
                                         "MEX"="Mexico",
                                         "FSM"="Micronesia (Federated States of)",
                                         "MNG"="Mongolia",
                                         "MNE"="Montenegro",
                                         "MAR"="Morocco",
                                         "MOZ"="Mozambique",
                                         "MMR"="Myanmar",
                                         "NAM"="Namibia",
                                         "NRU"="Nauru",
                                         "NPL"="Nepal",
                                         "NDL"="Netherlands (Kingdom of the)",
                                         "NCL"="New Caledonia",
                                         "NZL"="New Zealand",
                                         "NIC"="Nicaragua",
                                         "NER"="Niger",
                                         "NGA"="Nigeria",
                                         "NIU"="Niue",
                                         "MKD"="North Macedonia",
                                         "NOR"="Norway",
                                         "OMN"="Oman",
                                         "PAK"="Pakistan",
                                         "PSE"="Palestine",
                                         "PAN"="Panama",
                                         "PNG"="Papua New Guinea",
                                         "PRY"="Paraguay",
                                         "PER"="Peru",
                                         "PHL"="Philippines",
                                         "POL"="Poland",
                                         "POR"="Portugal",
                                         "PRI"="Puerto Rico",
                                         "QAT"="Qatar",
                                         "KOR"="Republic of Korea",
                                         "MDA"="Republic of Moldova",
                                         "ROU"="Romania",
                                         "RUS"="Russian Federation",
                                         "RWA"="Rwanda",
                                         "KNA"="Saint Kitts and Nevis",
                                         "LCA"="Saint Lucia",
                                         "VCT"="Saint Vincent and the Grenadines",
                                         "WSM"="Samoa",
                                         "STP"="Sao Tome and Principe",
                                         "SAU"="Saudi Arabia",
                                         "SEN"="Senegal",
                                         "SRB"="Serbia",
                                         "SYC"="Seychelles",
                                         "SLE"="Sierra Leone",
                                         "SGP"="Singapore",
                                         "SVK"="Slovakia",
                                         "SVN"="Slovenia",
                                         "SLB"="Solomon Islands",
                                         "SOM"="Somalia",
                                         "ZAF"="South Africa",
                                         "ESP"="Spain",
                                         "LKA"="Sri Lanka",
                                         "SDN"="Sudan (former)",
                                         "SUR"="Suriname",
                                         "SWE"="Sweden",
                                         "CHE"="Switzerland",
                                         "SYR"="Syrian Arab Republic",
                                         "TJK"="Tajikistan",
                                         "THA"="Thailand",
                                         "TLS"="Timor-Leste",
                                         "TGO"="Togo",
                                         "TKL"="Tokelau",
                                         "TKL"="Tonga",
                                         "TTO"="Trinidad and Tobago",
                                         "TUN"="Tunisia",
                                         "TUR"="Türkiye",
                                         "TKM"="Turkmenistan",
                                         "TUV"="Tuvalu",
                                         "UGA"="Uganda",
                                         "UKR" = "Ukraine",
                                         "UAE" = "United Arab Emirates",
                                         "GBR"="United Kingdom of Great Britain and Northern Ireland",
                                         "TZA" = "United Republic of Tanzania",
                                         "USA" = "United States of America",
                                         "URY" = "Uruguay",
                                         "UZB" = "Uzbekistan",
                                         "VUT" = "Vanuatu",
                                         "VEN" = "Venezuela (Bolivarian Republic of)",
                                         "VNM" = "Viet Nam",
                                         "YEM" = "Yemen",
                                         "ZMB" = "Zambia",
                                         "ZWE" = "Zimbabwe",
                                         "GUF" = "French Guiana",
                                         "GLP"= "Guadeloupe",
                                         "MTQ"="Martinique",
                                         "SRB"="Serbia and Montenegro",
                                         "REU"="Réunion"),
                    CRP=as.factor(Item),
                    CRP=fct_recode(CRP,"car"="Carrots and turnips",
                                   "cab"="Cabbages",
                                   "bck"="Buckwheat",
                                   "brl"="Barley",
                                   "ban"="Bananas",
                                   "yam"="Yams",
                                   "wpo"="Potatoes",
                                   "whe"="Wheat",
                                   "rce"="Rice",
                                   "tom"="Tomatoes",
                                   "tob"="Unmanufactured tobacco",
                                   "tea"="Tea leaves",
                                   "spo"="Sweet potatoes",
                                   "sfl"="Sunflower seed",
                                   "suc"="Sugar cane",
                                   "sub"="Sugar beet",
                                   "soy"="Soya beans",
                                   "srg"="Sorghum",
                                   "rye"="Rye",
                                   "rub"="Natural rubber in primary forms",
                                   "rcg"="Canary seed",
                                   "rsd"="Rape or colza seed",
                                   "pig"="Pigeon peas, dry",
                                   "mlt"="Millet",
                                   "oni"="Onions and shallots, green",
                                   "olv"="Olives",
                                   "olp"="Oil palm fruit",
                                   "oat"="Oats",
                                   "grd"="Groundnuts, excluding shelled",
                                   "flx"="Flax, raw or retted",
                                   "pea"="Chick peas, dry",
                                   "cow"="Cow peas, dry",
                                   "cot"="Seed cotton, unginned",
                                   "cof"="Coffee, green",
                                   "con"="Coconuts, in shell",
                                   "coc"="Cocoa beans",
                                   "cit"="Other citrus fruit, n.e.c.",
                                   "csv"="Cassava, fresh",))
actuals2000officals<-actuals2000

bananamanipulation2000<-actuals2000[which(actuals2000$CRP=="ban"| actuals2000$CRP=="Plantains and cooking bananas" ),]
bananamanipulation2000<-bananamanipulation2000%>% group_by(Country) %>% summarise(Value = sum(Value))
bananamanipulation2000 <- as.data.frame(bananamanipulation2000)
actuals2000 <- subset(actuals2000, actuals2000$CRP!="Plantains and cooking bananas" & actuals2000$CRP!="Plantains and cooking bananas" ) 
actuals2000<-actuals2000|>select(Country,CRP,Value)
bananamanipulation2000
bananamanipulation2000$CRP<-rep("ban",131)
actuals2000<-rbind(actuals2000,bananamanipulation2000)

actuals2010 <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/FAOSTAT_data_en_1-7-2024.csv")
names(actuals2010)
actuals2010<- actuals2010|>select(Item,Area,Year,Value,Flag.Description)
actuals2010<-mutate(actuals2010, Country = as.character(Area),
                    Country = fct_recode(Country,"AFG"="Afghanistan",
                                         "ALB"="Albania",
                                         "DZA"="Algeria",
                                         "AGO"="Angola",
                                         "ATG"="Antigua and Barbuda",
                                         "ARG"="Argentina",
                                         "ARM"="Armenia",
                                         "AUS"="Australia",
                                         "AUT"="Austria",
                                         "AZE"="Azerbaijan",
                                         "BHS"="Bahamas",
                                         "BHR"="Bahrain",
                                         "BGD"="Bangladesh",
                                         "BRB"="Barbados",
                                         "BLR"="Belarus",
                                         "BEL"="Belgium",
                                         "BLZ"="Belize",
                                         "BEN"="Benin",
                                         "BTN"="Bhutan",
                                         "BOL"="Bolivia (Plurinational State of)",
                                         "BIH"="Bosnia and Herzegovina",
                                         "BWA"="Botswana",
                                         "BRA"="Brazil",
                                         "BRN"="Brunei Darussalam",
                                         "BRG"="Bulgaria",
                                         "BFA"="Burkina Faso",
                                         "BDI"="Burundi",
                                         "CPV"="Cabo Verde",
                                         "KHM"="Cambodia",
                                         "CMR"="Cameroon",
                                         "CAN"="Canada",
                                         "CAF"="Central African Republic",
                                         "TCD"="Chad",
                                         "CHL"="Chile",
                                         "CHN"="China",
                                         "HKG"="China, Hong Kong SAR",
                                         "MAC"="China, Macao SAR",
                                         "CHN"="China, mainland",
                                         "TWN"="China, Taiwan Province of",
                                         "COL"="Colombia",
                                         "COM"="Comoros",
                                         "COG"="Congo",
                                         "COK"="Cook Islands",
                                         "CRI"="Costa Rica",
                                         "CIV"="Côte d'Ivoire",
                                         "HRV"="Croatia",
                                         "CUB"="Cuba",
                                         "CYP"="Cyprus",
                                         "CZE"="Czechia",
                                         "PRK"="Democratic People's Republic of Korea",
                                         "COD"="Democratic Republic of the Congo",
                                         "DNK"="Denmark",
                                         "DJI"="Djibouti",
                                         "DMA"="Dominica",
                                         "DOM"="Dominican Republic",
                                         "ECU"="Ecuador",
                                         "EGY"="Egypt",
                                         "SLV"="El Salvador",
                                         "GNQ"="Equatorial Guinea",
                                         "ERI"="Eritrea",
                                         "EST"="Estonia",
                                         "SWZ"="Eswatini",
                                         "ETH"="Ethiopia",
                                         "FRO"="Faroe Islands",
                                         "FJI"="Fiji",
                                         "FIN"="Finland",
                                         "FRA"="France",
                                         "PYF"="French Polynesia",
                                         "GAB"="Gabon",
                                         "GMB"="Gambia",
                                         "GEO"="Georgia",
                                         "DEU"="Germany",
                                         "GHA"="Ghana",
                                         "GRC"="Greece",
                                         "GRD"="Grenada",
                                         "GTM"="Guatemala",
                                         "GIN"="Guinea",
                                         "GNB"="Guinea-Bissau",
                                         "GUY"="Guyana",
                                         "HTI"="Haiti",
                                         "HND"="Honduras",
                                         "HUN"="Hungary",
                                         "ISL"="Iceland",
                                         "IND"="India",
                                         "IDN"="Indonesia",
                                         "IRN"="Iran (Islamic Republic of)",
                                         "IRQ"="Iraq",
                                         "IRL"="Ireland",
                                         "ISR"="Israel",
                                         "ITA"="Italy",
                                         "JAM"="Jamaica",
                                         "JPN"="Japan",
                                         "JOR"="Jordan",
                                         "KAZ"="Kazakhstan",
                                         "KEN"="Kenya",
                                         "KIR"="Kiribati",
                                         "KWT"="Kuwait",
                                         "KGZ"="Kyrgyzstan",
                                         "LAO"="Lao People's Democratic Republic",
                                         "LAV"="Latvia",
                                         "LBN"="Lebanon",
                                         "LSO"="Lesotho",
                                         "LBR"="Liberia",
                                         "LBY"="Libya",
                                         "LTU"="Lithuania",
                                         "LUX"="Luxembourg",
                                         "MDG"="Madagascar",
                                         "MWI"="Malawi",
                                         "MYS"="Malaysia",
                                         "MDV"="Maldives",
                                         "MLI"="Mali",
                                         "MLT"="Malta",
                                         "MHL"="Marshall Islands",
                                         "MRT"="Mauritania",
                                         "MUS"="Mauritius",
                                         "MEX"="Mexico",
                                         "FSM"="Micronesia (Federated States of)",
                                         "MNG"="Mongolia",
                                         "MNE"="Montenegro",
                                         "MAR"="Morocco",
                                         "MOZ"="Mozambique",
                                         "MMR"="Myanmar",
                                         "NAM"="Namibia",
                                         "NRU"="Nauru",
                                         "NPL"="Nepal",
                                         "NDL"="Netherlands (Kingdom of the)",
                                         "NCL"="New Caledonia",
                                         "NZL"="New Zealand",
                                         "NIC"="Nicaragua",
                                         "NER"="Niger",
                                         "NGA"="Nigeria",
                                         "NIU"="Niue",
                                         "MKD"="North Macedonia",
                                         "NOR"="Norway",
                                         "OMN"="Oman",
                                         "PAK"="Pakistan",
                                         "PSE"="Palestine",
                                         "PAN"="Panama",
                                         "PNG"="Papua New Guinea",
                                         "PRY"="Paraguay",
                                         "PER"="Peru",
                                         "PHL"="Philippines",
                                         "POL"="Poland",
                                         "POR"="Portugal",
                                         "PRI"="Puerto Rico",
                                         "QAT"="Qatar",
                                         "KOR"="Republic of Korea",
                                         "MDA"="Republic of Moldova",
                                         "ROU"="Romania",
                                         "RUS"="Russian Federation",
                                         "RWA"="Rwanda",
                                         "KNA"="Saint Kitts and Nevis",
                                         "LCA"="Saint Lucia",
                                         "VCT"="Saint Vincent and the Grenadines",
                                         "WSM"="Samoa",
                                         "STP"="Sao Tome and Principe",
                                         "SAU"="Saudi Arabia",
                                         "SEN"="Senegal",
                                         "SRB"="Serbia",
                                         "SYC"="Seychelles",
                                         "SLE"="Sierra Leone",
                                         "SGP"="Singapore",
                                         "SVK"="Slovakia",
                                         "SVN"="Slovenia",
                                         "SLB"="Solomon Islands",
                                         "SOM"="Somalia",
                                         "ZAF"="South Africa",
                                         "ESP"="Spain",
                                         "LKA"="Sri Lanka",
                                         "SDN"="Sudan (former)",
                                         "SUR"="Suriname",
                                         "SWE"="Sweden",
                                         "CHE"="Switzerland",
                                         "SYR"="Syrian Arab Republic",
                                         "TJK"="Tajikistan",
                                         "THA"="Thailand",
                                         "TLS"="Timor-Leste",
                                         "TGO"="Togo",
                                         "TKL"="Tokelau",
                                         "TKL"="Tonga",
                                         "TTO"="Trinidad and Tobago",
                                         "TUN"="Tunisia",
                                         "TUR"="Türkiye",
                                         "TKM"="Turkmenistan",
                                         "TUV"="Tuvalu",
                                         "UGA"="Uganda",
                                         "UKR" = "Ukraine",
                                         "UAE" = "United Arab Emirates",
                                         "GBR"="United Kingdom of Great Britain and Northern Ireland",
                                         "TZA" = "United Republic of Tanzania",
                                         "USA" = "United States of America",
                                         "URY" = "Uruguay",
                                         "UZB" = "Uzbekistan",
                                         "VUT" = "Vanuatu",
                                         "VEN" = "Venezuela (Bolivarian Republic of)",
                                         "VNM" = "Viet Nam",
                                         "YEM" = "Yemen",
                                         "ZMB" = "Zambia",
                                         "ZWE" = "Zimbabwe"),
                    CRP=as.factor(Item),
                    CRP=fct_recode(CRP,"car"="Carrots and turnips",
                                   "cab"="Cabbages",
                                   "bck"="Buckwheat",
                                   "brl"="Barley",
                                   "ban"="Bananas",
                                   "yam"="Yams",
                                   "wpo"="Potatoes",
                                   "whe"="Wheat",
                                   "rce"="Rice",
                                   "tom"="Tomatoes",
                                   "tob"="Unmanufactured tobacco",
                                   "tea"="Tea leaves",
                                   "spo"="Sweet potatoes",
                                   "sfl"="Sunflower seed",
                                   "suc"="Sugar cane",
                                   "sub"="Sugar beet",
                                   "soy"="Soya beans",
                                   "srg"="Sorghum",
                                   "rye"="Rye",
                                   "rub"="Natural rubber in primary forms",
                                   "rcg"="Canary seed",
                                   "rsd"="Rape or colza seed",
                                   "pig"="Pigeon peas, dry",
                                   "mlt"="Millet",
                                   "oni"="Onions and shallots, green",
                                   "olv"="Olives",
                                   "olp"="Oil palm fruit",
                                   "oat"="Oats",
                                   "grd"="Groundnuts, excluding shelled",
                                   "flx"="Flax, raw or retted",
                                   "pea"="Chick peas, dry",
                                   "cow"="Cow peas, dry",
                                   "cot"="Seed cotton, unginned",
                                   "cof"="Coffee, green",
                                   "con"="Coconuts, in shell",
                                   "coc"="Cocoa beans",
                                   "cit"="Other citrus fruit, n.e.c.",
                                   "csv"="Cassava, fresh",))
actuals2010officals<-actuals2010

bananamanipulation2010<-actuals2010[which(actuals2010$CRP=="ban"| actuals2010$CRP=="Plantains and cooking bananas" ),]
bananamanipulation2010<-bananamanipulation2010%>% group_by(Country) %>% summarise(Value = sum(Value))
bananamanipulation2010 <- as.data.frame(bananamanipulation2010)
actuals2010 <- subset(actuals2010, actuals2010$CRP!="ban" & actuals2010$CRP!="Plantains and cooking bananas" ) 
actuals2010<-actuals2010|>select(Country,CRP,Value)
bananamanipulation2010$CRP<-rep("ban",127)
actuals2010<-rbind(actuals2010,bananamanipulation2010)


#######################
##3 ~ Potentials Data##
#######################

car <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/car_CRUTS32_Hist_8110Hr_ctr.csv")
cab <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/cab_CRUTS32_Hist_8110Hr_ctr.csv")
bck <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/bck_CRUTS32_Hist_8110Hr_ctr.csv")
bsg <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/bsg_CRUTS32_Hist_8110Hr_ctr.csv")
alf <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/alf_CRUTS32_Hist_8110Hr_ctr.csv")
brl <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/brl_CRUTS32_Hist_8110Hr_ctr.csv")
ban <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/ban_CRUTS32_Hist_8110Hr_ctr.csv")
yam <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/yam_CRUTS32_Hist_8110Hr_ctr.csv")
wpo <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/wpo_CRUTS32_Hist_8110Hr_ctr.csv")
whe <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/whe_CRUTS32_Hist_8110Hr_ctr.csv")
rcw <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/rcw_CRUTS32_Hist_8110Hr_ctr.csv")
tom <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/tom_CRUTS32_Hist_8110Hr_ctr.csv")
tob <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/tob_CRUTS32_Hist_8110Hr_ctr.csv")
tea <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/tea_CRUTS32_Hist_8110Hr_ctr.csv")
swg <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/swg_CRUTS32_Hist_8110Hr_ctr.csv")
spo <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/spo_CRUTS32_Hist_8110Hr_ctr.csv")
sfl <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/sfl_CRUTS32_Hist_8110Hr_ctr.csv")
suc <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/suc_CRUTS32_Hist_8110Hr_ctr.csv")
sub <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/sub_CRUTS32_Hist_8110Hr_ctr.csv")
soy <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/soy_CRUTS32_Hist_8110Hr_ctr.csv")
srg <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/srg_CRUTS32_Hist_8110Hr_ctr.csv")
mzs <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/mzs_CRUTS32_Hist_8110Hr_ctr.csv")
rye <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/rye_CRUTS32_Hist_8110Hr_ctr.csv")
rub <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/rub_CRUTS32_Hist_8110Hr_ctr.csv")
rcg <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/rcg_CRUTS32_Hist_8110Hr_ctr.csv")
rsd <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/rsd_CRUTS32_Hist_8110Hr_ctr.csv")
pig <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/pig_CRUTS32_Hist_8110Hr_ctr.csv")
phb <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/phb_CRUTS32_Hist_8110Hr_ctr.csv")
pml <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/pml_CRUTS32_Hist_8110Hr_ctr.csv")
oni <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/oni_CRUTS32_Hist_8110Hr_ctr.csv")
olv <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/olv_CRUTS32_Hist_8110Hr_ctr.csv")
olp <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/olp_CRUTS32_Hist_8110Hr_ctr.csv")
oat <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/oat_CRUTS32_Hist_8110Hr_ctr.csv")
nap <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/nap_CRUTS32_Hist_8110Hr_ctr.csv")
mis <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/mis_CRUTS32_Hist_8110Hr_ctr.csv")
mze <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/mze_CRUTS32_Hist_8110Hr_ctr.csv")
jtr <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/jtr_CRUTS32_Hist_8110Hr_ctr.csv")
grd <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/grd_CRUTS32_Hist_8110Hr_ctr.csv")
grm <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/grm_CRUTS32_Hist_8110Hr_ctr.csv")
fml <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/fml_CRUTS32_Hist_8110Hr_ctr.csv")
flx <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/flx_CRUTS32_Hist_8110Hr_ctr.csv")
rcd <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/rcd_CRUTS32_Hist_8110Hr_ctr.csv")
pea <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/pea_CRUTS32_Hist_8110Hr_ctr.csv")
cow <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/cow_CRUTS32_Hist_8110Hr_ctr.csv")
cot <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/cot_CRUTS32_Hist_8110Hr_ctr.csv")
cof <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/cof_CRUTS32_Hist_8110Hr_ctr.csv")
con <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/con_CRUTS32_Hist_8110Hr_ctr.csv")
coc <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/coc_CRUTS32_Hist_8110Hr_ctr.csv")
cit <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/cit_CRUTS32_Hist_8110Hr_ctr.csv")
chk <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/chk_CRUTS32_Hist_8110Hr_ctr.csv")
csv <- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/csv_CRUTS32_Hist_8110Hr_ctr.csv")

cab <- cab[which(cab$LC==11 & cab$EXC==1 & cab$AEZ==35),]
bck <- bck[which(bck$LC==11 & bck$EXC==1 & bck$AEZ==35),]
bsg <- bsg[which(bsg$LC==11 & bsg$EXC==1 & bsg$AEZ==35),]
alf <- alf[which(alf$LC==11 & alf$EXC==1 & alf$AEZ==35),]
brl <- brl[which(brl$LC==11 & brl$EXC==1 & brl$AEZ==35),]
ban <- ban[which(ban$LC==11 & ban$EXC==1 & ban$AEZ==35),]
yam <- yam[which(yam$LC==11 & yam$EXC==1 & yam$AEZ==35),]
wpo <- wpo[which(wpo$LC==11 & wpo$EXC==1 & wpo$AEZ==35),]
whe <- whe[which(whe$LC==11 & whe$EXC==1 & whe$AEZ==35),]
rcw <- rcw[which(rcw$LC==11 & rcw$EXC==1 & rcw$AEZ==35),]
tom <- tom[which(tom$LC==11 & tom$EXC==1 & tom$AEZ==35),]
tob <- tob[which(tob$LC==11 & tob$EXC==1 & tob$AEZ==35),]
tea <- tea[which(tea$LC==11 & tea$EXC==1 & tea$AEZ==35),]
swg <- swg[which(swg$LC==11 & swg$EXC==1 & swg$AEZ==35),]
spo <- spo[which(spo$LC==11 & spo$EXC==1 & spo$AEZ==35),]
sfl <- sfl[which(sfl$LC==11 & sfl$EXC==1 & sfl$AEZ==35),]
suc <- suc[which(suc$LC==11 & suc$EXC==1 & suc$AEZ==35),]
sub <- sub[which(sub$LC==11 & sub$EXC==1 & sub$AEZ==35),]
soy <- soy[which(soy$LC==11 & soy$EXC==1 & soy$AEZ==35),]
srg <- srg[which(srg$LC==11 & srg$EXC==1 & srg$AEZ==35),]
mzs <- mzs[which(mzs$LC==11 & mzs$EXC==1 & mzs$AEZ==35),]
rye <- rye[which(rye$LC==11 & rye$EXC==1 & rye$AEZ==35),]
rub <- rub[which(rub$LC==11 & rub$EXC==1 & rub$AEZ==35),]
rcg <- rcg[which(rcg$LC==11 & rcg$EXC==1 & rcg$AEZ==35),]
rsd <- rsd[which(rsd$LC==11 & rsd$EXC==1 & rsd$AEZ==35),]
pig <- pig[which(pig$LC==11 & pig$EXC==1 & pig$AEZ==35),]
phb <- phb[which(phb$LC==11 & phb$EXC==1 & phb$AEZ==35),]
pml <- pml[which(pml$LC==11 & pml$EXC==1 & pml$AEZ==35),]
oni <- oni[which(oni$LC==11 & oni$EXC==1 & oni$AEZ==35),]
olv <- olv[which(olv$LC==11 & olv$EXC==1 & olv$AEZ==35),]
olp <- olp[which(olp$LC==11 & olp$EXC==1 & olp$AEZ==35),]
oat <- oat[which(oat$LC==11 & oat$EXC==1 & oat$AEZ==35),]
nap <- nap[which(nap$LC==11 & nap$EXC==1 & nap$AEZ==35),]
mis <- mis[which(mis$LC==11 & mis$EXC==1 & mis$AEZ==35),]
mze <- mze[which(mze$LC==11 & mze$EXC==1 & mze$AEZ==35),]
jtr <- jtr[which(jtr$LC==11 & jtr$EXC==1 & jtr$AEZ==35),]
grd <- grd[which(grd$LC==11 & grd$EXC==1 & grd$AEZ==35),]
grm <- grm[which(grm$LC==11 & grm$EXC==1 & grm$AEZ==35),]
fml <- fml[which(fml$LC==11 & fml$EXC==1 & fml$AEZ==35),]
flx <- flx[which(flx$LC==11 & flx$EXC==1 & flx$AEZ==35),]
rcd <- rcd[which(rcd$LC==11 & rcd$EXC==1 & rcd$AEZ==35),]
pea <- pea[which(pea$LC==11 & pea$EXC==1 & pea$AEZ==35),]
cow <- cow[which(cow$LC==11 & cow$EXC==1 & cow$AEZ==35),]
cot <- cot[which(cot$LC==11 & cot$EXC==1 & cot$AEZ==35),]
cof <- cof[which(cof$LC==11 & cof$EXC==1 & cof$AEZ==35),]
con <- con[which(con$LC==11 & con$EXC==1 & con$AEZ==35),]
coc <- coc[which(coc$LC==11 & coc$EXC==1 & coc$AEZ==35),]
cit <- cit[which(cit$LC==11 & cit$EXC==1 & cit$AEZ==35),]
chk <- chk[which(chk$LC==11 & chk$EXC==1 & chk$AEZ==35),]
csv <- csv[which(csv$LC==11 & csv$EXC==1 & csv$AEZ==35),]

binded<-rbind(car, cab, bck, bsg, alf, brl, ban, yam, wpo, whe, rcw, tom, tob, tea, swg, spo, sfl, suc, sub, soy, srg, mzs, rye, rub, rcg, rsd, pig, phb, pml, oni, olv, olp, oat, nap, mis, mze, jtr, grd, grm, fml, flx, rcd, pea, cow, cot, cof, con, coc, cit, chk, csv)
potential<-binded
potential<-potential|>select(ADM0,LC,EXC,AEZ ,CRP, P_VS.S, P_VS.S.MS,P_VS...mS,Yld,Yld.1,Yld.2)
potential<-mutate(potential, Country = ADM0)
potential<-potential|>select(Country,CRP, P_VS.S, P_VS.S.MS,P_VS...mS)

ricemanipulation<-potential[which(potential$CRP=="rcw"| potential$CRP=="rcd" ),]
ricemanipulation<-ricemanipulation%>% group_by(Country) %>% summarise(P_VS.S  = sum(P_VS.S),P_VS.S.MS=sum(P_VS.S.MS),P_VS...mS=sum(P_VS...mS))
ricemanipulation <- as.data.frame(ricemanipulation)
potential <- subset(potential, potential$CRP!="rcw" & potential$CRP!="rcd")
ricemanipulation$CRP<-rep("rce",189)
potential<-rbind(potential,ricemanipulation)

milletmanipulation<-potential[which(potential$CRP=="pml"| potential$CRP=="fml" ),]
milletmanipulation<-milletmanipulation%>% group_by(Country) %>% summarise(P_VS.S  = sum(P_VS.S),P_VS.S.MS=sum(P_VS.S.MS),P_VS...mS=sum(P_VS...mS))
milletmanipulation <- as.data.frame(milletmanipulation)
potential <- subset(potential, potential$CRP!="pml" & potential$CRP!="fml")
milletmanipulation$CRP<-rep("mlt",189)
potential<-rbind(potential,milletmanipulation)

#########################
##3 ~ Novel Data Models##
#########################

data2000<-inner_join(potential, actuals2000)
data2010<-inner_join(potential, actuals2010)

data2000$Value[data2000$Value == 0]<-NA
data2010$Value[data2010$Value == 0]<-NA

data2000all<-data2000
data2010all<-data2010
data2000mid<-data2000
data2010mid<-data2010
data2000hih<-data2000
data2010hih<-data2010

data2000all$P_VS...mS[data2000all$P_VS...mS ==0]<-NA
data2010all$P_VS...mS[data2010all$P_VS...mS ==0]<-NA
data2000mid$P_VS.S.MS[data2000mid$P_VS.S.MS ==0]<-NA
data2010mid$P_VS.S.MS[data2010mid$P_VS.S.MS ==0]<-NA
data2000hih$P_VS.S[data2000hih$P_VS.S ==0]<-NA
data2010hih$P_VS.S[data2010hih$P_VS.S ==0]<-NA

data2000all<-mutate(data2000all,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2010all<-mutate(data2010all,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2000all<-mutate(data2000all,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2010all<-mutate(data2010all,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2000all<-data2000all|>select(Country, CRP, lnValue,lnPlow)
data2010all<-data2010all|>select(Country, CRP, lnValue, lnPlow)
data2000mid<-mutate(data2000mid,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2010mid<-mutate(data2010mid,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2000mid<-mutate(data2000mid,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2010mid<-mutate(data2010mid,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2000mid<-data2000mid|>select(Country, CRP, lnValue,lnPmid)
data2010mid<-data2010mid|>select(Country, CRP, lnValue, lnPmid)
data2000hih<-mutate(data2000hih,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2010hih<-mutate(data2010hih,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2000hih<-mutate(data2000hih,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2010hih<-mutate(data2010hih,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2000hih<-data2000hih|>select(Country, CRP, lnValue,lnPhigh)
data2010hih<-data2010hih|>select(Country, CRP, lnValue, lnPhigh)

simple2000all<-lm(lnValue~lnPlow, data2000all)
simple2010all<-lm(lnValue~lnPlow, data2010all)
feCountry2000all<-plm(lnValue~ lnPlow, data2000all, index= c("Country"), model="within")
feCRP2000all<-plm(lnValue~ lnPlow, data2000all, index= c("CRP"), model="within")
feCountry2010all<-plm(lnValue~ lnPlow, data2010all, index= c("Country"), model="within")
feCRP2010all<-plm(lnValue~ lnPlow, data2010all, index= c("CRP"), model="within")

simple2000mid<-lm(lnValue~lnPmid, data2000mid)
simple2010mid<-lm(lnValue~lnPmid, data2010mid)
feCountry2000mid<-plm(lnValue~ lnPmid, data2000mid, index= c("Country"), model="within")
feCRP2000mid<-plm(lnValue~ lnPmid, data2000mid, index= c("CRP"), model="within")
feCountry2010mid<-plm(lnValue~ lnPmid, data2010mid, index= c("Country"), model="within")
feCRP2010mid<-plm(lnValue~ lnPmid, data2010mid, index= c("CRP"), model="within")

simple2000hih<-lm(lnValue~lnPhigh, data2000hih)
simple2010hih<-lm(lnValue~lnPhigh, data2010hih)
feCountry2000hih<-plm(lnValue~ lnPhigh, data2000hih, index= c("Country"), model="within")
feCRP2000hih<-plm(lnValue~ lnPhigh, data2000hih, index= c("CRP"), model="within")
feCountry2010hih<-plm(lnValue~ lnPhigh, data2010hih, index= c("Country"), model="within")
feCRP2010hih<-plm(lnValue~ lnPhigh, data2010hih, index= c("CRP"), model="within")

m1a<-simple2000all
m2a<-simple2010all
m3a<-feCountry2000all
m4a<-feCRP2000all
m5a<-feCountry2010all
m6a<-feCRP2010all

m1m<-simple2000mid
m2m<-simple2010mid
m3m<-feCountry2000mid
m4m<-feCRP2000mid
m5m<-feCountry2010mid
m6m<-feCRP2010mid

m1h<-simple2000hih
m2h<-simple2010hih
m3h<-feCountry2000hih
m4h<-feCRP2000hih
m5h<-feCountry2010hih
m6h<-feCRP2010hih

stargazer(m1a,m2a,m3a,m4a,m5a,m6a,type="text")
stargazer(m1m,m2m,m3m,m4m,m5m,m6m,type="text")
stargazer(m1h,m2h,m3h,m4h,m5h,m6h,type="text")

##############################
##3 ~ Novel Data Correlation##
##############################

data2000$Value[data2000$Value == 0]<-NA
data2010$Value[data2010$Value == 0]<-NA
data2000$P_VS...mS[data2000$P_VS...mS == 0]<-NA
data2010$P_VS...mS[data2010$P_VS...mS == 0]<-NA
by(data2000, data2000$CRP,function(data2000) cor.test(data2000$Value, data2000$P_VS...mS, use="pair", method="kendall"))
by(data2000, data2000$CRP,function(data2000) cor(data2000$Value, data2000$P_VS...mS, use="pair", method="pearson"))

#################################################
##4 ~ Novel Data only official estimates Models##
#################################################

actuals2000officals
actuals2010officals
actuals2000officals|>distinct(Flag.Description)
actuals2010officals|>distinct(Flag.Description)
actuals2000officals<-actuals2000officals[which(actuals2000officals$Flag.Description=="Official figure" | actuals2000officals$Flag.Description=="Figure from international organizations"),]
actuals2010officals<-actuals2010officals[which(actuals2010officals$Flag.Description=="Official figure" | actuals2010officals$Flag.Description=="Figure from international organizations"),]

bananamanipulation2000officals<-actuals2000officals[which(actuals2000officals$CRP=="ban"| actuals2000officals$CRP=="Plantains and cooking bananas" ),]
bananamanipulation2000officals<-bananamanipulation2000officals%>% group_by(Country) %>% summarise(Value = sum(Value))
bananamanipulation2000officals <- as.data.frame(bananamanipulation2000officals)
actuals2000officals <- subset(actuals2000officals, actuals2000officals$CRP!="Plantains and cooking bananas" & actuals2000officals$CRP!="Plantains and cooking bananas" ) 
actuals2000officals<-actuals2000officals|>select(Country,CRP,Value)
bananamanipulation2000officals
bananamanipulation2000officals$CRP<-rep("ban",69)
actuals2000officals<-rbind(actuals2000officals,bananamanipulation2000officals)

bananamanipulation2010officals<-actuals2010officals[which(actuals2010officals$CRP=="ban"| actuals2010officals$CRP=="Plantains and cooking bananas" ),]
bananamanipulation2010officals<-bananamanipulation2010officals%>% group_by(Country) %>% summarise(Value = sum(Value))
bananamanipulation2010officals <- as.data.frame(bananamanipulation2010officals)
actuals2010officals <- subset(actuals2010officals, actuals2010officals$CRP!="ban" & actuals2010officals$CRP!="Plantains and cooking bananas" ) 
actuals2010officals<-actuals2010officals|>select(Country,CRP,Value)
bananamanipulation2010officals
bananamanipulation2010officals$CRP<-rep("ban",77)
actuals2010officals<-rbind(actuals2010officals,bananamanipulation2010officals)

data2000officals<-inner_join(potential, actuals2000officals)
data2010officals<-inner_join(potential, actuals2010officals)
data2000officals$Value[data2000officals$Value == 0]<-NA
data2010officals$Value[data2010officals$Value == 0]<-NA

data2000allofficals<-data2000officals
data2010allofficals<-data2010officals
data2000midofficals<-data2000officals
data2010midofficals<-data2010officals
data2000hihofficals<-data2000officals
data2010hihofficals<-data2010officals

data2000allofficals$P_VS...mS[data2000allofficals$P_VS...mS ==0]<-NA
data2010allofficals$P_VS...mS[data2010allofficals$P_VS...mS ==0]<-NA
data2000midofficals$P_VS.S.MS[data2000midofficals$P_VS.S.MS ==0]<-NA
data2010midofficals$P_VS.S.MS[data2010midofficals$P_VS.S.MS ==0]<-NA
data2000hihofficals$P_VS.S[data2000hihofficals$P_VS.S ==0]<-NA
data2010hihofficals$P_VS.S[data2010hihofficals$P_VS.S ==0]<-NA

data2000allofficals<-mutate(data2000allofficals,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2010allofficals<-mutate(data2010allofficals,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2000allofficals<-mutate(data2000allofficals,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2010allofficals<-mutate(data2010allofficals,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2000allofficals<-data2000allofficals|>select(Country, CRP, lnValue,lnPlow)
data2010allofficals<-data2010allofficals|>select(Country, CRP, lnValue, lnPlow)
data2000midofficals<-mutate(data2000midofficals,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2010midofficals<-mutate(data2010midofficals,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2000midofficals<-mutate(data2000midofficals,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2010midofficals<-mutate(data2010midofficals,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2000midofficals<-data2000midofficals|>select(Country, CRP, lnValue,lnPmid)
data2010midofficals<-data2010midofficals|>select(Country, CRP, lnValue, lnPmid)
data2000hihofficals<-mutate(data2000hihofficals,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2010hihofficals<-mutate(data2010hihofficals,Phigh= 1000*P_VS.S,Pmid=1000*P_VS.S.MS,Plow=1000*P_VS...mS)
data2000hihofficals<-mutate(data2000hihofficals,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2010hihofficals<-mutate(data2010hihofficals,lnPhigh= log(Phigh),lnPmid=log(Pmid),lnPlow=log(Plow), lnValue=log(Value))
data2000hihofficals<-data2000hihofficals|>select(Country, CRP, lnValue,lnPhigh)
data2010hihofficals<-data2010hihofficals|>select(Country, CRP, lnValue, lnPhigh)

simple2000allofficals<-lm(lnValue~lnPlow, data2000allofficals)
simple2010allofficals<-lm(lnValue~lnPlow, data2010allofficals)
feCountry2000allofficals<-plm(lnValue~ lnPlow, data2000allofficals, index= c("Country"), model="within")
feCRP2000allofficals<-plm(lnValue~ lnPlow, data2000allofficals, index= c("CRP"), model="within")
feCountry2010allofficals<-plm(lnValue~ lnPlow, data2010allofficals, index= c("Country"), model="within")
feCRP2010allofficals<-plm(lnValue~ lnPlow, data2010allofficals, index= c("CRP"), model="within")

simple2000midofficals<-lm(lnValue~lnPmid, data2000midofficals)
simple2010midofficals<-lm(lnValue~lnPmid, data2010midofficals)
feCountry2000midofficals<-plm(lnValue~ lnPmid, data2000midofficals, index= c("Country"), model="within")
feCRP2000midofficals<-plm(lnValue~ lnPmid, data2000midofficals, index= c("CRP"), model="within")
feCountry2010midofficals<-plm(lnValue~ lnPmid, data2010midofficals, index= c("Country"), model="within")
feCRP2010midofficals<-plm(lnValue~ lnPmid, data2010midofficals, index= c("CRP"), model="within")

simple2000hihofficals<-lm(lnValue~lnPhigh, data2000hihofficals)
simple2010hihofficals<-lm(lnValue~lnPhigh, data2010hihofficals)
feCountry2000hihofficals<-plm(lnValue~ lnPhigh, data2000hihofficals, index= c("Country"), model="within")
feCRP2000hihofficals<-plm(lnValue~ lnPhigh, data2000hihofficals, index= c("CRP"), model="within")
feCountry2010hihofficals<-plm(lnValue~ lnPhigh, data2010hihofficals, index= c("Country"), model="within")
feCRP2010hihofficals<-plm(lnValue~ lnPhigh, data2010hihofficals, index= c("CRP"), model="within")

m1aof<-simple2000allofficals
m2aof<-simple2010allofficals
m3aof<-feCountry2000allofficals
m4aof<-feCRP2000allofficals
m5aof<-feCountry2010allofficals
m6aof<-feCRP2010allofficals

m1mof<-simple2000midofficals
m2mof<-simple2010midofficals
m3mof<-feCountry2000midofficals
m4mof<-feCRP2000midofficals
m5mof<-feCountry2010midofficals
m6mof<-feCRP2010midofficals

m1hof<-simple2000hihofficals
m2hof<-simple2010hihofficals
m3hof<-feCountry2000hihofficals
m4hof<-feCRP2000hihofficals
m5hof<-feCountry2010hihofficals
m6hof<-feCRP2010hihofficals

stargazer(m1aof,m2aof,m3aof,m4aof,m5aof,m6aof,type="text")
stargazer(m1mof,m2mof,m3mof,m4mof,m5mof,m6mof,type="text")
stargazer(m1hof,m2hof,m3hof,m4hof,m5hof,m6hof,type="text")

#######################################################
##4 ~ Novel Data only official estimates Correlations##
#######################################################

data2000officals$Value[data2000officals$Value == 0]<-NA
data2010officals$Value[data2010officals$Value == 0]<-NA
data2000officals$P_VS...mS[data2000officals$P_VS...mS == 0]<-NA
data2010officals$P_VS...mS[data2010officals$P_VS...mS == 0]<-NA
by(data2000officals, data2000officals$CRP,function(data2000officals) cor.test(data2000officals$Value, data2000officals$P_VS...mS, use="pair", method="kendall"))
by(data2000officals, data2000officals$CRP,function(data2000officals) cor(data2000officals$Value, data2000officals$P_VS...mS, use="pair", method="pearson"))







