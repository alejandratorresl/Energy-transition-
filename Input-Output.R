#### Input-Output tables + Environmental accounts

#This script builds the consolidated input-output table for 18 sectors using data from WIOD.
#First, it uses environmental accounts to get:
# 1. Sectoral energy intensity by type of energy
# 2. Intermediate consumption shares by type of energy
# 3. Final demand shares by type of energy
# 4. Sales shares by type of energy

# This information is used to break the energy sector from the IO table and create four
# energy subsectors: oil, coal, natural gas and green. 

#The final output is the table of inter-sectoral shares used to calibrate the model.

################################################################################

#Clear
rm(list = ls(all.names = TRUE)) 
gc() 

#Working directory
setwd("")

##Packages
library(tidyverse)
library(readxl)
library(stringr)
library(xlsx)



##Upload data

IO_raw <-read_excel("MEX_NIOT_nov16.xlsx", sheet="National IO-tables")
#Delete rows with no observations for the matrix
IO_raw<-IO_raw %>% drop_na(Year)  %>% filter(Year==2014)

#To get to the 15 activities, we separate them from value added rows
IO_1<-IO_raw%>% filter(Origin=="Domestic" |Origin=="Imports")

#Create a variable that contains only the first letter of the ISIC code. This is the division
IO_1<-IO_1%>% mutate(Code1 = substr(IO_1$Code, 1,1)) %>% 
  mutate_at(c(5:67), as.numeric) #Matrix is in string format. Transform to numeric values

#Put some activities together
IO_1<-IO_1 %>% mutate(Code1=replace(Code1, Code=="D35", "B")) %>% #Put Electricity, gas, steam and air conditioning supply in division B
  mutate(Code1=replace(Code1, Code1=="H", "HJ"))%>% #Transportation and storage + Information and communication
  mutate(Code1=replace(Code1, Code1=="J", "HJ")) %>% 
  mutate(Code1=replace(Code1, Code1=="L", "LMN")) %>% #Real estate and business services
  mutate(Code1=replace(Code1, Code1=="M", "LMN")) %>% 
  mutate(Code1=replace(Code1, Code1=="N", "LMN")) %>% 
  mutate(Code1=replace(Code1, Code1=="R", "RSU")) %>% #Arts, entertainment, recreation, other
  mutate(Code1=replace(Code1, Code1=="U", "RSU"))

#Aggregate rows by division
IO_1 <- IO_1 %>% group_by(Code1) %>%   summarise_if(is.numeric, sum, na.rm = TRUE)

#Go back to raw data and get value added rows to paste them to the rest of the data
IO_2<-IO_raw %>% filter(Year==2014)%>% filter(Origin=="TOT") %>%
  mutate_at(c(5:67), as.numeric) 
#Rename code variable to avoid missing values
IO_2 <-rename(IO_2,Code1=Code)
IO_2 <- IO_2 %>% select(-Description, -Origin)
#Append the two parts of the matrix
IO_div <-union_all(IO_1,IO_2)

#From now on, we will work only with numbers so strings are innecessary, we can drop them
IO_div<-IO_div%>%select(-Year, -Code1)

#Create columns with aggregated activities by division
IO_div1<-IO_div  %>% mutate(A = IO_div %>% select(starts_with("A")) %>% rowSums())%>%
  mutate(B=B+D35)%>% #Electricity, gas, steam and air conditioning supply into mining and quarrying: Energy
  mutate(C = IO_div %>% select(starts_with("C") & -starts_with("CONS")) %>% rowSums())%>%
  mutate(E = IO_div %>% select(starts_with("E") & -"EXP") %>% rowSums())%>%
  mutate(G = IO_div %>% select(starts_with("G") & -"GO" & -"GFCF") %>% rowSums())%>%
  mutate(HJ = IO_div %>% select(starts_with("H") |starts_with("J") ) %>% rowSums())%>%
  mutate(K = IO_div %>% select(starts_with("K")) %>% rowSums())%>%
  mutate(LMN = IO_div %>% select(starts_with("L") |starts_with("M") | starts_with("N")) %>% rowSums())%>%
  mutate(O = IO_div %>% select(starts_with("O")) %>% rowSums())%>%
  mutate(P = IO_div %>% select(starts_with("P")) %>% rowSums())%>%
  mutate(RSU = IO_div %>% select(starts_with("R")| starts_with("S") |starts_with("U")) %>% rowSums())

###We need this for environmental accounts
sector_D = IO_div1$D35[23]+IO_div1$E[23]
electricity = IO_div1$D35[23]/sector_D
water = 1-electricity

#Drop innecessary variables
IO2<-IO_div1 %>% select(A, B, C, E, F, G, HJ, I, K ,LMN, O, P, Q, RSU, T, CONS_h, CONS_np, CONS_g, GFCF, INVEN, EXP, GO)

#Vector containing row names
Code1<-c("A", "B", "C", "E", "F", "G", "HJ","I", "K", "LMN", "O", "P","Q","RSU", "T",
         "II_fob", "TXSP", "EXP_adj", "PURR", "PURNR", "VA", "IntTTM", "GO")

#Name rows and columns in the matrix
IO2_mat<-as.matrix(IO2, labels=true)
rownames(IO2_mat)<-Code1


################################################################################
#Now we turn to the environmental accounts table

#Read table
Env_MEX<-read_excel("MEX_EM_May12.xlsx", sheet="2009")

#Drop missing values and non-numeric variables
Env_MEX <- Env_MEX %>% drop_na(TOTAL) %>% select(-...1, -...2)

#Separate Electricity and water supply. We use the weights of each subsector from the IO table
Electricity <-  Env_MEX[17,1:28]*electricity
Water_supply <-Env_MEX[17,1:28]*water
T <- rep(0,28)

Env_MEX <-rbind(Env_MEX, Electricity, Water_supply, T)

#Assign the division to each economic activity
index <- c("A", "B", rep("C", times = 14), "D", "F", rep("G", 3), "I",
           rep("HJ", 5), "K", rep("LMN",2), "O","P", "Q", "RSU",  "TOT", "FC_HH", "GO", "B", "E", "T")

Env_MEX$index <-index

#Aggregate rows by division
Env_MEX_ag <- Env_MEX %>% group_by(index) %>%   summarise_if(is.numeric, sum, na.rm = TRUE)

#Get four types of energy: oil, coal, natural gas and green
Env_MEX_ag <- Env_MEX_ag %>% mutate(OIL = CRUDE + DIESEL + GASOLINE + JETFUEL + LFO + HFO + NAPHTA + OTHPETRO + WASTE)%>% 
  mutate(COAL = HCOAL + BCOAL + COKE) %>%
  mutate(NATGAS = NATGAS + OTHGAS) %>%
  mutate(GREEN = BIOGASOL +	BIODIESEL +	BIOGAS + OTHRENEW +	ELECTR + HEATPROD	+ NUCLEAR	+ HYDRO	+ GEOTHERM + SOLAR + WIND +	OTHSOURC + LOSS) %>%
  select(index, OIL, COAL, NATGAS, GREEN, TOTAL)

#Reorder rows
Env_MEX_ag <- Env_MEX_ag[c(1,3:6,8,10:18,2,19,7,9),]

#Intermediate consumption shares
ICS_oil <- as.numeric(Env_MEX_ag[17,2]/Env_MEX_ag[17,6])
ICS_coal <- as.numeric(Env_MEX_ag[17,3]/Env_MEX_ag[17,6])
ICS_natgas <- as.numeric(Env_MEX_ag[17,4]/Env_MEX_ag[17,6])
ICS_green <- as.numeric(Env_MEX_ag[17,5]/Env_MEX_ag[17,6])

ICS<-c(ICS_oil,ICS_coal, ICS_natgas, ICS_green)

#Final consumption shares
FCS_oil <- as.array(rep(as.numeric(Env_MEX_ag[18,2]/Env_MEX_ag[18,6]),7))
FCS_coal <- rep(as.numeric(Env_MEX_ag[18,3]/Env_MEX_ag[18,6]),7)
FCS_natgas <- rep(as.numeric(Env_MEX_ag[18,4]/Env_MEX_ag[18,6]),7)
FCS_green <- rep(as.numeric(Env_MEX_ag[18,5]/Env_MEX_ag[18,6]),7)

FCS<-c(FCS_oil,FCS_coal, FCS_natgas, FCS_green)
FCS<-rep(FCS,7)
#Sales shares
SS_oil <- as.numeric(Env_MEX_ag[19,2]/Env_MEX_ag[19,6])
SS_coal <- as.numeric(Env_MEX_ag[19,3]/Env_MEX_ag[19,6])
SS_natgas <- as.numeric(Env_MEX_ag[19,4]/Env_MEX_ag[19,6])
SS_green <- as.numeric(Env_MEX_ag[19,5]/Env_MEX_ag[19,6])

SS<-c(SS_oil,SS_coal, SS_natgas, SS_green)

#Add rows with each energy type
Oil <-  as.numeric(c("Oil",Env_MEX_ag[16,2:6]*SS_oil))
Coal <-  as.numeric(c("Coal",Env_MEX_ag[16,2:6]*SS_coal))
NatGas <-  as.numeric(c("NatGas",Env_MEX_ag[16,2:6]*SS_natgas))
Green <-  as.numeric(c("Green",Env_MEX_ag[16,2:6]*SS_green))

Energy<-as.data.frame(rbind(t(Oil), t(Coal), t(NatGas), t(Green)))

colnames(Energy) <- c("index", "OIL", "COAL", "NATGAS", "GREEN", "TOTAL")
Env_MEX_final<-rbind(Env_MEX_ag, Energy)

Env_MEX_shares<-Env_MEX_final %>% mutate(oil = OIL/TOTAL)  %>% mutate(coal = COAL/TOTAL)  %>%
  mutate(natgas = NATGAS/TOTAL)  %>% mutate(green = GREEN/TOTAL) %>%select(index, oil, coal, natgas, green)

Env_MEX_shares<-Env_MEX_shares %>% filter((index!="D" & index!="B" & index!="TOT" & index!="FC_HH" & index!="GO") | is.na(index))

FCS_0<-c(as.numeric(Env_MEX_ag[18,2]/Env_MEX_ag[18,6]),as.numeric(Env_MEX_ag[18,3]/Env_MEX_ag[18,6]),as.numeric(Env_MEX_ag[18,4]/Env_MEX_ag[18,6]),as.numeric(Env_MEX_ag[18,5]/Env_MEX_ag[18,6]))


shares_breakdown<-rbind(ICS, FCS_0, SS)


write.xlsx(Env_MEX_shares, file="IO_mexico.xlsx", sheetName ="Env_shares")
write.xlsx(shares_breakdown, file="IO_mexico.xlsx", sheetName ="Shares_Breakdown", append=TRUE)



################################################################################
#Back to IO tables

#Create energy inputs
oil<-IO2$B*ICS_oil 
coal<-IO2$B*ICS_coal
natgas<-IO2$B*ICS_natgas
green<-IO2$B*ICS_green

oil[23]<-IO2$B[23]*SS_oil
coal[23]<-IO2$B[23]*SS_coal
natgas[23]<-IO2$B[23]*SS_natgas
green[23]<-IO2$B[23]*SS_green


oil[21]<-oil[23]-oil[16]
coal[21]<-coal[23]-coal[16]
natgas[21]<-natgas[23]-natgas[16]
green[21]<-green[23]-green[16]

IO2_en<-cbind(IO2_mat, oil, coal, natgas, green)
IO2_en<-IO2_en[,-2] #Drop total energy column

#Reorder columns and rows
IO2_en<-IO2_en[, c(1:14,22:25, 15:21)]


##Rows

#Rows with energy inputs

mix_oil<-rbind(as.matrix(Env_MEX_shares[,2]), as.matrix(FCS_oil))
mix_coal<-rbind(as.matrix(Env_MEX_shares[,3]), as.matrix(FCS_coal))
mix_natgas<-rbind(as.matrix(Env_MEX_shares[,4]), as.matrix(FCS_natgas))
mix_green<-rbind(as.matrix(Env_MEX_shares[,5]), as.matrix(FCS_green))


oil_r<-IO2_en[2,]*mix_oil
coal_r<-IO2_en[2,]*mix_coal
natgas_r<-IO2_en[2,]*mix_natgas
green_r<-IO2_en[2,]*mix_green

#Matrix with energy inputs on columns and rows
IO2_en<-rbind(IO2_en, t(oil_r), t(coal_r), t(natgas_r), t(green_r))
#Drop total energy row
IO2_en<-IO2_en[-2,]

#Reorder columns and rows
IO2_en<-IO2_en[c(1:14,23:26, 15:22),]

#Expenditure shares
expenditure<-IO2_en[1:18,19:23]
exp_tot<-sum(expenditure) #Total expenditure
exp_tot_each<-rowSums(expenditure) #Expenditure by activity
exp_shares<-exp_tot_each/exp_tot #Expenditure shares

#Matrix with only activities
IO3<-IO2_en[1:18,1:18]
#Sales
GO<-IO2_en[26,1:18]
GO_m<-t(replicate(18, GO))
#Beta M
beta_transposed<-(IO3/GO_m)
#Intermediates
il_fob<-IO2_en[19,1:18]
#Value added: Output-Total intermediates
VA<-GO-il_fob

#sales, va and expenditure
vars<-rbind(GO, VA, exp_shares)

IO2_en_df<-as.data.frame(IO2_en)

write.xlsx(IO2_en_df, file="IO_mexico.xlsx", sheetName ="IO2", append=TRUE)
write.xlsx(beta_transposed, file="IO_mexico.xlsx", sheetName="BetaM", append=TRUE)
write.xlsx(vars, file="IO_mexico.xlsx", sheetName="vars", append=TRUE)


###############################################################################
# Non-Energy emissions

#Read table
CO2_MEX <-read_excel("MEX_CO2_May12.xlsx", sheet="2009")

#Drop missing values and non-numeric variables
CO2_MEX <- CO2_MEX %>% drop_na(TOTAL) %>% select(-...1, -...2)


#Separate Electricity and water supply. We use the weights of each subsector from the IO table
Electricity <-  CO2_MEX[17,1:29]*electricity
Water_supply <-CO2_MEX[17,1:29]*water
T <- rep(0,28)


CO2_MEX <-rbind(CO2_MEX, Electricity, Water_supply, T)

#Assign the division to each economic activity
index <- c("A", "B", rep("C", times = 14), "D", "F", rep("G", 3), "I",
           rep("HJ", 5), "K", rep("LMN",2), "O","P", "Q", "RSU",  "TOT", "FC_HH", "GO", "B", "E", "T")

CO2_MEX$index <-index

#Aggregate rows by division
CO2_MEX <- CO2_MEX %>% group_by(index) %>%   summarise_if(is.numeric, sum, na.rm = TRUE)

#Get four types of energy: oil, coal, natural gas and green
CO2_MEX_ag <- CO2_MEX %>% mutate(OIL = CRUDE + DIESEL + GASOLINE + JETFUEL + LFO + HFO + NAPHTA + OTHPETRO + WASTE)%>% 
  mutate(COAL = HCOAL + BCOAL + COKE) %>%
  mutate(NATGAS = NATGAS + OTHGAS) %>%
  mutate(GREEN = BIOGASOL +	BIODIESEL +	BIOGAS + OTHRENEW +	ELECTR + HEATPROD	+ NUCLEAR	+ HYDRO	+ GEOTHERM + SOLAR + WIND +	OTHSOURC) %>%
  mutate(NON_ENERGY = NonENERGY) %>%
  select(index, OIL, COAL, NATGAS, GREEN, NON_ENERGY, TOTAL)

#Reorder rows
CO2_MEX_ag <- CO2_MEX_ag[c(1,3:6,8,10:18,2,19,7,9),]



#Add rows with each energy type
Oil <-  as.numeric(c("Oil",CO2_MEX_ag[16,2:7]*SS_oil))
Coal <-  as.numeric(c("Coal",CO2_MEX_ag[16,2:7]*SS_coal))
NatGas <-  as.numeric(c("NatGas",CO2_MEX_ag[16,2:7]*SS_natgas))
Green <-  as.numeric(c("Green",CO2_MEX_ag[16,2:7]*SS_green))

Energy<-as.data.frame(rbind(t(Oil), t(Coal), t(NatGas), t(Green)))

colnames(Energy) <- c("index", "OIL", "COAL", "NATGAS", "GREEN", "NON_ENERGY", "TOTAL")
CO2_MEX_final<-rbind(CO2_MEX_ag, Energy)

CO2_MEX_final <- CO2_MEX_final[c(1:16,20:23,17:19),]


non_energy <- CO2_MEX_final[1:20,6]


write.xlsx(CO2_MEX_final, file="IO_mexico.xlsx", sheetName ="CO2", append=TRUE)
write.xlsx(non_energy, file="IO_mexico.xlsx", sheetName ="Non_Energy_Emissions", append=TRUE)


############################################################################

#Socio-economic data


##Upload data
sea <-read_excel("Socio_Economic_Accounts.xlsx", sheet="DATA")

#Filter data
sea <- sea %>% filter(country=="MEX" & (variable=="EMPE" |  variable=="LAB")) %>% select("variable", "code", "2014")

#Create a variable that contains only the first letter of the ISIC code. This is the division
sea<-sea%>% mutate(Code1 = substr(sea$code, 1,1)) 

#Put some activities together
sea<-sea %>% mutate(Code1=replace(Code1, Code1=="D", "B")) %>% #Put Electricity, gas, steam and air conditioning supply in division B
  mutate(Code1=replace(Code1, Code1=="H", "HJ"))%>% #Transportation and storage + Information and communication
  mutate(Code1=replace(Code1, Code1=="J", "HJ")) %>% 
  mutate(Code1=replace(Code1, Code1=="L", "LMN")) %>% #Real estate and business services
  mutate(Code1=replace(Code1, Code1=="M", "LMN")) %>% 
  mutate(Code1=replace(Code1, Code1=="N", "LMN")) %>% 
  mutate(Code1=replace(Code1, Code1=="R", "RSU")) %>% #Arts, entertainment, recreation, other
  mutate(Code1=replace(Code1, Code1=="U", "RSU"))

#Aggregate rows by division
sea1 <- sea %>% group_by(Code1, variable) %>%   summarise_if(is.numeric, sum, na.rm = TRUE)

sea1 <- sea1 %>% spread(variable, "2014")

#Add rows with each energy type
Oil <-  c("Oil", as.numeric(c(sea1[2, 2:3]*SS_oil)))
Coal <-  c("Gas", as.numeric(c(sea1[2, 2:3]*SS_coal)))
NatGas <-  c("NatGas", as.numeric(c(sea1[2, 2:3]*SS_natgas)))
Green <-  c("Green", as.numeric(c(sea1[2, 2:3]*SS_green)))


Energy<-as.data.frame(rbind(t(Oil), t(Coal), t(NatGas), t(Green)))
colnames(Energy) <- c("Code1", "EMPE", "LAB")

sea1<-rbind(as.matrix(sea1), Energy)
sea1 <- sea1 %>% filter(Code1!="B")

sea1 <- sea1 %>% mutate(Av_wage = as.numeric(LAB)/as.numeric(EMPE)) 

#Labor shares
sea1 <- sea1 %>% mutate(sum = sum(as.numeric(EMPE))) %>% mutate(lab_share = as.numeric(EMPE)/sum)

write.xlsx(sea1, file="IO_mexico.xlsx", sheetName ="Labor_shares", append=TRUE)

