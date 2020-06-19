rm(list = ls())

all_cleaned_dt <- read.csv("data_b.csv")






### Selecting predictive variables
names(all_cleaned_dt)
all_cleaned_dt$X.1 <- NULL
all_cleaned_dt$PRODUCT_CATEGORY <- NULL
all_cleaned_dt$ORIGINAL_PLANT <- NULL
all_cleaned_dt$REGIONAL_SALES_MGR_ID <- NULL

str(all_cleaned_dt)

all_cleaned_dt$X <- as.factor(all_cleaned_dt$X)
all_cleaned_dt$Month <- as.factor(all_cleaned_dt$Month)


### plant B analysis
all_cleaned_dt_b <- as.data.table(all_cleaned_dt)
nrow(all_cleaned_dt_b)
str(all_cleaned_dt_b)

#train <- sample.split(all_cleaned_dt_b$X, SplitRatio = 0.6)
#all_cleaned_dt_b.test <- all_cleaned_dt_b[!train, ]
# Run regression on train data set individually to check for significancy

glm.train <- glm(X ~ COLOR_ID , data = all_cleaned_dt_b, subset = train, family = binomial)

glm.train <- glm(X ~ COLOR_ID , data = all_cleaned_dt_b, family = binomial)

summary(glm.train)

options(max.print=10000)

library(data.table)
## ALLIANCE_LEVEL_ID
table(all_cleaned_dt_b.test$ALLIANCE_LEVEL_ID)
aggregation <- all_cleaned_dt_b[,list(row_count = .N, sales = sum(NET_SALES_UNITS)), by = ALLIANCE_LEVEL_ID]

alliance_others <- as.character(aggregation[aggregation$sales < 100,]$ALLIANCE_LEVEL_ID) 

length(unique(all_cleaned_dt_b$ALLIANCE_LEVEL_ID))
all_cleaned_dt_b$ALLIANCE_LEVEL_ID <- as.character(all_cleaned_dt_b$ALLIANCE_LEVEL_ID)
all_cleaned_dt_b[all_cleaned_dt_b$ALLIANCE_LEVEL_IDD %in% alliance_others,]$ALLIANCE_LEVEL_ID <- 'OTHER'
all_cleaned_dt_b$ALLIANCE_LEVEL_ID <- as.factor(all_cleaned_dt_b$ALLIANCE_LEVEL_ID)
length(unique(all_cleaned_dt_b$ALLIANCE_LEVEL_ID))

all_lev_req <- c("BR","CL", "EM","PR","RB") 
all_cleaned_dt_b[!(all_cleaned_dt_b$ALLIANCE_LEVEL_ID %in% all_lev_req), ]$ALLIANCE_LEVEL_ID<- 'OTHER'




## "REGION_STATE_ID"
table(all_cleaned_dt_b$REGION_STATE_ID)
aggregation <- all_cleaned_dt_b[,list(row_count = .N, sales = sum(NET_SALES_UNITS)), by = REGION_STATE_ID]

others <- as.character(aggregation[aggregation$sales < 100,]$REGION_STATE_ID) 

length(unique(all_cleaned_dt_b$REGION_STATE_ID))
all_cleaned_dt_b$REGION_STATE_ID <- as.character(all_cleaned_dt_b$REGION_STATE_ID)
all_cleaned_dt_b[all_cleaned_dt_b$REGION_STATE_ID %in% others,]$REGION_STATE_ID <- 'OTHER'
region_state_req <- c('AB','AZ', 'CO', 'GA', 'IL', 'KY', 'ME', 'MI', 'MT', 'NC', 'NH', 'NS', 'NY', 'ON', 'QC', 'SD', 'TN', 'TX', 'UT', 'WI', 'WV', 'WY')
all_cleaned_dt_b[!(all_cleaned_dt_b$REGION_STATE_ID %in% region_state_req),]$REGION_STATE_ID <- 'OTHER'
all_cleaned_dt_b$REGION_STATE_ID <- as.factor(all_cleaned_dt_b$REGION_STATE_ID)
length(unique(all_cleaned_dt_b$REGION_STATE_ID))



#$ "FABRIC_ID"
table(all_cleaned_dt_b$FABRIC_ID)
aggregation <- all_cleaned_dt_b[,list(row_count = .N, sales = sum(NET_SALES_UNITS)), by = FABRIC_ID]

fabric_others <- as.character(aggregation[aggregation$sales < 500,]$FABRIC_ID) 

length(unique(all_cleaned_dt_b$FABRIC_ID))
all_cleaned_dt_b$FABRIC_ID <- as.character(all_cleaned_dt_b$FABRIC_ID)
all_cleaned_dt_b[all_cleaned_dt_b$FABRIC_ID %in% fabric_others,]$FABRIC_ID <- 'OTHER'
all_cleaned_dt_b$FABRIC_ID <- as.factor(all_cleaned_dt_b$FABRIC_ID)
length(unique(all_cleaned_dt_b$FABRIC_ID))

names(coef(summary(glm.train))[coef(summary(glm.train))[,4] > 0.05,4])
fab_lev_req <- c('IDVSO','IDVBO','IDTN5','IDTLF','IDTC','IDTBO','IDSTC','IDSS4','IDSS3','IDSS2','IDSKL2','IDSH2','IDSCLL','IDSBLL','IDSBLB','IDSBL','IDSBDL','IDSBDB','IDSBB','IDSALL','IDSALB','IDS51','IDS50','IDS21','IDS13','IDS11','IDRT2','IDPSP2','IDPD7','IDPD6','IDPD5','IDPC','IDPBC1','IDOTHER','IDMHPB','IDLT1','IDLNC2','IDHX6','IDHN2','IDHM5','IDHM2','IDHF7','IDHF6','IDHC3','IDHC2','IDHC1','IDH96','IDH92','IDH62','IDH60','IDH47','IDH46','IDH45','IDH44','IDH40','IDH31','IDH29','IDH28','IDH24','IDH22','IDH21','IDGCFR','IDGC1','IDGC','IDGBC2','IDGBC1','IDGBC','IDEHR','IDDLF','IDDF5','IDDF2','IDCZY2','IDBRD2','IDBLF','IDBBO','IDBA2','IDBA1','IDAMB3','IDAMB2','IDAL4','IDAL3','IDAL2','IDAB','ID00000000000000604025','ID00000000000000604024','ID00000000000000604023','ID00000000000000604005','ID00000000000000604004')

all_cleaned_dt_b[(all_cleaned_dt_b$FABRIC_ID %in% fab_lev_req), ]$FABRIC_ID<- 'OTHER'




## ORIGINAL_MATERIAL_ID
table(all_cleaned_dt_b$ORIGINAL_MATERIAL_ID)
aggregation <- all_cleaned_dt_b[,list(row_count = .N, sales = sum(NET_SALES_UNITS)), by = ORIGINAL_MATERIAL_ID]

others <- as.character(aggregation[aggregation$sales < 500,]$ORIGINAL_MATERIAL_ID) 

length(unique(all_cleaned_dt_b$ORIGINAL_MATERIAL_ID))
all_cleaned_dt_b$ORIGINAL_MATERIAL_ID <- as.character(all_cleaned_dt_b$ORIGINAL_MATERIAL_ID)
all_cleaned_dt_b[all_cleaned_dt_b$ORIGINAL_MATERIAL_ID %in% others,]$ORIGINAL_MATERIAL_ID <- 'OTHER'
all_cleaned_dt_b$ORIGINAL_MATERIAL_ID <- as.factor(all_cleaned_dt_b$ORIGINAL_MATERIAL_ID)
length(unique(all_cleaned_dt_b$ORIGINAL_MATERIAL_ID))

mat_lev_req <- c("HCCLLKTD","HCCLTRI","HCSIMP") 
all_cleaned_dt_b[(all_cleaned_dt_b$ORIGINAL_MATERIAL_ID %in% fab_lev_req), ]$ORIGINAL_MATERIAL_ID<- 'OTHER'



## OPERATING_SYSTEM_ID
table(all_cleaned_dt_b$OPERATING_SYSTEM_ID)
aggregation <- all_cleaned_dt_b[,list(row_count = .N, sales = sum(NET_SALES_UNITS)), by = OPERATING_SYSTEM_ID]

others <- as.character(aggregation[aggregation$sales < 100,]$OPERATING_SYSTEM_ID) 

length(unique(all_cleaned_dt_b$OPERATING_SYSTEM_ID))
all_cleaned_dt_b$OPERATING_SYSTEM_ID <- as.character(all_cleaned_dt_b$OPERATING_SYSTEM_ID)
all_cleaned_dt_b[all_cleaned_dt_b$OPERATING_SYSTEM_ID %in% others,]$OPERATING_SYSTEM_ID <- 'OTHER'
all_cleaned_dt_b$OPERATING_SYSTEM_ID <- as.factor(all_cleaned_dt_b$OPERATING_SYSTEM_ID)
length(unique(all_cleaned_dt_b$OPERATING_SYSTEM_ID))

op_sys_id_req <- c("CLL") 
all_cleaned_dt_b[(all_cleaned_dt_b$OPERATING_SYSTEM_ID %in% op_sys_id_req), ]$OPERATING_SYSTEM_ID<- 'OTHER'




## OPERATING_SYS_OPT_ID
table(all_cleaned_dt_b$OPERATING_SYS_OPT_ID)
aggregation <- all_cleaned_dt_b[,list(row_count = .N, sales = sum(NET_SALES_UNITS)), by = OPERATING_SYS_OPT_ID]

others <- as.character(aggregation[aggregation$sales < 100,]$OPERATING_SYS_OPT_ID) 

length(unique(all_cleaned_dt_b$OPERATING_SYS_OPT_ID))
all_cleaned_dt_b$OPERATING_SYS_OPT_ID <- as.character(all_cleaned_dt_b$OPERATING_SYS_OPT_ID)
all_cleaned_dt_b[all_cleaned_dt_b$OPERATING_SYS_OPT_ID %in% others,]$OPERATING_SYS_OPT_ID <- 'OTHER'
all_cleaned_dt_b$OPERATING_SYS_OPT_ID <- as.factor(all_cleaned_dt_b$OPERATING_SYS_OPT_ID)
length(unique(all_cleaned_dt_b$OPERATING_SYS_OPT_ID))

op_sys_id_req <- c("AT","QC") 
all_cleaned_dt_b[(all_cleaned_dt_b$OPERATING_SYS_OPT_ID %in% op_sys_id_req), ]$OPERATING_SYS_OPT_ID<- 'OTHER'



## Month
table(all_cleaned_dt_b$Month)

length(unique(all_cleaned_dt_b$Month))
str(all_cleaned_dt_b)

month_req <- c("2", "3","5", "9","7", "8") 
all_cleaned_dt_b[(all_cleaned_dt_b$Month %in% month_req), ]$Month<- 'OTHER'


## COLOR_ID
table(all_cleaned_dt_b$COLOR_ID)
aggregation <- all_cleaned_dt_b[,list(row_count = .N, sales = sum(NET_SALES_UNITS)), by = COLOR_ID]

others <- as.character(aggregation[aggregation$sales < 500,]$COLOR_ID) 

length(unique(all_cleaned_dt_b$COLOR_ID))
all_cleaned_dt_b$COLOR_ID <- as.character(all_cleaned_dt_b$COLOR_ID)
all_cleaned_dt_b[all_cleaned_dt_b$COLOR_ID %in% others,]$COLOR_ID <- 'OTHER'
all_cleaned_dt_b$COLOR_ID <- as.factor(all_cleaned_dt_b$COLOR_ID)
length(unique(all_cleaned_dt_b$COLOR_ID))

names(coef(summary(glm.train))[coef(summary(glm.train))[,4] > 0.05,4])




col_req <- c('VSO806','VSO804','VSO803','VSO802','VSO801','VBO806','VBO803','VBO801','SS310','LS333','LS319','LS302','LS1353','DC302','647L','640B','00000000000000040002','00000000000000017440','00000000000000016337','00000000000000016336','00000000000000014279','00000000000000008703','00000000000000007604','00000000000000006446','00000000000000005807','00000000000000005806','00000000000000005760','00000000000000005703','00000000000000005701','00000000000000005528','00000000000000005503','00000000000000005446','00000000000000005335','00000000000000005208','00000000000000005021','00000000000000004792','00000000000000004766','00000000000000004446','00000000000000004164','00000000000000003274','00000000000000003263','00000000000000003262','00000000000000003259','00000000000000003024','00000000000000003006','00000000000000003001','00000000000000002900','00000000000000002802','00000000000000002227','00000000000000002226','00000000000000002223','00000000000000002221','00000000000000002102','00000000000000002100','00000000000000001555','00000000000000001500','00000000000000001123','00000000000000001122','00000000000000001120','00000000000000001051','00000000000000000951','00000000000000000912','00000000000000000907','00000000000000000878','00000000000000000868','00000000000000000843','00000000000000000842','00000000000000000837','00000000000000000836','00000000000000000829','00000000000000000815','00000000000000000814','00000000000000000813','00000000000000000804','00000000000000000704','00000000000000000701','00000000000000000690','00000000000000000653','00000000000000000647','00000000000000000644','00000000000000000637','00000000000000000636','00000000000000000629','00000000000000000621','00000000000000000620','00000000000000000608','00000000000000000606','00000000000000000604','00000000000000000589','00000000000000000582','00000000000000000529','00000000000000000521','00000000000000000520','00000000000000000517','00000000000000000501','00000000000000000454','00000000000000000453','00000000000000000448','00000000000000000444','00000000000000000416','00000000000000000272','00000000000000000271','00000000000000000240','00000000000000000231','00000000000000000229','00000000000000000223','00000000000000000222','00000000000000000221','00000000000000000214','00000000000000000211','00000000000000000201','00000000000000000162','00000000000000000146','00000000000000000143','00000000000000000139','00000000000000000135','00000000000000000123','00000000000000000121','00000000000000000114','00000000000000000109','00000000000000000108','00000000000000000107','00000000000000000106','00000000000000000105','00000000000000000104','00000000000000000103','00000000000000000102') 
all_cleaned_dt_b[(all_cleaned_dt_b$COLOR_ID %in% col_req), ]$COLOR_ID<- 'OTHER'




all_cleaned_dt_b$NET_SALES_UNITS <- NULL


str(all_cleaned_dt_b)

train <- sample.split(all_cleaned_dt_b$X, SplitRatio = 0.6)
all_cleaned_dt_b.test <- all_cleaned_dt_b[!train, ]


glm.train <- glm(X ~ . , data = all_cleaned_dt_b, subset = train, family = binomial)

all_cleaned_dt_b.test$prediction <- predict(glm.train, newdata = all_cleaned_dt_b.test, type = "response")
all_cleaned_dt_b.test$prediction <- ifelse((all_cleaned_dt_b.test$prediction) > 0.04, 1, 0)
table(all_cleaned_dt_b.test$X, all_cleaned_dt_b.test$prediction)


# Whole data
all_cleaned_dt_b$prediction <- predict(glm.train, newdata = all_cleaned_dt_b, type = "response")
all_cleaned_dt_b$prediction <- ifelse((all_cleaned_dt_b$prediction) > 0.04, 1, 0)
table(all_cleaned_dt_b$X, all_cleaned_dt_b$prediction)


nrow(all_cleaned_dt_b[X==1, ])
nrow(all_cleaned_dt_b)

3933/(11367+3933)
3933/(3933+65435)
(625222+3933)/nrow(all_cleaned_dt_b)

6211/(6211+128938)
6211/(6211+9098)
(561719+6211)/nrow(all_cleaned_dt_b)

(10044)/(10044+5256)
10044/(10044 + 278119)
(412538+10044)/nrow(all_cleaned_dt_b)

str(all_cleaned_dt_b)

#True positive rate = TP/(TP+FN) or sensitivity
1486/(1486+4636)
#True negative rate = TN/(TN+FP) or specificity
1486/(1486+26183)
# Accuracy=(TP+TN)/Grand total
(1486+250067)/nrow(all_cleaned_dt_b.test)
# 
250067/(250067 + 26183)

str(all_cleaned_dt_b.test)

250067 + 26183 + 4636 + 1486
nrow(all_cleaned_dt_b.test)
