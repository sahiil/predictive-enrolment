options(stringsAsFactors = F)
rm(list=ls())
cat('\014')
if(!("pacman" %in% rownames(installed.packages()))){
  install.packages("pacman")
}

require("pacman")
p_load(growthmodels)
p_load(dplyr, data.table, stringr)
p_load(yaml, minpack.lm)
p_load(DBI,odbc)


self.DB_HOST = '172.21.188.217'  # TODO:Remove this line on PRODUCTION
self.DB_PORT = '1433'  # TODO:Remove this line on PRODUCTION
self.DB_USER = 'Srv_sql_DCCCD'  # TODO:Remove this line on PRODUCTION
self.DB_PW = 'Nolw0@83pqPn'  # TODO:Remove this line on PRODUCTION
self.DB_NAME = 'dcccd_db'  # TODO:Remove this line on PRODUCTION
self.DB_DRIVER = 'ODBC Driver 17 for SQL Server'  # TODO:Remove this line on PRODUCTION

cn<-dbConnect(odbc(), 
              Driver = self.DB_DRIVER, 
              Server = self.DB_HOST, 
              Database = self.DB_NAME,
              UID=self.DB_USER,
              PWD=self.DB_PW,
              MultipleActiveResultSets='true'
)


print(getwd())
setwd('D:\\DCCCD\\StudentCentricScheduling\\predictive_curves')
source('curve_fitting_functions.R')
config<-"r_config.csv"
df.config <- dbGetQuery(cn,'SELECT * FROM r_config')

#default=filter(df.config,Variable=='default)$'value'
run_semester=filter(df.config,Variable=='sem')$'value'
run_semester=filter(df.config,Variable=='sem')$'value'
run_ay=filter(df.config,Variable=='ay')$'value'
# gompertz_flg=strtoi(filter(df.config,Variable=='gompertz')$'value')
# weibull_flg=strtoi(filter(df.config,Variable=='weibull')$'value')
# linear_flg=strtoi(filter(df.config,Variable=='linear')$'value')
# logrithmic_flg=strtoi(filter(df.config,Variable=='logrithmic')$'value')
# loglogit_flg=strtoi(filter(df.config,Variable=='loglogit')$'value')
# negexp_flg=strtoi(filter(df.config,Variable=='negexp')$'value')
start_trim=strtoi(filter(df.config,Variable=='start_trim')$'value')
end_trim=strtoi(filter(df.config,Variable=='end_trim')$'value')
run_ay=filter(df.config,Variable=='ay')$'value'
run_id=filter(df.config,Variable=='run_id')$'value'
fname <- paste0("autotest_",run_ay,"_",run_semester)

trim <- 1 # if yes then 1 else 0

setwd('..')


df.raw <- dbGetQuery(cn,sprintf('select TIME,ENROLLMENT,FIL,GROUP1 from %s', fname))
df.raw$TIME=as.numeric(df.raw$TIME)
df.raw$ENROLLMENT=as.numeric(df.raw$ENROLLMENT)
# PRELIMINARY DATA PREP
df.raw <- df.raw[df.raw$FIL>0,]

# max week calculation
df.raw <- df.raw %>%
  group_by(GROUP1) %>%
  mutate(MAX_WK = max(TIME))

# creating labels

library(stringr)
groupinfo=str_split_fixed(df.raw$GROUP1, '_', 4)
df.raw$COLLEGE <- groupinfo[,1]
df.raw$COURSE <- groupinfo[,2]
df.raw$MODALITY <- groupinfo[,3]
df.raw$MARKER <- groupinfo[,4]
# 
# df.raw$COLLEGE <- substr(df.raw$GROUP1, 1, str_locate(df.raw$GROUP1, "College") + 6)
# df.raw$COURSE <- substr(str_replace(df.raw$GROUP1, df.raw$COLLEGE, ""), 1, 9)
# df.raw <- df.raw %>%
#   mutate(MODALITY = (ifelse(grepl("DL", GROUP1), "DL",
#                             ifelse(grepl("F2F", GROUP1), "F2F", "Other"))))
# df.raw$MARKER <- str_replace(df.raw$GROUP1, paste0(df.raw$COLLEGE, df.raw$COURSE, df.raw$MODALITY), "")




# ifelse(tolower(run_semester)=='fall' | tolower(run_semester)=='spring',
#   {df.raw <- df.raw %>%
#     mutate(MAX_WK = ifelse(MARKER %in% "16", MAX_WK - 21, MAX_WK - 14))
#   df.raw <- df.raw[df.raw$TIME <= df.raw$MAX_WK & df.raw$TIME>60,]},
#   {df.raw <- df.raw %>%
#     mutate(MAX_WK = ifelse(MARKER %in% "16", MAX_WK - 14, MAX_WK - 14))
#  df.raw <- df.raw[df.raw$TIME <= df.raw$MAX_WK,]})

# #Trimming the ends
# df.raw <- df.raw %>%mutate(MAX_WK =  MAX_WK - end_trim)
# #Trimming the starts
# df.raw <- df.raw[df.raw$TIME <= df.raw$MAX_WK & df.raw$TIME>start_trim,]

ifelse(tolower(run_semester)=='fall' | tolower(run_semester)=='spring',
       {
         df.raw <- df.raw %>%
           mutate(MAX_WK1 = ifelse(MARKER %in% "16", MAX_WK-end_trim, MAX_WK-end_trim))
         df.raw <- df.raw[df.raw$TIME <= df.raw$MAX_WK & df.raw$TIME>0,]},
       {df.raw <- df.raw %>%
         mutate(MAX_WK = ifelse(MARKER %in% "16", MAX_WK - end_trim, MAX_WK - end_trim))
       df.raw <- df.raw[df.raw$TIME <= df.raw$MAX_WK,]})
# df.raw$test<-0
# df.raw$MAX_WK=df.raw$MAX_WK-1
# df.raw %>%
#   mutate(MAX_WK1 = ifelse(MARKER %in% "16", MAX_WK-1, MAX_WK-1))

df.default <- df.raw[df.raw$TIME == df.raw$MAX_WK, ]
print("============== FITTING GOMPERTZ DISTRIBUTION ================================")
df.gompertz <- bind_rows(lapply(unique(df.raw$GROUP1), function(x) fit.gompertz(x, df.raw)))

print("============== FITTING WEIBULL DISTRIBUTION ================================")
df.weibull <- bind_rows(lapply(unique(df.raw$GROUP1), function(x) fit.weibull(x, df.raw)))

print("============== FITTING LOGLOGIT DISTRIBUTION ================================")
df.loglogit <- bind_rows(lapply(unique(df.raw$GROUP1), function(x) fit.loglogit(x, df.raw)))

print("============== FITTING NEGEXP DISTRIBUTION ================================")
df.negexp <- bind_rows(lapply(unique(df.raw$GROUP1), function(x) fit.negexp(x, df.raw)))



print("============== FITTING LINEAR DISTRIBUTION ================================")
df.linear <- bind_rows(lapply(unique(df.raw$GROUP1), function(x) fit.linear(x, df.raw)))

print("============== FITTING LOGRITHMIC DISTRIBUTION ================================")
df.logrithmic <- bind_rows(lapply(unique(df.raw$GROUP1), function(x) fit.log(x, df.raw)))
print("before time")
#end <- Sys.time()

#print(paste0("TIME ELAPSED ---> ",as.numeric(end - start, units = "secs"), "Secs"))
print("============== COLLATE THE RESULTS ================================")
df.all <- bind_rows(df.gompertz, df.weibull, df.loglogit, df.negexp, df.linear, df.logrithmic)
df.collated <- collate.results(df.all)
print("REACHED THE END")
df.collated$run_id<-run_id
df.all$run_id<-run_id
df.default$run_id<-run_id


batchupload<-function(df,dbname,batchsize=10000){
  df.empty<- head(df,0)
  system.time(
    cn %>% dbWriteTable(
      dbname,
      df.empty, overwrite = TRUE
    )
  )
  
  if (nrow(df) >= batchsize) {
    print('Batch mode')
    batch <- split(df, 1:nrow(df) %/% batchsize)
    
    op <- lapply(batch, function(x) {
      cn%>%dbWriteTable(
        dbname,
        x, append = TRUE
      )
    })
  } else { 
    print("single mode")
    system.time(
      cn %>% dbWriteTable(
        dbname,
        df, overwrite = TRUE
      )
    )
  }
}

df.collated[is.na(df.collated)] <- NA
df.collated <- df.collated[is.finite(df.collated$PREDICTION),]
batchupload(df.collated,paste0(run_ay,"_",run_semester,'_modelSummary_output'),batchsize=10000)
# system.time(
#   cn %>% dbWriteTable(
#     paste0(run_ay,"_",run_semester,'_modelSummary_output'),
#     df.collated, overwrite = TRUE
#   )
# )
print("Output for model summary completed")

df.all[is.na(df.all)] <- NA
df.all <- df.all[is.finite(df.all$PREDICTION),]
batchupload(df.all,paste0(run_ay,"_",run_semester,'_powerBI_output'),batchsize=10000)
# system.time(
#   cn %>% dbWriteTable(
#     paste0(run_ay,"_",run_semester,'_powerBI_output'),
#     df.all, overwrite = TRUE
#   )
# )
print("Output for PowerBi complete")
df.default[is.na(df.default)] <- NA
#df.default <- df.default[is.finite(rowSums(df.default),]
batchupload(df.default,paste0(run_ay,"_",run_semester,'_default_output'),batchsize=10000)
# system.time(
#   cn %>% dbWriteTable(
#     paste0(run_ay,"_",run_semester,'_default_output'),
#     df.default, overwrite = TRUE
#   )
# )
print("Output for default complete")
# 
# fwrite(df.all, paste0('02_Output//',run_ay,"_",run_semester,'_powerBI_output'), row.names = F)
# fwrite(df.collated, paste0('02_Output//',run_ay,"_",run_semester,'_modelSummary_output', '.csv'), row.names = F)
# fwrite(df.default, paste0('02_Output//',run_ay,"_",run_semester,'_default_output', '.csv'), row.names = F)
print("OUTPUT COMPLETED")
