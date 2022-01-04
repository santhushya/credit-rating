library(rJava)
library(RJDBC)
# ------------------------------------------ connection ----------------------------------------------------- #
db_connect <- function(){
  jdbcDriver <-JDBC("oracle.jdbc.OracleDriver",classPath="ojdbc6.jar")
  ac <- dbConnect(jdbcDriver,"jdbc:oracle:thin:@//10.251.110.8:1536/FUSIONP_STDBY","IFRSDATAUSR","IFRS123$Pw")
  return(ac)
}

ac <- db_connect()

application_test <- dbGetQuery(ac,"SELECT * FROM  BRACFUSION.KPMG_DATAVIEW_LODF where year = '2021' and month = '03'")

save(application_test, 'rdata/application_test.RData')