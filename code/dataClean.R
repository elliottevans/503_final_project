setwd("~/Desktop/STATS 503/503_final_project")
source('code/functions.r')

#Large file, takes a few seconds to read
dat<-read.csv("data\\Most-Recent-Cohorts-All-Data-Elements.csv")
cols_using<-read.csv("data\\cols_using.csv")
cols_using<-subset(cols_using,VARIABLE.NAME!='')
#Subset the data to non-null responses
sub_dat<-subset(dat,MD_EARN_WNE_P6!='NULL')

#Get the percent of missing values for each column
col_info<-apply(sub_dat, 2, function(col)sum(col=='NULL')/length(col))

#The columns with the most missing data
sort(col_info[which(colnames(sub_dat) %in% cols_using$VARIABLE.NAME)],
     decreasing = T)

write.csv(sub_dat[,cols_using$VARIABLE.NAME],'data\\sub_dat.csv',row.names = F)

