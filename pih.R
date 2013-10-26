#pih
library(xlsx)
library(data.table)
library(ggplot2)

datadir="/Users/davej/TW/PIH/data/"
big.file=paste(datadir,"UHM/july.xls",sep="")

catchment=c("Mirebalais","Saut d'Eau","Savanette")

read.uhm<-function(tab="patients",write.mysql=F,read.mysql=F,over.write=F){
   #read patient tab from spreadsheet (default) 
   #TODO: generalize this to other tabs   
   #optionally, write to mysql database 
   #optionally, read from mysql database 
   #the sheets we have with mapping to number
   start=Sys.time()
   db="umh"
   user="root"
   #the table name that will exist in MySQL
   suff=""
   tab.mysql=paste(tab,suff,sep="")
   sheets=list(patients=6,checkins=7,consultations=8,pivot_diagnoses=9,
      diagnoses=10,visits=11,hospitalization=12,vitals=13,pivot_surgery=14,
      postOpNote1=15,postOpNote2=16)
   if (!(tab %in% names(sheets))) {
      print("sheetlist")
      print(sheets)
      stop(paste("tab",tab,"not in sheet list"))
   }
   sheet=sheets[[tab]]
   start.read=Sys.time()
   if (read.mysql) {
      print(paste("Reading from MySQL, Table:",tab.mysql))
      con=dbConnect(MySQL(),user=user,db=db)
      if(!(tab.mysql %in% dbListTables(con))) {
         stop(paste("Table",tab.mysql,"not in mysql"))
      }
      data=dbReadTable(con,tab.mysql)
      dbDisconnect(con)
   } else {
      print("Reading from spreadsheet")
      data=data.table(read.xlsx(big.file,sheet))
   }
   print("Read time")
   print(Sys.time()-start.read)
   if (write.mysql) {
      start.write=Sys.time()
      print(paste("Writing to MySQL, Table: ",tab.mysql))
      con=dbConnect(MySQL(),user=user,db=db)
      if(tab.mysql %in% dbListTables(con)) {
         if (over.write==F){
            stop(paste("Table",tab.mysql,"already exist, use over.write=T to over-write"))
         } else {
            print(paste("Over-writing table",tab.mysql))
            dbRemoveTable(con,tab.mysql)
         }
      }
      dbWriteTable(con,tab.mysql,data)
      dbDisconnect(con)
      print("Write time")
      print(Sys.time()-start.write)
   }
   print("Total time")
   print(Sys.time()-start)
   return(data)
}

maternity<-function(pat=read.patients()){
   #replace excel formulas like
   #=COUNTIFS(patients!$L$2:$L$12000,"="&"F",patients!$Z$2:$Z$12000,"1")
   
   #Get all maternity patients and relevant columns
   Maternity=pat[Maternity.patient.==TRUE,
                 list(gender,age_at_reg,commune)]
   #total number of maternity patients
   Tot=nrow(Maternity)
   #add inside.catchment column
   Maternity[,inside.catchment:=commune %in% catchment]   
   #add age groups TODO: check these 
   breaks=c(0,5,14,49,150)
   Maternity[,age.group:=cut(age_at_reg,br=breaks)]
   
   #calculate tables
   #using data.table functionality
   Male.Fem=Maternity[,.N,by=gender]
   #OR like this, standard R functionality
   #Male.Fem=table(Maternity[,gender])
   Age.Groups=Maternity[,.N,by=age.group]
   Geo.Orig=Maternity[,.N,by=inside.catchment]
   
   #add percentages
   Male.Fem[,Percent:=round(100*N/Tot)]
   Age.Groups[,Percent:=round(100*N/Tot)]
   Geo.Orig[,Percent:=round(100*N/Tot)]
   #print out information
   line="---------------------------"
   print(Male.Fem)
   print(line)
   print(Age.Groups)
   print(line)
   print(Geo.Orig)
   
   #make some plots, TODO: save to file or arrange in a grid
   mf.plot<-ggplot(Male.Fem,aes(x=factor(1),fill=gender))+geom_bar(width=1)
   age.plot<-ggplot(Age.Groups,aes(age.group,N,fill=age.group))+geom_bar(stat="identity")
   print(mf.plot)
   #this will currently over-write the other one TODO: put them
   #next to each other or into png files
   print(age.plot)
}


