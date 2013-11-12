#io functions

read.uhm<-function(tab="patients",write.mysql=F,read.mysql=T,over.write=F){
   #read patient tab from spreadsheet (default) 
   #TODO: generalize this to other tabs   
   #optionally, write to mysql database 
   #optionally, read from mysql database 
   #the sheets we have with mapping to number
   start=Sys.time()
   db="uhm"
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
      data=data.table(dbReadTable(con,tab.mysql))
      dbDisconnect(con)
   } else {
      print("Reading from spreadsheet")
      data=data.table(read.xlsx(big.file,sheet))
      print(paste(nrow(data),"rows"))
   }
   #print("Read time")
   #print(Sys.time()-start.read)
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
      #print("Write time")
      #print(Sys.time()-start.write)
   }
   #print("Total time")
   #print(Sys.time()-start)
   if (tab=="patients"){
      end.date=define.end.date(data)
      birth.date=data[,as.Date(birthdate)]
      age=as.numeric((end.date-birth.date))/365.25
      data[,age.at.end:=age]
   }
   return(data)
}

load.all.mysql<-function(){
   #read all tabs from the spreadsheet and write them all to mysql
   #after that, read the mysql tables in just to check things
   
   sheets=list(
      patients=6,checkins=7,consultations=8,
      #pivot_diagnoses=9,
      diagnoses=10,visits=11,hospitalization=12,
      vitals=13,
      #pivot_surgery=14,
      postOpNote1=15,
      postOpNote2=16)
   
   for (tab in names(sheets)) {
      print(tab)
      d=read.uhm(tab,write.mysql=T)
      d2=read.uhm(tab,read.mysql=T)
   }
}
