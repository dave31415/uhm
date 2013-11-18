#Top level program

#TODO: fix issues with dots in xlsx names. Change to underscores so msql 
#doesn't change them later

rootdir="/Users/davej/TW/PIH/"
source(paste(rootdir,"/uhm/setup.R",sep=""))

define.maternity<-function(){
   loc=c("Labor and Delivery","Women\x92s Triage","Antepartum Ward","Postpartum Ward")
   consult=read.uhm('consultations')
   check=read.uhm('checkins')
   consult_pat=unique(consult[encounter_location %in% loc,zlemr])
   #print(length(consult_pat))
   check_pat=unique(check[encounter_location %in% loc,zlemr])
   #print(length(check_pat))
   both=unique(c(consult_pat,check_pat))
   nmat=length(both)
   print(paste(nmat,"unique maternity patients"))
   return(both)
}

define.end.date<-function(pat){
   d=na.omit(as.Date(pat$date_registered))
   dmax=max(d)
   year=year(dmax)
   month=month(dmax)+1
   if (month==13) {
      month=1
      year=year+1
   }
   end.date=as.Date(paste(year,month,"28",sep="-"))
   return(end.date)
}

get.maternity<-function(pat=read.uhm(tab="patients")){
   #Get all maternity patients and relevant columns
   mat.pat=data.table(zlemr=define.maternity())
   mat.pat[,Maternity.Patient:=T]
   setkey(mat.pat,zlemr)
   #Maternity=pat[Maternity_patient_==TRUE,
   #              list(gender,age_at_reg,commune,zlemr)]
   setkey(pat,zlemr)
   Maternity=pat[mat.pat,list(gender,age.at.end,commune,zlemr)]
   #add inside.catchment column
   Maternity[,inside.catchment:=commune %in% catchment]   
   #add age groups TODO: check these 
   breaks=c(0,5,14,49,150)
   Maternity[,age.group:=cut(age.at.end,br=breaks)]
}

get.womans.triage<-function(diag=read.uhm(tab="diagnoses")){
   #TODO: check this logic with Phil, looks to give 
   #numbers different from spreadsheet
   #TODO, Woman's Triage has a funny character in it, why, issue with mysql,R,excel ?
   return(diag[name =="Women\x92s Triage" & coded ==1,])
}

maternity<-function(pat=read.uhm(tab="patients")){
   #replace excel formulas like
   #=COUNTIFS(patients!$L$2:$L$12000,"="&"F",patients!$Z$2:$Z$12000,"1")
   
   #Get all maternity patients and relevant columns
   Maternity=get.maternity()
   
   #calculate tables
   #using data.table functionality
   Male.Fem=Maternity[,.N,by=gender]
   #OR like this, standard R functionality
   #Male.Fem=table(Maternity[,gender])
   Age.Groups=Maternity[,.N,by=age.group]
   Geo.Orig=Maternity[,.N,by=inside.catchment]
   #add percentages
   Tot=nrow(Maternity)
   Male.Fem[,Percent:=round(100*N/Tot)]
   Age.Groups[,Percent:=round(100*N/Tot)]
   Geo.Orig[,Percent:=round(100*N/Tot)]
   #write to html file
   HTML('<div id="male_fem">',html.file)
   HTML(Male.Fem,html.file)
   HTML('</div>',html.file)
   
   HTML('<div id="age">',html.file)
   HTML(Age.Groups,html.file) 
   HTML('</div>',html.file)
   
   HTML('<div id="geo">',html.file)
   HTML(Geo.Orig,html.file)
   HTML('</div>',html.file)
   
   #print out information
   line="---------------------------"
   print(Male.Fem)
   print(line)
   print(Age.Groups)
   print(line)
   print(Geo.Orig)
   
   #make some plots
   mf.plot<-ggplot(Maternity,aes(factor(1),fill=gender))+geom_bar()+coord_polar("y")+ylab("")+xlab("")
   age.plot<-ggplot(Maternity,aes(factor(1),fill=age.group))+geom_bar()+coord_polar("y")+ylab("")+xlab("")
   #put together into one panel?
   show_plot(mf.plot,dopng=T,file="mfplot")
   show_plot(age.plot,dopng=T,file="ageplot")
   #multiplot(mf.plot,age.plot,cols=1)
}

womans.triage.top10<-function(){
   wt=get.womans.triage()
   wt[,diag:=order.fac(diagnosis_coded_en)]
   counts=wt[,.N,by=diag]
   top.ten=counts[order(-N),][1:10,]
   top.ten=na.omit(top.ten)
   print(top.ten)
   #write to html file
   HTML('<div id="top_ten">',html.file)
   HTML(top.ten,html.file)
   HTML('</div>',html.file)
   #TODO: add an ordered factor so it plots the bars in right order                            
   #this isn't working as expected
   top.ten.plot<-ggplot(top.ten,aes(diag,N))+geom_bar(stat="identity")
   top.ten.plot=top.ten.plot+xlab("Diagnosis")
   top.ten.plot=top.ten.plot+ylab("Number")+coord_flip()
   #opts(axis.text.x  = theme_text(angle=35,hjust = 1,vjust =1))
   #print(top.ten.plot)
   #use showplot instead and show how it can be saved to file
   show_plot(top.ten.plot,dopng=dopng,file="top.ten.women",
             width=1200,height=600)
   return(top.ten)
}

clinicians<-function(diag=read.uhm(tab="diagnoses")){
   #make the table and plots on the clinicians on the maternity tab
   loc=c("Labor and Delivery","Women\x92s Triage","Antepartum Ward","Postpartum Ward")
   diag=diag[name %in% loc,]
   diag=na.omit(diag[,list(provider,coded)])
   diag=diag[coded %in% c(0,1),]
   d=diag[,list(Num.DX=.N,Num.DX.coded=sum(coded)),by=provider]
   d=d[Num.DX>0,]
   d[,Percent:=as.integer(100.0*Num.DX.coded/Num.DX)]
   d=d[order(-Num.DX),]
   #change order of columns
   d=d[,list(Num.DX.coded,Num.DX,Percent,provider)]
   #write to terminal
   print(d)
   #write to html file
   HTML('<div id="clinicians">',html.file)
   HTML(d,html.file)
   HTML('</div>',html.file)
   p<-ggplot(d,aes(provider,Percent))+geom_bar(stat="identity")
   p=p+ylab("Percent Coded")
   p=p+coord_flip()
   show_plot(p,dopng=T,file="providers")
   return(d)
}

run.maternity<-function(){
   #run all the functions and create an ugly html file
   #obviously we could improve the webpage by making a template
   
   #target <- HTMLInitFile(filename=html.file, BackGroundColor="#BBBBEE")
   HTML("<html><h1> This Webpage is Sooo Ugli!!</h1>",file=html.file,append=F)
   maternity()
   womans.triage.top10()
   clinicians()
   HTML('<div id="mfplot"><img src="mfplot.png"></div>"
      <div id="ageplot>"><img src="ageplot.png"></div>
      <div id="providers_plot"> <img src="providers.png"></div>
      <div id="top_ten_plot"><img src="top.ten.women.png"></div>',file=html.file)
}

hosp.time<-function(){
   hosp=read.uhm("hospitalization")
   admit=c("Antepartum Ward","Labor and Delivery","Postpartum Ward")
   h=hosp[admitting_ward %in% admit & outcome == "Discharged",]
   tab=h[,list(.N,MeanHospTime=mean(length_of_hospitalization)),by=admitting_ward]
   print(tab)
   h=hosp[admitting_ward %in% admit & outcome == "Still Hospitalized" 
            & length_of_hospitalization >= 14,]
   tab2=h[,list(.N,MeanHospTime=mean(length_of_hospitalization)),by=admitting_ward]
   print(tab2)
}

cascade<-function(){
   #create the contingency table cascade
   loc=c("Labor and Delivery","Women\x92s Triage","Antepartum Ward","Postpartum Ward")
   consult=read.uhm("consultations")
   consult=consult[encounter_location %in% loc,list(encounter_location,dispo_location,disposition)]
   disp=c("Admit to hospital","Transfer within hospital")
   #consult=consult[encounter_location %in% loc & disposition %in% disp,]
   consult[,ad.tran:=(disposition %in% disp)]
   consult[,dispo.new:=disposition]
   consult[ad.tran==T,dispo.new:=paste("Admit/Transfer to",dispo_location)]
   
   con=consult[,.N, by=list(encounter_location,dispo.new)]
   tab=xtabs(N ~ dispo.new + encounter_location, con)
   return(tab)
}
   

