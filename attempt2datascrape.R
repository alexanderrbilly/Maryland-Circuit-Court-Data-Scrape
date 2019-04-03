#This code was created for a dissertation project by Alexander Billy at 
#Georgetown University. 

#The data scrapes Circuit Court forfeiture related cases and creates .csv files.
#Note: manual exploration of Prince George's and Montgomery County will be required
#Otherwise, all other states follow the standardized procedure. 

#The code searches alphabetically for records in a certain county. Here, I have listed
#Howard county as the county from which to scrape records. 

#Given issues with the server's response time, I have made it possible to manually 
#change the start and end dates. 

#I have also provided you with the Selenium framework to use Chrome or Firefox.

remDr$close()
rm(rD)
gc()
rm(list=ls())
setwd("~/Desktop/WebScrape")
library(httr)
library(rvest)
library(zoo)
library(anytime)
library(lubridate)
library(dplyr)
library(RSelenium)

#Selecting when to stop looking
final <- "04/01/2016"
finale <- as.Date(anydate(final))  

search <- function(x,y,z,q){
  if(x == 1){
    search_again <- remDr$findElement(using = "link text","Search Again")
    search_again$clickElement()
    #reorbit <<- 0
    alpha_box <- remDr$findElement("name","lastName")
    alpha_box$sendKeysToElement(list(y))
    court_system <- remDr$findElement("name","courtSystem")
    court_system <- remDr$findElement("xpath","//input[@value='C']")
    court_system$clickElement()
    county_name <- remDr$findElement("name","countyName")
    option <- remDr$findElement(using = 'xpath', "//*/option[contains(@value,\"HOWARD\")]")
    #option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'HOWARD']")
    option$clickElement()
    start_date <- remDr$findElement("name","filingStart")
    
    start_date$sendKeysToElement(list(format(as.Date(z,"%m/%d/%yyyy"),"%m/%d/%Y")))
    end_date <- remDr$findElement("name","filingEnd")
    end_date$sendKeysToElement(list(format(as.Date(q,"%m/%d/%yyyy"),"%m/%d/%Y")))
    
    #Submit search
    submit_search <- remDr$findElement("name","action")
    submit_search$clickElement()
    
    #Generate warning message if it appears on first search
    warning_500 <- remDr$findElement("class name","pagebanner")
    reorbit <<- ifelse(substr(warning_500$getElementText(),1,15)=="500 items found",1,0)
    title<- remDr$getTitle()
  }
  else if(x ==0){
    search_again <- remDr$findElement(using = "link text","Search Again")
    search_again$clickElement()
    alpha_box <- remDr$findElement("name","lastName")
    alpha_box$sendKeysToElement(list(y))
    court_system <- remDr$findElement("name","courtSystem")
    court_system <- remDr$findElement("xpath","//input[@value='C']")
    court_system$clickElement()
    county_name <- remDr$findElement("name","countyName")
    option <- remDr$findElement(using = 'xpath', "//*/option[contains(@value,\"HOWARD\")]")
    #option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'HOWARD']")
    option$clickElement()
    start_date <- remDr$findElement("name","filingStart")
    start_date$sendKeysToElement(list(format(as.Date(z,"%m/%d/%yyyy"),"%m/%d/%Y")))
    end_date <- remDr$findElement("name","filingEnd")
    end_date$sendKeysToElement(list(format(as.Date(q,"%m/%d/%yyyy"),"%m/%d/%Y")))
    
    submit_search <- remDr$findElement("name","action")
    submit_search$clickElement()
    
    qerror <- "Error : \t Summary: NoSuchElement\n \t Detail: An element could not be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n\t Further Details: run errorDetails method\n"
    q_death <- try(unlist(remDr$findElement("class name", "pagebanner")$getElementAttribute('id')),silent = TRUE)
    
    
    #Generate warning message if it appears on first search
    while(q_death[1] == qerror & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      date_end <- date_end+1
      end_date <- remDr$findElement("name","filingEnd")
      end_date$clearElement()
      end_date$sendKeysToElement(list(format(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y")))
      submit_search <- remDr$findElement("name","action")
      submit_search$clickElement()
      q_death <- try(unlist(remDr$findElement("class name", "pagebanner")$getElementAttribute('id')),silent = TRUE)
    }
    
    warning_500 <- remDr$findElement("class name","pagebanner")
    reorbit <<- ifelse(substr(warning_500$getElementText(),1,15)=="500 items found",1,0)
    title<- remDr$getTitle()
    #csv.export <- remDr$findElement(using = "link text","CSV")
    #csv.export$clickElement()
  }
}

eCaps <- list(
  chromeOptions = 
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = "~/Desktop/WebScrape",
      "safebrowsing.enabled" = TRUE
    )
    )
)

fprof <- makeFirefoxProfile(list(browser.download.dir = "~/Desktop/WebScrape"
                                 ,browser.download.folderList = 2L
                                 ,browser.download.manager.showWhenStarting = FALSE
                                 ,browser.download.manager.showAlertOnComplete = FALSE
                                 ,browser.helperApps.neverAsk.openFile = "text/csv; charset=UTF-8"
                                 ,browser.helperApps.neverAsk.saveToDisk = "text/csv; charset=UTF-8"))


browser <- remoteDriver(port = 5556, browserName = "firefox",extraCapabilities = fprof)
remDr<-browser[["client"]]
url <- "http://casesearch.courts.state.md.us/casesearch/"

rD <- rsDriver(verbose=FALSE,port=4444L,browser="firefox")
#rD <- rsDriver(verbose=FALSE, port=4444L, browser="firefox")
remDr <- rD$client
#remDr$open(silent = TRUE)
#Go to main page
remDr$navigate(url)


#rD <- rsDriver(verbose=FALSE, port=4444L, browser="firefox", extraCapabilities=eCaps)
#remDr <- rD$client

#Go to main page

#Click the disclaimer messages
selectAll <- remDr$findElement("name","disclaimer")
selectAll$clickElement()
selectAction <- remDr$findElement("name","action")
selectAction$clickElement()

reorbit <- 0
for (i in 3:length(letters)){
  #replace A with phoenecians
  #Type in Alphabetically, Select Circuit Court System
  if(i == 1){
    start1 <- "01/01/2010"
    date_start <- as.Date(anydate(start1))
    alpha_box <- remDr$findElement("name","lastName")
    alpha_box$sendKeysToElement(list(letters[i]))
    court_system <- remDr$findElement("name","courtSystem")
    court_system <- remDr$findElement("xpath","//input[@value='C']")
    court_system$clickElement()
    county_name <- remDr$findElement("name","countyName")
    option <- remDr$findElement(using = 'xpath', "//*/option[contains(@value,\"HOWARD\")]")
    #option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'HOWARD']")
    option$clickElement()
    
    #Creating a dynamic date entry pattern
    start_date <- remDr$findElement("name","filingStart")
    start_date$sendKeysToElement(list(format(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y")))
    end_date <- remDr$findElement("name","filingEnd")
    
    #use this date to check
    #date_end <- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=5)
    date_end <<- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=3)
    #date_end <<- date_start + 10
    end_date$sendKeysToElement(list(format(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y")))
    
    #Submit search
    submit_search <- remDr$findElement("name","action")
    submit_search$clickElement()
    
    print("I got to the search page")
    
    error <- "Error : \t Summary: NoSuchElement\n \t Detail: An element could not be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n \t Further Details: run errorDetails method"
    fatal <- try(unlist(remDr$findElement(using="xpath", "pagebanner")$getElementAttribute('id')),silent = TRUE)
    
    #Generate warning message if it appears on first search
    while(fatal[1] == error & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y"))) &format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      date_end <<- date_end+1
      end_date <- remDr$findElement("name","filingEnd")
      end_date$clearElement()
      print(as.Date(date_end,"%m/%d/%yyyy"))
      end_date$sendKeysToElement(list(format(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y")))
      submit_search <- remDr$findElement("name","action")
      submit_search$clickElement()
      fatal<- try(unlist(remDr$findElement("class name", "error")$getElementAttribute('id')),silent = TRUE)
    }
    
    if(fatal[1]!=error){
      warning_500 <- remDr$findElement(using = "class name","pagebanner")
      reorbit <<- ifelse(substr(warning_500$getElementText(),1,15)=="500 items found",1,0)
    }
    
    search_again <- remDr$findElement(using = "link text","Search Again")
    title<- remDr$getTitle()
    
    #added while loop here
    while(format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))& format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
    if(reorbit == 1 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))& format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      while(reorbit == 1 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))& format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
        date_end <<- date_end-1
        search(reorbit,letters[i],date_start,date_end)
      }
    }
    else if(reorbit == 0 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))& format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      while(reorbit == 0 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))&& format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
        csv_error <- "Error : \t Summary: NoSuchElement\n \t Detail: An element could not be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n \t Further Details: run errorDetails method"
        csv_death<- try(unlist(remDr$findElement(using='class name', "pagebanner")$getElementAttribute('id')),silent = TRUE)
        if(csv_error[1]!=csv_death){
          csv.export <- remDr$findElement(using = "link text","CSV")
          csv.export$clickElement()
          date_start <<-date_end+1
          #date_end <- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=52)
          date_end <<- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=3)
          #date_end <<- date_start + 10
          search(reorbit,letters[i],date_start,date_end)
        }
        if(csv_error[1]==csv_death){
          search(reorbit,letters[i],date_start,date_end)
        }
      }
    }
    }
    
    setwd("/Users/alexanderrbilly/Downloads")
    #setwd("/Users/alexanderrbilly/Desktop/SaveScrape/HOWARD F")
    #temp <- list.files(pattern='\\.csv', recursive=TRUE)
    temp <- list.files(pattern='\\.csv.part', recursive=TRUE)
    #
    #colnames(HOWARD) <- c("CaseNo","Defendent","Date_Useless","Type","Circuit Court","CaseType","Status","File_Date","Description")
    
    
    #data_HOWARD <- do.call("rbind",HOWARD)
    if(length(temp)!=0)
    {
      HOWARD <- do.call(rbind,lapply(temp, function(x) {
        tryCatch(read.csv(x, header = FALSE, sep = ','), error=function(e) NULL)
      }))
      colnames(HOWARD) <- c("CaseNo","Defendent","Date_Useless","Type","Circuit Court","CaseType","Status","File_Date","Description")
      data_HOWARD <- HOWARD[(HOWARD$CaseType=="FF" | substr(HOWARD$CaseType,1,4)=="Forf" | substr(HOWARD$CaseType,1,4)=="FORF"),]
      #setwd("/Users/alexanderrbilly/Desktop/SaveScrape/HOWARD B")
      setwd("/Users/alexanderrbilly/Desktop/WebScrape/HOWARD")
      outfile <- paste(i,"-HOWARD",sep="")
      write.csv(data_HOWARD,quote=FALSE,sep=",",outfile)
    }
    #do.call(file.remove,list(list.files(pattern='casesearch-.*\\.csv')))
    setwd("/Users/alexanderrbilly/Downloads")
    do.call(file.remove,list(list.files(pattern='\\.csv.part')))
    search_again <- remDr$findElement(using = "link text","Search Again")
    search_again$clickElement()
    i <<- i+1
  }
  else if((i > 1)){
  #else if((i > 1)&(i < 26)&(i!=24)){
    start1 <- "01/01/2010"
    date_start <- as.Date(anydate(start1))
    final <- "03/30/2016"
    finale <- as.Date(anydate(final))
    
    alpha_box <- remDr$findElement("name","lastName")
    alpha_box$clearElement()
    alpha_box$sendKeysToElement(list(letters[i]))
    court_system <- remDr$findElement("name","courtSystem")
    court_system <- remDr$findElement("xpath","//input[@value='C']")
    court_system$clickElement()
    county_name <- remDr$findElement("name","countyName")
    option <- remDr$findElement(using = 'xpath', "//*/option[contains(@value,\"HOWARD\")]")
    #option <- remDr$findElement(using = 'xpath', "//*/option[@value = 'HOWARD']")
    option$clickElement()
    
    #Creating a dynamic date entry pattern
    start_date <- remDr$findElement("name","filingStart")
    start_date$clearElement()
    start_date$sendKeysToElement(list(format(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y")))
    end_date <- remDr$findElement("name","filingEnd")
    
    #use this date to check
    #date_end <- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=5)
    date_end <<- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=3)
    #date_end <<- date_start+10
    end_date$clearElement()
    end_date$sendKeysToElement(list(format(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y")))
    
    #Submit search
    if(format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      submit_search <- remDr$findElement("name","action")
      submit_search$clickElement()
    }
    
    qerror_1 <- "Error : \t Summary: NoSuchElement\n \t Detail: An element could not be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n \t Further Details: run errorDetails method"
    q_death_1<- try(unlist(remDr$findElement(using='class name', "pagebanner")$getElementAttribute('id')),silent = TRUE)
    #Generate warning message if it appears on first search
    while(qerror_1 == q_death_1[1] & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))&format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      print("qerror")
      date_end <<- date_end+1
      end_date <- remDr$findElement("name","filingEnd")
      end_date$clearElement()
      print(as.Date(date_end,"%m/%d/%yyyy"))
      end_date$sendKeysToElement(list(format(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y")))
      submit_search <- remDr$findElement("name","action")
      submit_search$clickElement()
      q_death_1<- try(unlist(remDr$findElement("class name", "pagebanner")$getElementAttribute('id')),silent = TRUE)
    }
    
    if(format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))& format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
    if(qerror_1!=q_death_1[1]){
      warning_500 <- remDr$findElement(using = "class name","pagebanner")
      reorbit <<- ifelse(substr(warning_500$getElementText(),1,15)=="500 items found",1,0)
    }
    search_again <- remDr$findElement(using = "link text","Search Again")
    }
    
    while(format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))& format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
    if(reorbit == 1 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y"))) & format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      while(reorbit == 1 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y"))) & format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
        print("500 error")
        date_end <- date_end-1
        search(reorbit,letters[i],date_start,date_end)
      }
    }
    else if(reorbit == 0 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y"))) &format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
      while(reorbit == 0 & format(as.Date(anydate(as.Date(date_start,"%m/%d/%yyyy"),"%m/%d/%Y"))) < format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y"))) &format(as.Date(anydate(as.Date(date_end,"%m/%d/%yyyy"),"%m/%d/%Y"))) <= format(as.Date(anydate(as.Date(finale,"%m/%d/%yyyy"),"%m/%d/%Y")))){
        print("no longer error")
        csv_error <- "Error : \t Summary: NoSuchElement\n \t Detail: An element could not be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n \t Further Details: run errorDetails method"
        csv_death<- try(unlist(remDr$findElement(using='class name', "pagebanner")$getElementAttribute('id')),silent = TRUE)
        if(csv_error[1]!=csv_death){
          csv.export <- remDr$findElement(using = "link text","CSV")
          csv.export$clickElement()
          date_start <<-date_end+1
          #date_end <- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=52)
          date_end <<- as.Date(as.yearmon(seq.Date(date_start,by='month',length.out = 1)),frac=3)
          #date_end <<- date_start + 10
          search(reorbit,letters[i],date_start,date_end)
        }
        if(csv_error==csv_death[1]){
          search(reorbit,letters[i],date_start,date_end)
        }
      }
    }
    }
    
    setwd("/Users/alexanderrbilly/Downloads")
    #temp <- list.files(pattern='\\.csv', recursive=TRUE)
    temp <- list.files(pattern='\\.csv.part', recursive=TRUE)
    #colnames(HOWARD) <- c("CaseNo","Defendent","Date_Useless","Type","Circuit Court","CaseType","Status","File_Date","Description")
    #data_HOWARD <- do.call("rbind",HOWARD)
    
    if(length(temp)!=0)
    {
      HOWARD <- do.call(rbind,lapply(temp, function(x) {
        tryCatch(read.csv(x, header = FALSE, sep = ','), error=function(e) NULL)
      }))
      colnames(HOWARD) <- c("CaseNo","Defendent","Date_Useless","Type","Circuit Court","CaseType","Status","File_Date","Description")
      data_HOWARD <- HOWARD[(HOWARD$CaseType=="FF" | substr(HOWARD$CaseType,1,4)=="Forf" | substr(HOWARD$CaseType,1,4)=="FORF"),]
      setwd("/Users/alexanderrbilly/Desktop/WebScrape/HOWARD")
      outfile <- paste(i,"-HOWARD",sep="")
      write.csv(data_HOWARD,quote=FALSE,sep=",",outfile)
    }
    
    
    #do.call(file.remove,list(list.files(pattern='casesearch-.*\\.csv')))
    setwd("/Users/alexanderrbilly/Downloads")
    do.call(file.remove,list(list.files(pattern='\\.csv.part')))
    
    #qerror <- "Error : \t Summary: NoSuchElement\n \t Detail: An element could not be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n"
    #q_death <- try(unlist(remDr$findElement("class name", "error")$getElementAttribute('id')),silent = TRUE)
    search_again <- remDr$findElement(using = "link text","Search Again")
    search_again$clickElement()
    i <<- i+1
  }
}
