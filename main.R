library(RSelenium)
library(xml2)
library(dplyr)
library(rvest)
# library(tcltk2)

ppath <- "C:\\Users\\RUI-Ospite\\Desktop\\web-scraping"
setwd(ppath)
source("waitForElement.R")
# Louvre first page
attraction_link <- "https://www.tripadvisor.com/Attraction_Review-g187147-d188757-Reviews-Louvre_Museum-Paris_Ile_de_France.html"

# Creazione del driver
driver <- rsDriver(browser=c("firefox"),port=2540L,version="3.141.59",  geckover ="0.29.0"
                   # Aggiungere il seguente argomento per estrarre dati senza aprire visibilmente firefox
                   #extraCapabilities = list(
                    # "moz:firefoxOptions" = list(
                     #  args = list('--headless')
                    # ))
                   )
remote_driver <- driver[["client"]]

remote_driver$navigate(attraction_link)
waitAndClick(driver = remote_driver, iter = 60, attributeLabel = "id", attributeValue = "onetrust-accept-btn-handler")

recensioni <- data.frame(pagina= c(NA), titolo = c(NA), recensione = c(NA), data = c(NA), provenienza = c(NA), punteggio= c(NA))

# Calcolo numero di pagine

html_page <- remote_driver$getPageSource()[[1]]

reviews_number <- read_html(html_page) %>% 
  html_nodes(".Ci") %>%
  html_text()

reviews_number <- as.numeric(gsub(",","",strsplit(reviews_number, " ", fixed=T)[[1]][5]))

if((reviews_number %% 10) == 0){
  reviews_pages <- reviews_number/10
} else {
  reviews_pages <- (reviews_number %/% 10) + 1
}

reviews_count <- 0
links <- c()
page_to_recover <- c()

current_date <- Sys.Date()
for(i in 1:reviews_pages){
  
  an.error.occurred <- FALSE
  # Costruzione link pagina successiva
  if(i==1){
    current_link <- attraction_link
  } else{
    current_link <- gsub("Reviews",paste("Reviews-or",as.character((i-1)*10),sep = ""),attraction_link,fixed=T)
    links <- append(links,current_link)
  }
  
  # Navigazione alla pagina i-esima
  an.error.occurred <- tryCatch({
    #print(paste("Moving to page:", current_link))
    remote_driver$navigate(current_link)
    wait_xpath <- gsub("_",as.character(i),"//*[@aria-label='_'][@type='button']",fixed = T)
    waitForElement(driver=remote_driver, iter=30, attributeLabel="xpath", attributeValue=wait_xpath, attributeToRetrieve="type")
    #waitAndClick(driver = remote_driver, iter = 1, attributeLabel = "id", attributeValue = "onetrust-accept-btn-handler")
    
    # Estrazione reviews
    html_page <- remote_driver$getPageSource()[[1]]
    
    reviews_contents <- read_html(html_page) %>% # Only the text
      html_nodes(".dDKKM .NejBf") %>%
      html_text()
    
    reviews_score <- read_html(html_page) %>% 
      html_nodes(xpath="//*[@id='tab-data-qa-reviews-0']/div/div[5]/div/span/div/div[2]/svg") %>%
      html_attr(name="aria-label")
    
    if(length(reviews_contents)==0 |length(reviews_score)==0){
      print("The reviews in this page do not follow the usual structure, realoading the page")
      remote_driver$navigate(current_link)
      waitForElement(driver=remote_driver, iter=60, attributeLabel="xpath", attributeValue=wait_xpath, attributeToRetrieve="type")
      html_page <- remote_driver$getPageSource()[[1]]
    }
    
    # Consistenza della data della recensione corrente
    if(i!=1){
      consistency_text <- read_html(html_page) %>%
        html_nodes(".cspKb.bQCoY") %>%
        html_text()
      string <- substr(consistency_text[1],9,25)
      string <- gsub(",","",string,fixed = T)
      string_list <- strsplit(string, " ", fixed=T)
      string_list[[1]][1] <- match(string_list[[1]][1], month.name)
      date_str <- paste(string_list[[1]],collapse = '/')
      consistency_date <- as.Date(date_str, "%m/%d/%Y")
      reset_driver <- FALSE
      if(format(consistency_date,"%Y")!=format(current_date,"%Y")){ 
        print("Different years from reviews of the previous pages, restarting remote driver for safety reasons")
        try(remote_driver$close())
        try(driver$server$stop())
        try(system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout=TRUE, ignore.stderr=TRUE))
        driver <- rsDriver(browser=c("firefox"),port= 2540L, verbose = FALSE,version="3.141.59",  geckover ="0.29.0",
                           extraCapabilities = list(
                             "moz:firefoxOptions" = list(
                               args = list('--headless')
                             )))
        remote_driver <- driver[["client"]]
        remote_driver$navigate(current_link)
        waitAndClick(driver = remote_driver, iter = 60, attributeLabel = "id", attributeValue = "onetrust-accept-btn-handler")
        reset_driver <- TRUE # Do not reset the driver again in the following if
        
      }
      
      if(consistency_date>current_date && !reset_driver){ 
        print("Page inconsistency, restarting remote driver...")
        try(remote_driver$close())
        try(driver$server$stop())
        try(system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout=TRUE, ignore.stderr=TRUE))
        driver <- rsDriver(browser=c("firefox"),port= 2540L, verbose = FALSE,version="3.141.59",  geckover ="0.29.0",
                           extraCapabilities = list(
                             "moz:firefoxOptions" = list(
                               args = list('--headless')
                             )))
        remote_driver <- driver[["client"]]
        remote_driver$navigate(current_link)
        waitAndClick(driver = remote_driver, iter = 60, attributeLabel = "id", attributeValue = "onetrust-accept-btn-handler")
        
      }}
    FALSE
  },
  error = function(e) {
    print(e)
    print(consistency_date)
    print(current_date)
    try(remote_driver$close())
    try(driver$server$stop())
    try(system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout=TRUE, ignore.stderr=TRUE))
    driver <<- rsDriver(browser=c("firefox"),port= 2540L, verbose = FALSE,version="3.141.59",  geckover ="0.29.0",
                        extraCapabilities = list(
                          "moz:firefoxOptions" = list(
                            args = list('--headless')
                          )))
    remote_driver <<- driver[["client"]]
    reviews_contents <<- c("No reviews")
    remote_driver$navigate(current_link)
    waitAndClick(driver = remote_driver, iter = 60, attributeLabel = "id", attributeValue = "onetrust-accept-btn-handler")
    #Sys.sleep(3)
    TRUE
  }
  )
  if(!an.error.occurred){
    html_page <- remote_driver$getPageSource()[[1]]
    
    reviews_title <- read_html(html_page) %>% 
      html_nodes(".fUpii .NejBf") %>%
      html_text()
    
    reviews_text <- read_html(html_page) %>% 
      html_nodes(".dDKKM .NejBf") %>%
      html_text()
    
    reviews_date <- read_html(html_page) %>% 
      html_nodes(".fxays .cspKb") %>%
      html_text()
    
    reviews_state <- read_html(html_page) %>% 
      html_nodes("#tab-data-qa-reviews-0 .bQCoY span:nth-child(1)") %>%
      html_text()
    
    reviews_score <- read_html(html_page) %>% 
      html_nodes(xpath="//*[@id='tab-data-qa-reviews-0']/div/div[5]/div/span/div/div[2]/svg") %>%
      html_attr(name="aria-label")
  }else{
    print("An error occurred, moving to next iteration")
    page_to_recover <- append(page_to_recover,i)
    next
  }
  
  if(length(reviews_contents) == 0 | length(reviews_title)==0 |length(reviews_score)==0 | length(reviews_state)==0 | length(reviews_text)==0 | length(reviews_date)==0){
    print(paste("Different structure at page",i))
    page_to_recover <- append(page_to_recover,i)
    next
  }else{
    current_reviews_count <- length(reviews_contents)
    for(r in 1:current_reviews_count){
      
      if(reviews_state[r] == '' | is.na(reviews_state[r]) | is.null(reviews_state[r])){
        reviews_state[r] <- "Uknown Location"
      }
      
      if(grepl("contribution", reviews_state[r], fixed = TRUE)){
        recensioni[reviews_count+r,] <- c(i,reviews_title[r],reviews_text[r],reviews_date[r], "Uknown Location",substr(reviews_score[r],1,3))
      }else {
        recensioni[reviews_count+r,] <- c(i,reviews_title[r],reviews_text[r],reviews_date[r],reviews_state[r],substr(reviews_score[r],1,3))
      }
    }
    # Get current date
    string <- substr(reviews_date[current_reviews_count],9,25)
    string <- gsub(",","",string,fixed = T)
    string_list <- strsplit(string, " ", fixed=T)
    string_list[[1]][1] <- match(string_list[[1]][1], month.name)
    date_str <- paste(string_list[[1]],collapse = '/')
    current_date <- as.Date(date_str, "%m/%d/%Y")
  }
  reviews_count <- reviews_count + current_reviews_count
}

# Chiusura selenium webDriver
try(remote_driver$close())
try(driver$server$stop())
try(system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout=TRUE, ignore.stderr=TRUE))

# create data folder in your current working directory
save(page_to_recover,recensioni, file = "./data/louvre_recensioni.RData")

