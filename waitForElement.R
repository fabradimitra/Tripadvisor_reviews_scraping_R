# Wait for element 
waitForElement <- function(driver, iter, attributeLabel, attributeValue, attributeToRetrieve=NULL) {
  found <- FALSE
  for (t in 1:iter){
    if(is.null(attributeToRetrieve)){
      element <- suppressMessages(try(unlist(driver$findElement(using = attributeLabel, value = attributeValue)$getElementAttribute(attributeLabel)),silent = TRUE))
    } else {
      element <- suppressMessages(try(unlist(driver$findElement(using = attributeLabel, value = attributeValue)$getElementAttribute(attributeToRetrieve)),silent = TRUE))
    }
    try({
    if(!grepl("NoSuchElementException", element[1], fixed = TRUE)){
      found <- TRUE
      break
      }
    })
    #print(found)
    Sys.sleep(1)
    if(found){
      break
    }
  }
  return(found)
}

# Wait and click
waitAndClick <- function(driver, iter, attributeLabel, attributeValue, attributeToRetrieve=NULL) {
  clicked <- FALSE
  for (t in 1:iter){
    if(is.null(attributeToRetrieve)){
      element <- suppressMessages(try(unlist(driver$findElement(using = attributeLabel, value = attributeValue)$getElementAttribute(attributeLabel)),silent = TRUE))
    } else {
      element <- suppressMessages(try(unlist(driver$findElement(using = attributeLabel, value = attributeValue)$getElementAttribute(attributeToRetrieve)),silent = TRUE))
    }
    try({
    if(!grepl("NoSuchElementException", element[1], fixed = TRUE)){
      webElem <- driver$findElement(using = attributeLabel, value = attributeValue)
      webElem$clickElement()
      clicked <- TRUE
      }
    })
    #print(clicked)
    Sys.sleep(1)
    if(clicked){
      break
    }
  }
  return(clicked)
}
