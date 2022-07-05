library(xml2)
library(tidyverse)

records <- xml2::read_xml("C:/Users/Samuel/OneDrive/Desktop/export.xml")


xml_find_all(
  records,
  xpath = "
    .//Record[
         @type = 'HKQuantityTypeIdentifierHeartRate' and
         (starts-with(@endDate, '2021'))
      ]"
) -> records



tibble(
  ts = records %>% 
    xml_attr("endDate") %>% 
    as.POSIXct(format = "%Y-%m-%d %H:%M:%S %z"),  
  rate = records %>% 
    xml_attr("value") %>% 
    as.integer()
) -> records

records
