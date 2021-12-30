# Libraries ---------------------------------------------------------------
lapply(as.list(c("tidyverse","janitor","readxl","writexl",
                 "rvest","xml2")),require,character.only=TRUE)
# URL ---------------------------------------------------------------------
Data_Sets <- read_html("https://www.education.gov.za/Programmes/EMIS/EMISDownloads.aspx") %>% 
  html_elements("a") %>% 
  html_attr(name = "href")

Data_Sets <- data.frame(dataset_links = Data_Sets) %>% 
  filter(str_detect(dataset_links,"^[[:punct:]]Link"),
         str_detect(dataset_links,"forcedownload"))

Data_Sets <- Data_Sets %>% 
  mutate(dataset_links = paste0("https://www.education.gov.za/Programmes/EMIS/EMISDownloads",
                                dataset_links))
dir.create("Schools_Db")
# Donwload_Files ----------------------------------------------------------
for(i in seq_along(1:nrow(Data_Sets))){
  try(download_xml(url = Data_Sets$dataset_links[[i]],
               file = paste0("./Schools_Db/","file_",i,".xlsx")))
  print(paste0("File ",i, "downloaded ","proceeding to file ",i+1,"."))
}

