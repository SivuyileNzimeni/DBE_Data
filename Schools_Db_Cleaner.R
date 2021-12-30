start_time <- Sys.time()
# Libraries ---------------------------------------------------------------
lapply(as.list(c("tidyverse","janitor","readxl",
                 "writexl")),require,character.only=TRUE)
# All_Excel_Reader --------------------------------------------------------
all_excel <- function(path){
  collect_sheets <- excel_sheets(path)
  number_of_sheets <- 1:length(collect_sheets)
  per_sheet <- list()
  for(i in seq_along(number_of_sheets)){
    per_sheet[[i]] <- read_xlsx(path = path,
                                sheet = collect_sheets[i])
  }
  return(per_sheet)}
# All_Masterlist ----------------------------------------------------------
EDU_Dbs <- data.frame(master_list = list.files(path = "./Schools_Db",
           full.names=TRUE,
           pattern = ".xlsx")) %>% 
  mutate(schools_db = map(master_list,all_excel))

EDU_Dbs <- EDU_Dbs %>% 
  unnest(schools_db)

EDU_Dbs$schools_db <- lapply(EDU_Dbs$schools_db,sapply,as.character)

EDU_Dbs$schools_db <- lapply(EDU_Dbs$schools_db,as.data.frame)

EDU_Dbs <- EDU_Dbs %>% 
  unnest(schools_db)

EDU_Dbs <- EDU_Dbs %>% 
  clean_names()
EDU_Dbs <- EDU_Dbs %>% 
  select(contains("emis"),
         contains("data"),
         contains("province"),
         contains("exam_centre"),
         contains("exam_no"),
         contains("institution_name"),
         contains("municipality"),
         contains("muni"),
         contains("ward"),
         contains("address"),
         contains("quintile"),
         contains("long"),
         contains("lat"))

EDU_Dbs <- EDU_Dbs %>% 
  unique()
# Clean EMIS Number -------------------------------------------------------
EDU_Dbs <- EDU_Dbs %>% 
  mutate(datayear = ifelse(is.na(datayear)==TRUE,
                           data_year,
                           datayear)) %>% 
  select(-data_year) %>% 
  mutate(new_natemis = ifelse(is.na(new_natemis)==TRUE,
                              old_natemis,
                              new_natemis),
         new_natemis = ifelse(is.na(new_natemis)==TRUE,
                              emis_no,
                              new_natemis),
         new_natemis = ifelse(is.na(new_natemis)==TRUE,
                              neimsemisn,
                              new_natemis),
         new_natemis = ifelse(is.na(new_natemis)==TRUE,
                              main_natemis,
                              new_natemis),
         new_natemis = ifelse(is.na(new_natemis)==TRUE,
                              nat_emis_2,
                              new_natemis),
         new_natemis = ifelse(is.na(new_natemis)==TRUE,
                              nat_emis,
                              new_natemis)) %>% 
  select(-c(old_natemis,emis_no,neimsemisn,main_natemis,
            nat_emis_2,nat_emis)) %>% 
  unique()
# Clean_Exams --------------------------------------------------
EDU_Dbs <- EDU_Dbs %>% 
  filter(!is.na(exam_no),
         exam_no != "N/A",
         !is.na(exam_centre))
# Clean_Institution_Name ----------------------------------------------------
EDU_Dbs <- EDU_Dbs %>% 
  mutate(institution_name = ifelse(is.na(institution_name)==TRUE,
                                   institution_name_2,
                                   institution_name),
         institution_name = ifelse(is.na(institution_name)==TRUE,
                                   official_institution_name,
                                   institution_name)) %>% 
  select(!contains("institution_name_2"),
         -official_institution_name)
# Clean Further -----------------------------------------------------------
EDU_Dbs <- EDU_Dbs[,c("new_natemis","exam_centre","institution_name","exam_no",
           "quintile","ward_id","street_address",
           "new_long","gis_longitude","gis_long","longitude",
           "new_lat","gis_lat","latitude")]

EDU_Dbs <- EDU_Dbs %>% 
  mutate(new_long = ifelse(is.na(new_long)==TRUE,
                           gis_longitude,
                           new_long),
         new_long = ifelse(is.na(new_long)==TRUE,
                           gis_long,
                           new_long),
         new_long = ifelse(is.na(new_long)==TRUE,
                           longitude,
                           new_long),
         new_lat = ifelse(is.na(new_lat)==TRUE,
                          gis_lat,
                          new_lat),
         new_lat = ifelse(is.na(new_lat)==TRUE,
                          latitude,
                          new_lat)) %>% 
  select(-c(gis_longitude,gis_long,longitude,
            gis_lat,latitude)) %>% 
  unique() %>% 
  filter(exam_no !="NULL") %>% 
  unique()


EDU_Dbs <- EDU_Dbs %>% 
  rename(emisno =new_natemis,
         centername=exam_centre,
         centreno = exam_no)

dir.create("Clean_Db")

write_csv(EDU_Dbs,
          file = paste0("./Clean_Db/Clean_Db_Schools_",Sys.Date(),".csv"))

end_time <- Sys.time()

print(difftime(time1 = end_time,
               time2= start_time,
               units = "mins"))
rm(list= ls())
