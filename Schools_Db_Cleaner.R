start_time <- Sys.time()
# Libraries ---------------------------------------------------------------
source("./All_Functions.R",
       local = knitr::knit_global())
sivu_is_a_lib()
doParallel::registerDoParallel(cores = 10)
# All_Masterlist ----------------------------------------------------------
EDU_Dbs <- data.frame(master_list = list.files(path = "./Schools_Db",
           full.names=TRUE,
           pattern = ".xlsx"))

EDU_Dbs <- EDU_Dbs %>% 
  mutate(sheet_names = map(master_list,excel_sheets))

EDU_Dbs <- EDU_Dbs %>% 
  unnest(sheet_names)
# Custom Excel ------------------------------------------------------------

extract_excel <- function(a_df,sheets,paths){
  number_of_rows <- 1:nrow(a_df)
  a_df <- data.frame(a_df)
  all_files <- list()
  try(
  for(i in seq_along(number_of_rows)){
    all_files[[i]] <- read_xlsx(path = a_df[i,paste0(paths)],
                                sheet = a_df[i,paste0(sheets)])
    })
  return(all_files)
}

Dbs <- extract_excel(a_df = EDU_Dbs,
                     sheets = "sheet_names",
                     paths = "master_list")


Dbs <- lapply(Dbs,sapply,as.character)
Dbs <- lapply(Dbs,as.data.frame)

Dbs <- do.call(bind_rows,Dbs)

Dbs <- Dbs %>% 
  clean_names()

Dbs <- Dbs[,grep("emis|data|province|exam_centre|exam_no|institution_name|municipality|muni|ward|address|quintile|long|lat|learners|educators",
              colnames(Dbs))]

Dbs <- Dbs %>% 
  unique()
# Clean EMIS Number -------------------------------------------------------
Dbs <- Dbs %>% 
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
Dbs <- Dbs %>% 
  filter(!is.na(exam_no),
         exam_no != "N/A",
         !is.na(exam_centre))
# Clean_Institution_Name ----------------------------------------------------
Dbs <- Dbs %>% 
  mutate(institution_name = ifelse(is.na(institution_name)==TRUE,
                                   institution_name_2,
                                   institution_name),
         institution_name = ifelse(is.na(institution_name)==TRUE,
                                   official_institution_name,
                                   institution_name)) %>% 
  select(!contains("institution_name_2"),
         -official_institution_name)
# Clean_Learners and Educators --------------------------------------------
Dbs <- Dbs %>% 
  pivot_longer(contains("learners"),
               names_to = "learners",
               values_to = "learner_number") %>% 
  filter(!is.na(learner_number)) %>%
  mutate(learners = "learners") %>% 
  unique() %>% 
  pivot_wider(names_from = learners,
              values_from = learner_number) %>% 
  unnest(learners)

Dbs <- Dbs %>% 
  pivot_longer(contains("educators"),
               names_to = "educators",
               values_to = "educator_number") %>% 
  filter(!is.na(educator_number)) %>% 
  mutate(educators = "educators") %>% 
  unique() %>% 
  pivot_wider(names_from = educators,
              values_from = educator_number) %>% 
  unnest(educators)

# Clean Further -----------------------------------------------------------
Dbs <- Dbs[,c("new_natemis","exam_centre","institution_name","exam_no",
           "quintile","ward_id","street_address",
           "new_long","gis_longitude","gis_long","longitude",
           "new_lat","gis_lat","latitude","learners","educators")]

Dbs <- Dbs %>% 
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


Dbs <- Dbs %>% 
  rename(emisno =new_natemis,
         centername=exam_centre,
         centreno = exam_no)

Dbs <- Dbs[,!grepl("long|lat",names(Dbs))]

dir.create("Clean_Db")


arrow::write_parquet(Dbs,
                     paste0("./Clean_Db/Clean_Db_Schools_",Sys.Date(),".parquet"))

end_time <- Sys.time()

list.files(full.names=TRUE,pattern = ".parquet",recursive = TRUE)

print(difftime(time1 = end_time,
               time2= start_time,
               units = "mins"))

list.files(full.names=TRUE,pattern = ".parquet",recursive = TRUE)


rm(list= ls())