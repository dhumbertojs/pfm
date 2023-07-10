library(httr)
library(purrr)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)

# Datos -------------------------------------------------------------------

url <- c(
  "https://datosabiertos.infocdmx.org.mx/dataset/6dda3a6f-4e2a-484c-9613-89899058f70d/resource/9c683caf-f559-462c-8e70-845c8902d7b4/download/_limpia_2022.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/2e770767-dfe5-4ba2-9af3-61338cc6509b/resource/43f4549c-9cba-4737-b86b-f2ee4924ddf8/download/limpia_2021.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/f8a7fab6-8466-4e1c-b3fb-b0d29afd2f83/resource/2a29d720-32bc-4268-90da-3b374aaa4379/download/limpia_2020.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/f92cb601-9b13-4e58-9556-9810c5347e2b/resource/d7019dab-918c-41e9-ad6d-9fb9838c5021/download/limpia_2019.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/29faea7e-4d34-4f5d-bdd4-50b61ace1f32/resource/074b6c4e-89fd-4119-a43a-69b1e007e1b3/download/limpia_2018.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/e42ccdad-f383-485b-802f-9aba520df083/resource/e8b2d6d6-c4cf-4a49-ac2b-512ab3597283/download/limpia_2017.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/017c49f8-39f7-4942-9408-728046586bc8/resource/5c68a7a5-be2d-4a4f-ad04-ff04a9ed2889/download/limpia_2016.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/bdadb1ab-e476-4c47-95fb-4a2d4af9e2de/resource/540d06f9-b9a2-4f56-b23d-1e84e2ed85a2/download/limpia_2015.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/e905654e-6a6a-4d0d-912f-17b5270aabf2/resource/d68925b2-fcd3-4676-b2c5-d73c3ac69d9a/download/limpia_2014.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/69b31de6-3a7b-4892-ac2e-79c7043616b4/resource/52c81f47-214b-4888-9869-0b17b781ebb9/download/limpia_2013.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/850fb128-2643-40b5-bbb4-f7a378494f90/resource/2bef2739-b570-4c80-afb2-e8ccb59e8f2c/download/limpia_2012.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/f4842322-3068-475f-b8c0-9a65b0a011e6/resource/de055dd6-2eb2-4d75-adb9-2041b47fdb4e/download/limpia_2011.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/7bdd86c0-93b9-4dd6-be66-a39ec814be1c/resource/5043bd4d-07bf-40db-a9df-8f2755aa36bc/download/limpia_2010.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/4ef8d70d-21c9-427b-95c2-8c006f4fec16/resource/132a3785-c718-4122-96df-031437d2f289/download/limpia_2009.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/1422cec6-f8e9-4715-bc49-ea66afff640a/resource/a27a3b30-b727-4650-896d-1258729e13ba/download/limpia_2008.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/bad94346-71e9-438d-b026-689d228e2d33/resource/2eb8f23e-a098-4746-80ce-e52562bd1f11/download/limpia_2007.xlsx",
  "https://datosabiertos.infocdmx.org.mx/dataset/17b24af2-4950-4476-91a1-d934fd4d7c68/resource/e0d70ebb-7c16-46c7-8865-c537df8ff637/download/limpia_2006.xlsx"
)

temp_files <- map(url, ~ {
  temp_file <- tempfile()
  GET(.x, write_disk(temp_file))
  temp_file
})

lista <- map(temp_files, ~ {
  read_excel(.x) %>% 
    clean_names() %>% 
    mutate_all(as.character)
})

lista <- bind_rows(lista)
map(temp_files, unlink)

summary(lista)

write.csv(lista, "./data/raw_data.csv", row.names = F)