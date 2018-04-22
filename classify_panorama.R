library(magick)
library(purrr)
library(stringr)
library(dplyr)
library(keras)
library(reticulate)

## sample per buurt in Amsterdam 400 panorama images en haal deze door 
## een pretrained image model om te zien wat voor plaatjes het zijn

## model kan een pretrqined vgg16 model zijn maar bijvoorbeeld ook een 
## model wat door vendors als api ter beschikking wordt geseteld

#### helper function ##########################################################

## helper functie voor list extractie clarifai output

cf_extract = function(N, outcf){
  tmpf = function(i,outcf)
  {
    outcf[["outputs"]][[1]][["data"]][["concepts"]][[i]][["name"]]
  }
  tmpf2 = function(i,outcf)
  {
    outcf[["outputs"]][[1]][["data"]][["concepts"]][[i]][["value"]]
  }
  entities = map_chr(1:N, tmpf, outcf = outcf)
  scores = map_dbl(1:N, tmpf2, outcf = outcf)
  tibble(entities, scores)
}

get_clarifai_classes = function(link)
{
  outcf = cf_model$predict_by_url(link, lang = "nl")
  out_dataset = cf_extract(10, outcf = outcf)
  out_dataset$link = link
  out_dataset
}


#### clarifai setup ############################################################

cf = import("clarifai")
cf_app = cf$rest$ClarifaiApp()
cf_model = cf_app$models$get("general-v1.3")


classify_buurtpanoramas = function(bunaam, N_panos = 300, pb=NULL)
{
  if (!is.null(pb)) pb$tick()$print()
  links  = Amsterdam2 %>% 
    filter(BU_NAAM == bunaam ) %>% 
    sample_n(N_panos) %>% 
    select(link) %>% .$link
  
  ### using clarifai app
  out = map_df(links, get_clarifai_classes )  
  out$BU_NAAM = bunaam
  out
}



######### loop nu over de buurten die we hebben ################################
tmp = table(Amsterdam2$BU_NAAM)
buurten = names(tmp[tmp >0])

#Amsterdam_Clarifai_classes = tibble::tibble()

for(b in buurten[80:96])
{
  out = classify_buurtpanoramas(b)
  Amsterdam_Clarifai_classes = bind_rows(Amsterdam_Clarifai_classes,out)
  saveRDS(Amsterdam_Clarifai_classes, "Amsterdam_Clarifai_classes.RDs")
  print(b)
}



clf_classes = Amsterdam_Clarifai_classes %>% group_by(entities) %>% summarise(n=n())
clf_buurt_classes = Amsterdam_Clarifai_classes %>% group_by(BU_NAAM) %>% summarise(n=n())


Amsterdam_Clarifai_classes2 = Amsterdam_Clarifai_classes %>% 
  left_join(Amsterdam_Panoramas)


