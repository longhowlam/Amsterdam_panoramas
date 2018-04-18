library(magick)
library(purrr)
library(stringr)
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


#### clarifai setup ########################################################

cf = import("clarifai")
cf_app = cf$rest$ClarifaiApp()
cf_model = cf_app$models$get("general-v1.3")


classify_buurtpanoramas = function(bunaam, N_panos = 400)
{
  
  links  = Amsterdam2 %>% 
    filter(BU_NAAM == bunaam ) %>% 
    sample_n(N_panos) %>% 
    select(link) %>% .$link
  
  ### using clarifai app
  out = map_df(links, get_clarifai_classes )  
  out$BU_NAAM = bunaam
  out
}



######### loop nu over de buurten die we hebben
tmp = table(Amsterdam2$BU_NAAM)
buurten = names(tmp[tmp >0])