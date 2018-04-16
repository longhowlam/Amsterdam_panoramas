library(httr)
library(purrr)
library(future)
library(dplyr)

################ helper extraction functions #############################################

IMG_url = function(x){
  outdata[["results"]][[x]][["image_sets"]][["thumbnail"]]
}

IMG_loc = function(x)
{
  x = outdata[["results"]][[x]][["geometrie"]][["coordinates"]][[1]]
  y = outdata[["results"]][[x]][["geometrie"]][["coordinates"]][[2]]
  z = outdata[["results"]][[x]][["geometrie"]][["coordinates"]][[3]]
  tibble::tibble(x,y,z)
}


############### scrape images links first ###########################################

#### single page #######

singlepage = function(page, pb=NULL)
{
  if (!is.null(pb)) pb$tick()$print()
  
  URI = paste0(
    "https://api.data.amsterdam.nl/panorama/recente_opnames/2016/?page=",
    page,
    "&page_size=200"
  )

  out = GET(url = URI)
  if(out$status_code == 200){
  
    outdata = content(out)
    N = length(outdata$results)

    panorame_link = map_chr(1:N, IMG_url)
    panorame_data = map_df(1:N, IMG_loc)
    panorame_data$link = panorame_link
    panorame_data$page = page
    return(panorame_data)
  }
  else{
    return(data.frame( x = NULL, y=NULL, z=NULL, link=NULL, page = NULL))
  }
}

##### spread over four cores to speed up scraping ######
# estimated there are ~2200 pages


plan(multicore)

batch1 %<-% {
   map_df(1:500, singlepage)
}


batch2 %<-% {
   map_df(501:1000, singlepage)
}

batch3 %<-% {
  map_df(1001:1500, singlepage)
}

batch4 %<-% {
  map_df(1501:2000, singlepage)
}

batch5 %<-% {
  map_df(2001:2200, singlepage)
}

batch5 = .Last.value

f <- futureOf(batch1)

tt = batch5 %>% group_by(page) %>%  summarise(n=n())


Amsterdam_Panoramas = bind_rows(
  batch1, batch2, batch3, batch4, batch5
)

saveRDS(Amsterdam_Panoramas, "Amsterdam_Panoramas.RDs")


Amsterdam_Panoramas[440000,]
