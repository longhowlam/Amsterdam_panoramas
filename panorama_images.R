library(httr)
library(purrr)
library(future)
library(dplyr)
library(stringr)
library(ggplot2)

################ helper extraction functions #############################################

IMG_url = function(i, outdata){
  outdata[["results"]][[i]][["image_sets"]][["thumbnail"]]
}

IMG_loc = function(i, outdata)
{
  x = outdata[["results"]][[i]][["geometrie"]][["coordinates"]][[1]]
  y = outdata[["results"]][[i]][["geometrie"]][["coordinates"]][[2]]
  z = outdata[["results"]][[i]][["geometrie"]][["coordinates"]][[3]]
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

    panorame_link = map_chr(1:N, IMG_url, outdata)
    panorame_data = map_df(1:N, IMG_loc, outdata)
    panorame_data$link = panorame_link
    panorame_data$page = page
    return(panorame_data)
  }
  else{
    return(data.frame( x = NULL, y=NULL, z=NULL, link=NULL, page = NULL))
  }
}


tmp = singlepage(1)

plot(tmp$y,tmp$x)
##### spread over four cores to speed up scraping ######
# estimated there are ~2200 pages





plan(multicore)

batch1 %<-% {
   map_df(1:750, singlepage)
}


batch2 %<-% {
   map_df(751:1500, singlepage)
}

batch3 %<-% {
  map_df(1501:2250, singlepage)
}

batch4 %<-% {
  map_df(2251:3000, singlepage)
}


f <- futureOf(batch4)

tt = batch4 %>% group_by(page) %>%  summarise(n=n())


Amsterdam_Panoramas = bind_rows(
  batch1, batch2, batch3, batch4
)

saveRDS(Amsterdam_Panoramas, "Amsterdam_Panoramas.RDs")


Amsterdam_Panoramas[440000,]

## google check
url_gm = sprintf(
  "https://www.google.nl/maps/@%s,%s,19z", 
  Amsterdam_Panoramas[400000,]$y,
  Amsterdam_Panoramas[400000,]$x
)


ggplot(Amsterdam_Panoramas, aes(x=y, y=x)) + geom_point(alpha = 0.1)    


url_gm
Amsterdam_Panoramas[40000,]$link




https://api.data.amsterdam.nl/panorama/recente_opnames/2016/?page=200&page_size=200
