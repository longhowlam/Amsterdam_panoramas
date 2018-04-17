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

#### spread over 4 cores on my machine to retrieve image links

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


#f <- futureOf(batch4)

## Now you have one big data frame with links to all panorama fotos
## and the corresponding coordinates

Amsterdam_Panoramas = bind_rows(
  batch1, batch2, batch3, batch4
)

saveRDS(Amsterdam_Panoramas, "Amsterdam_Panoramas.RDs")

### just create a scatter plot of a sample of the data
Amsterdam_Panoramas %>% 
  sample_n(10000) %>% 
  ggplot( aes(x=y, y=x)) +
  geom_point(alpha = 0.01)    





#### google check ####
i = 98
url_gm = sprintf(
  "https://www.google.nl/maps/@%s,%s,19z", 
  Amsterdam_Panoramas[i,]$y,
  Amsterdam_Panoramas[i,]$x
)
url_gm
Amsterdam_Panoramas[i,]$link
