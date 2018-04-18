library(magick)
library(purrr)
library(stringr)

## sample per buurt in Amsterdam 25 panorama images, download ze en grbuik magick om
## deze in een animated gif te zettenn. Deze kunnen we dangebruiken op een leaflet

createbuurtanimatie = function(bunaam)
{

  links  = Amsterdam2 %>% 
    filter(BU_NAAM == bunaam ) %>% 
    sample_n(25) %>% 
    select(link) %>% .$link
  
  
  download.file(links[1], "image.jpg", mode = "wb")
  imagelist = image_read("image.jpg")
  
  for ( link in links )
  {
    tryCatch({
      download.file(link, "image.jpg", mode = "wb")
      imagelist = c(imagelist,image_read("image.jpg"))
    },
    error=function(e) NA
    )
  }
  
  animation = image_animate(imagelist, fps = 5)
  
  fn = paste0(str_replace_all(bunaam,"\\s","_"), ".gif")
  fn = fn %>% str_replace_all("/", "@")
  image_write(animation, fn)
}


tmp = table(Amsterdam2$BU_NAAM)
buurten = names(tmp[tmp >0])

map(buurten, createbuurtanimatie)
