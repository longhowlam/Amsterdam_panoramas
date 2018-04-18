library(sp)
library(dplyr)
library(leaflet)

# De Amsterdam_panoramaset is een data set met punten
# we kunnen aggregeren op wijk nivo. We hebben buurt shape files van CBS
# En van elk punt kunnen we bepalen tot welek buurt in Amsterdam het hoort

#### Amsterdam wijken ####
AmsterdamWijken <- readRDS("AmsterdamWijken.Rds")
plot(AmsterdamWijken)

# Maak van Amsterdam_panoramas een sptialpoint dataframe
Amsterdam = Amsterdam_Panoramas
coordinates(Amsterdam) = ~ x + y
proj4string(Amsterdam) = CRS("+proj=longlat +datum=WGS84")

## bepaal nu tot welke buurt een punt hoort
buurtinfo = over(Amsterdam, AmsterdamWijken)

## Zet alles in een dataframe
Amsterdam2 = bind_cols(
  Amsterdam@data,
  Amsterdam@coords %>% as.data.frame,
  buurtinfo %>% select(BU_NAAM, AANT_INW)
  )

### we kunnen nu per BU_code aggregeren, kijken hoeveel plaatjes
### er per buurt zijn gemaakt

Amsterdam_buurt = Amsterdam2 %>% 
  group_by(BU_NAAM) %>% 
  summarise(
    aantal_pano = n(),
    eenplaatje = max(link)
  ) %>% 
  filter(!is.na(BU_NAAM))


## voeg weer de uitgerekende data terug in amsterdamwijken
pp = left_join(AmsterdamWijken@data, Amsterdam_buurt, by = c("BU_NAAM" = "BU_NAAM"))
AmsterdamWijken@data = pp


##### zet op een interactief kaartje ##############
polpopup = paste(
  AmsterdamWijken$BU_NAAM, 
  "aantal panos ", as.character(AmsterdamWijken$aantal_pano), "<br/>",
  "<img border='0' src='",
  AmsterdamWijken$eenplaatje,
  "' width='325' height='200'>"
)

m2 = leaflet()
m2 = m2 %>%
  addTiles() %>%
  addPolygons(
    weight=2,
    smoothFactor = 0.2, fillOpacity = 0.55, 
    color = ~colorQuantile(n = 9, "YlOrRd", AmsterdamWijken$aantal_pano)(aantal_pano),
    data = AmsterdamWijken,
    popup = polpopup )  
m2


################ nu ook met animated gifs
tt = AmsterdamWijken$BU_NAAM
fn = paste0(str_replace_all(tt,"\\s","_"), ".gif")
fn = fn %>% str_replace_all("/", "@")

polpopup = paste0(
  AmsterdamWijken$BU_NAAM, 
  "<br/>",
  "<img border='0' src='http://www.lhldsd.nl/amsterdam/",
  fn,
  "' width='325' height='200'>"
)

m = leaflet()
m = m %>%
  addTiles() %>%
  addPolygons(
    weight=2,
    smoothFactor = 0.2, fillOpacity = 0.55, 
    color = ~colorQuantile(n = 9, "YlOrRd", AmsterdamWijken$aantal_pano)(aantal_pano),
    data = AmsterdamWijken,
    popup = polpopup )  
m

