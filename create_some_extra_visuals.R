library(wordcloud2)
library(dplyr)
library(stringr)
library(leaflet)


clf_classes = Amsterdam_Clarifai_classes %>%
  group_by(entities) %>% 
  summarise(n=log10(n()))


clf_classes2 = Amsterdam_Clarifai_classes %>%
  group_by(entities) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice(1:50) %>% 
  arrange(entities)

wordcloud2(clf_classes, shape = "star")
wordcloud2(clf_classes, size = .51,shape = 'pentagon')



Amsterdam_boom_perc = Amsterdam_Clarifai_classes %>% 
  mutate(
    boom = str_detect(entities, "boom" )
  ) %>% 
  group_by(BU_NAAM) %>% 
  summarise(n=n(), boom = mean(boom))


AmsterdamWijken3 = AmsterdamWijken

pp = left_join(AmsterdamWijken3@data, Amsterdam_boom_perc, by = c("BU_NAAM" = "BU_NAAM"))
AmsterdamWijken3@data = pp


polpopup = paste0(
  "<b>",
  AmsterdamWijken3$BU_NAAM, " boom percentage ", round(100*AmsterdamWijken3$boom,1), "%", 
  "</b>",
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
    color = ~colorQuantile(n = 9, "Greens", AmsterdamWijken3$boom)(boom),
    data = AmsterdamWijken3,
    popup = polpopup )  
m
