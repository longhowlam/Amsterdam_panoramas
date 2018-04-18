# download all the images, just by looping through the more than 540K links
# we can divide the load on multiple cores by using the future packahe

N = dim(Amsterdam_Panoramas)[1]
Amsterdam_Panoramas$id = paste0("image",1:N,".jpg")

library(future)
plan(multicore)

a1 %<-% {
  for (i in 1:25000)
  {
   download.file(
      Amsterdam_Panoramas$link[i],
      paste0("amsterdam/", Amsterdam_Panoramas$id[i]),
      mode = "wb"
    )
  }
}

a2 %<-% {
  for (i in 25001:50000)
  {
    download.file(
      Amsterdam_Panoramas$link[i],
      paste0("amsterdam/", Amsterdam_Panoramas$id[i]),
      mode = "wb"
    )
  }
}


a3 %<-% {
  for (i in 50001:100000)
  {
    if(!file.exists(paste0("amsterdam/", Amsterdam_Panoramas$id[i]))){
      download.file(
        Amsterdam_Panoramas$link[i],
        paste0("amsterdam/", Amsterdam_Panoramas$id[i]),
        mode = "wb"
      )
    }
  }
}


a4 %<-% {
  for (i in 100001:150000)
  {
    if(!file.exists(paste0("amsterdam/", Amsterdam_Panoramas$id[i]))){
      print(i)
      download.file(
        Amsterdam_Panoramas$link[i],
        paste0("amsterdam/", Amsterdam_Panoramas$id[i]),
        mode = "wb"
      )
    }
  }
}



a5 %<-% {
  for (i in 150001:200000)
  {
    download.file(
      Amsterdam_Panoramas$link[i],
      paste0("amsterdam/", Amsterdam_Panoramas$id[i]),
      mode = "wb"
    )
  }
}



a6 %<-% {
  for (i in 200001:250000)
  {
    if(!file.exists(paste0("amsterdam/", Amsterdam_Panoramas$id[i]))){
      print(i)
      download.file(
        Amsterdam_Panoramas$link[i],
        paste0("amsterdam/", Amsterdam_Panoramas$id[i]),
        mode = "wb"
      )
    }
  }
}


futureOf(a6)
