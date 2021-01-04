library(magick)
library(tidyverse)

# unzip("CardRec/AllCard.zip", exdir = "CardRec")


white <- image_read("CardRec/AllCard/white.png") %>%
  image_resize("64x64!")


image_info(white)


imgs <- map(list.files("CardRec/AllCard/PNG", full.names = TRUE), image_read)
names(imgs) <- list.files("CardRec/AllCard/PNG", full.names = FALSE) %>% gsub(".png", "", .) 

cnums <- imgs %>%
  map(image_crop, geometry_area(85, 225,20, 50)) 



#Create Directory for Images
#map(names(cnums), function(x)dir.create(paste("AllCard/", x, sep = "")))


# map(cnums, image_ggplot)


###Write all the pictures

map(1:100, function(y){
  map(names(cnums), function(cc){

    ccs <- cnums[[cc]] %>% 
      image_rotate(runif(1, 0, 90) + 270* rbinom(1, 1, .5)) %>%
      image_resize(paste(round(runif(2, 10, 45)), collapse ="x"))
    
    image_composite(white, ccs, gravity =
                      sample(c("southwest", "southeast", "northwest", "northeast", "center", "north", "south", "east", "west"), 1)) %>%
      image_convert(colorspace = "gray")  %>%
      map(image_convert, colorspace = "rgb") %>%
      map(convert_img_to_array) %>%
      image_write(paste("CardRec/AllCard/", cc, "/", cc, "_",y, ".png", sep = ""))
  })
})


