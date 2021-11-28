
# AC Efficiency
# Gathering the data

# Packages
library(magick)
library(rsvg)

# Magick Tiger Test 
tiger <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 600)
print(tiger)


# Read 1 Image
image1 <- magick::image_read("C:/data/ACEfficiency-001/AC Efficiency/PXL_20210604_144936993.jpg")
print(image1)

# Process Image 1 for Testing



# Bring in All Images
files <- list.files() #create a vector with file names


for(i in 1:length(files)){#loop over file names
  
  load(files[i]) #load .rda-file
  
  #do some processing and save results
  
}




