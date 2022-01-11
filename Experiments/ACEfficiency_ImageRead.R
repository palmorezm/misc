
# AC Efficiency
# Gathering the data

# Packages
library(magick)
library(rsvg)
library(tesseract)
library(dplyr)

# Magick Tiger Test 
tiger <- image_read_svg('http://jeroen.github.io/images/tiger.svg', width = 600)
print(tiger)


# Read 1 Image
image1 <- magick::image_read("C:/data/ACEfficiency-001/AC Efficiency/PXL_20210604_144936993.jpg")
image2 <- magick::image_read("C:/data/ACEfficiency-001/AC Efficiency/PXL_20211007_023036680.jpg")
image3 <- magick::image_read("C:/data/ACEfficiency-001/AC Efficiency/PXL_20211001_020444229.jpg")
image4 <- magick::image_read("C:/data/ACEfficiency-001/AC Efficiency/PXL_20211001_020502199.jpg")
image5 <- magick::image_read("C:/data/ACEfficiency-001/AC Efficiency/PXL_20211001_020928050.jpg")
print(image1)
print(image2)
print(image3)
print(image4)
print(image5)

# Preprocess Image 1 for Testing


# Extraction of Text
eng <- tesseract("eng")
text <- tesseract::ocr(image4, engine = eng)
cat(text)

# We are looking specifically for the specified number; 
# maybe make black and white first?
nums <- tesseract(options = list(tessedit_char_whitelist = ".0123456789"))
text <- tesseract::ocr(image1, engine = nums) 
cat(text)

# Bring in All Images
files <- list.files() #create a vector with file names


for(i in 1:length(files)){#loop over file names
  
  load(files[i]) #load .rda-file
  
  #do some processing and save results
  
}

# Numeric Notes
# tesseract(options = list(tessedit_char_whitelist = ".0123456789"))
# Looks specifically for the characters "0-9" and the "." symbol
# 

