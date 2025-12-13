

library(patchwork)
library(magick)

img1 <- image_read("viz/placement_error_1.png")
img2 <- image_read("viz/placement_error_2.png")

## side by side
combined <- image_append(c(img1, img2), stack = FALSE)

# # stack vertically
# combined <- image_append(c(img1, img2), stack = TRUE)

## display
print(combined)

## save
image_write(combined, "viz/combined_images_horizontal.png")
