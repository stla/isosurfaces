command <- "mogrify -format png *.ppm"
system(command)

# command <- "convert -delay 1x16 -layers Optimize -fuzz 4% *.png anim.gif"
# system(command)

file.remove(Sys.glob("*.ppm"))

library(gifski)
pngFiles <- Sys.glob("*.png")#[1:90]
gifski(
  png_files = pngFiles,
  gif_file  = "anim.gif",
  width = 512, height = 512,
  delay = 1/16
)

file.remove(pngFiles)

# command <- "mogrify -layers 'optimize-plus' -fuzz 4% anim.gif"
# system(command)

# command <- "gifsicle -O3 --colors 256 < anim.gif > anim2.gif"
# system(command)
# 
# command <- "gifsicle --colors 256 --delay=6 --loop *.gif > anim2.gif"
# system(command)

