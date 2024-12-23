library(aRt)

camcorder::gg_record(
  dir = file.path("recording"),
  device = "png",
  width = 4,
  height = 4,
  units = "in",
  dpi = 300
)

for (i in 1:20) {
  print(spirals(s = i, max_size = 22-i))
  camcorder::record_polaroid()
}
for (i in 19:2) {
  print(spirals(s = i, max_size = 22-i))
  camcorder::record_polaroid()
}

camcorder::gg_playback(
  name = "spirals.gif",
  first_image_duration = 0.25,
  last_image_duration = 0.25,
  frame_duration = 0.25,
  background = "black",
  last_as_first = FALSE
)
