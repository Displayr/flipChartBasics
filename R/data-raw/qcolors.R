qColors <- c(grDevices::rgb(91, 155, 213, 255, maxColorValue = 255), # blue
             grDevices::rgb(237, 125, 49, 255, maxColorValue = 255), # orange
             grDevices::rgb(165, 165, 165, 255, maxColorValue = 255), # grey
             grDevices::rgb(30, 192, 0, 255, maxColorValue = 255), # yellow
             grDevices::rgb(68, 114, 196, 255, maxColorValue = 255), # darker blue
             grDevices::rgb(112, 173, 71, 255, maxColorValue = 255), # green
             grDevices::rgb(37, 94, 145, 255, maxColorValue = 255), # even darker blue
             grDevices::rgb(158, 72, 14, 255, maxColorValue = 255), # blood
             grDevices::rgb(99, 99, 99, 255, maxColorValue = 255), # dark grey
             grDevices::rgb(153, 115, 0, 255, maxColorValue = 255), # brown
             grDevices::rgb(38, 68, 120, 255, maxColorValue = 255), # very dark blue
             grDevices::rgb(67, 104, 43, 255, maxColorValue = 255), # darker green
             grDevices::rgb(255, 255, 255, 255, maxColorValue = 255), # black
             grDevices::rgb(255, 35, 35, 255, maxColorValue = 255)) # red

devtools::use_data(qColors, internal = FALSE, overwrite = TRUE)
devtools::use_data(qColors, internal = TRUE, overwrite = TRUE)