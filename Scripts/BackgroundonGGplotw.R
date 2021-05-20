library(extrafont)
library(png)
m <- png::readPNG("DoctorX.co.uk-Sq-BlackOnBlue.png")
m <- readPNG(system.file("img", "DoctorX.co.uk-Sq-BlackOnBlue.png", package="png"))
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.2), nrow=dim(m)[1])
qplot(1:10, rnorm(10), geom = "blank") +
  annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,    
                    rasterGrob(w)) + geom_point()
