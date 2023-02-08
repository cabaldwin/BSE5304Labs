if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
system("git config --global user.email 'drfuka@vt.edu' ") 
system("git config --global user.name 'Daniel Fuka' ")

# Get some knowledge of your work environment
Sys.getenv()
Sys.getenv("HOME")
Sys.getenv("WORKON_HOME")
mygitbasedir=Sys.getenv("WORKON_HOME")
myhomedir=Sys.getenv("HOME")
# We will explore this deeper when we get onto the VT ARC Next Week
# Note you should never store your authentication keys like this but... 


pacman::p_load(lubridate,rnoaa,ggplot2,moments,operators,topmodel,DEoptim,XML,data.table,RSQLite,argparse,stringi,stringr,sqldf,readr,rgeos,rgdal,sf,readr,tools,diffobj,png,grid,gridExtra,purrr,raster)
pacman::p_install_gh("USGS-R/ncdfgeom")
pacman::p_load(operators,topmodel,DEoptim)
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

# I am very proud of myself for making a model that can 
# actually be used by others.
data <- data.frame(
  day = as.Date("2019-01-01") + 0:99,
  tmin = runif(100) + seq(1,100)^2.5 / 10000,
  price = runif(100) + seq(100,1)^1.5 / 10
)
#new change
data$tmax = runif(100)*10  + data$tmin
coeff <- 6
# A few constants
tminColor <- "#0000fe"
  tmaxColor <- "#ff0000"
    priceColor <- rgb(0.2, 0.6, 0.9, 1)
    dir.create("pdfs")
    basestr=format(Sys.time(),"./pdfs/%Y%m%d%H%M")
    
    p1= ggplot(data, aes(x=day)) +
      geom_line( aes(y=tmin), linewidth=2, color=tminColor) + 
      geom_line( aes(y=tmax), linewidth=2, color=tmaxColor) + 
      geom_line( aes(y=price / coeff), linewidth=2, color=priceColor) +
      scale_y_continuous(
        # Features of the first axis
        name = "Temp(C)",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Price")
      ) + 
      # theme_ipsum() +
      theme(
        axis.title.y = element_text(color = "black", size=13),
        axis.title.y.right = element_text(color = priceColor, size=13)
      ) +
      ggtitle("Temperature down, price up")
    
    filename=paste0(basestr,"graph01.pdf")
    pdf(filename) 
    plot(p1)
    dev.off()
    print("file size")
    print(file.size(filename))
    print("I finished!")
    