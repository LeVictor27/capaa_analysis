
#palette_uw <- colorRampPalette(c("#4B2E83", "#E8E3D3"))
#palette_uw_reverse<- colorRampPalette(c("#E8E3D3", "#4B2E83"))
#palette_black_white <- colorRampPalette(c("#FFFFFF", "#000000"))

theme_uw <- function(){ 
  #Adapted from Maddie Picken's RPubs/RStudio Tutorial
  #https://rpubs.com/mclaire19/ggplot2-custom-themes
  
  font <- "Arial"   #assign font family up front
  theme_bw() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks = element_blank(),          #strip axis ticks
      
      #text elements
      plot.title = element_text(             #title
        family = font,           
        size = 16,                
        #face = 'bold',           
        margin=margin(5, b = 10),  #Margins around title
        hjust = 0.5),                #Center align
                       
      
      plot.subtitle = element_text(          #subtitle
        family = font,            
        size = 14
        margin=margin(5, b = 10),  #Margins around title
        ),               
      
      plot.caption = element_text(           #caption
        family = font,          
        size = 9,              
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,           
        size = 10),             
      
      axis.text = element_text(              #axis text
        family = font,         
        size = 9),               
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}
