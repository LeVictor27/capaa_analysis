palette_uw <- colorRampPalette(c("#4B2E83", "#E8E3D3"))
palette_uw_reverse<- colorRampPalette(c("#E8E3D3", "#4B2E83"))
palette_black_white <- colorRampPalette(c("#FFFFFF", "#000000"))

theme_uw <- function(){ 
  #Built off of Maddie Picken's RPubs/RStudio Tutorial
  #https://rpubs.com/mclaire19/ggplot2-custom-themes
  
  font <- "Arial"   #assign font family up front
  theme_bw() %+replace%    #replace elements we want to change
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.background=element_blank(),
      panel.grid.major = element_blank(),    
      panel.grid.minor = element_blank(),    
      panel.grid.major.x = element_line(color = "#D3D3D3"), 
      legend.title = element_blank(), 
      #Removing legend title since it usually defaults to variable name,
      #manually define it if you need it
      
      plot.title = element_text(           
        family = font,           
        size = 16,                
        #face = 'bold',           
        margin=margin(5, b = 10),  
        hjust = 0.5),                #Center align
                       
      plot.subtitle = element_text(         
        family = font,  
        size = 10,
        margin=margin(5, b = 10),  
        ),               
      
      plot.caption = element_text(          
        family = font,
        face="italic",
        size = 9,              
        hjust = 0,  #left justified
        margin=margin(5, b = 10)),               
      
      axis.title = element_text(             
        family = font,           
        size = 10),             
      
      axis.text = element_text(             
        family = font,         
        size = 9),               
      
      axis.text.x = element_text(           
        margin=margin(5, b = 10))
    )
}

theme_uw_map <- function(){ 
  #Built off of Maddie Picken's RPubs/RStudio Tutorial
  #https://rpubs.com/mclaire19/ggplot2-custom-themes
  
  font <- "Arial" 
  theme_void() %+replace%    #replace elements we want to change
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.background=element_blank(),
      panel.grid.major = element_blank(),    
      panel.grid.minor = element_blank(),    
      axis.ticks = element_blank(),         
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      rect = element_blank(),
      legend.title = element_blank(), 
      #Removing legend title since it usually defaults to variable name,
      #manually define it if you need it
      
      plot.title = element_text(         
        family = font,           
        size = 16,                
        #face = 'bold',           
        margin=margin(5, b = 10),  #Margins around title
        hjust = 0.5),                #Center align
      
      plot.subtitle = element_text(         
        family = font,  
        size = 10,
        margin=margin(5, b = 10),  #Margins around title
      ),               
      
      plot.caption = element_text(          
        family = font,
        face="italic",
        size = 9,              
        hjust = 0,  #left justified
        margin=margin(5, b = 10)),               
      
      axis.title = element_text(             
        family = font,           
        size = 10)
      

    )
}
