

#' Title
#'
#' @param text
#' @param font a google font to download. default = "Gravitas One"
#'
#' @return
#' @export
#'
#' @examples
seed_text<-function(text, font, ch, xshift, yshift, id ){
  #set font
  if(missing(font)){font<-"Gravitas One"}
  #download font
  font_add_google(name=font)



}

sep <- 0.5

#msg letters
msg2 <- bind_rows(
  get_letter("O", 1.1, sep, 1),
  get_letter("z", 2.9, sep, 2),
  get_letter("U", 4.5, sep, 3),
  get_letter("n", 6.1, sep, 4),
  get_letter("c", 7.9, sep, 5),
  get_letter("o", 9.4, sep, 6),
  get_letter("n", 11, sep, 7),
  get_letter("f", 12.8, sep, 8),
  get_letter("1", 14.4, sep, 9),
  get_letter("9", 15.3, sep, 10))




#function to get polygons with spacing
get_letter <- function(ch, xshift, yshift, id) {
  char_poly <-  fontr::glyph_polygon(
    ch, family = font,
    face = "regular", nseg = 100)
  char_poly$x <- (char_poly$x * 1.9) + xshift
  char_poly$y <- (char_poly$y * 1.9) + yshift
  char_poly$id <- id
  return(char_poly)
}


#trialing
text<-"testme"
chrs<-strsplit(text, "")
char<-data.frame(x=NULL,y=NULL,id=NULL)

for(i in 1:length(chrs[[1]])){
  if(i==1) {xshift<-0.1} else(xshift<-max(char$x+sep))
  char<-rbind(char, get_letter(chrs[[1]][i], xshift, sep, chrs[[1]][i]))

  }

plot(char$x[char$id=="m"],char$y[char$id=="m"])
