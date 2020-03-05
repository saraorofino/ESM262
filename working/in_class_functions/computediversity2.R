#' Describe diversity based on a list of species 
#' 
#' Compute a species diversity index
#' @param species list of species (names, or code) 
#' @return list with the following items
#' \describe{
#' \item{num}{ Number of distinct species}
#' \item{simpson}{Value of simpson diversity index}
#' \item{dominant}{Name of the most frequently occuring species}
#' }
#' @examples
#' computediversity(c("butterfly","butterfly","mosquito","butterfly","ladybug","ladybug")))
#' @references
#' http://www.tiem.utk.edu/~gross/bioed/bealsmodules/simpsonDI.html

computediversity = function(species, index="A", showplot=FALSE) {

species = as.factor(species)

# decide which index calculation to use
if (index=="B")
  diversity=compute_simpson_indexB(species)
else
  diversity =compute_simpson_index(species)

sm = as.data.frame(summary(species))
colnames(sm)="frequency"
sm$fish = rownames(sm)

if (showplot) 
  p=ggplot(sm,aes(x=fish,y=frequency, fill=fish))+geom_col()
else
  p=NULL
 

# number of species
nspecies = length(summary(species))

# which is the most frequent
tmp = which.max(summary(species))
dominant = names(summary(species)[tmp])

# output from function
return(list(num=nspecies, simpson=diversity, dominant=dominant, plt=p))
}


