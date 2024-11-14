
###librerias nece
library(stringr)
library(stringdist)
library(dplyr)
###leems y revisamos
herbario_conc<-read.csv("conc_cactaceae.csv")
catalogo_col<-read.csv("col_cactaceae.csv")
head(herbario_conc)
head(catalogo_col)

colnames(herbario_conc)
colnames(catalogo_col)


####sacamos lo q no nos sirve 

group <- c('variety', 'species', 'form', 'subspecies', 'infraspecific name', 'subvariety', 'proles', 'subform', 'lusus', 'species aggregate')
catalogo_col<-catalogo_col[catalogo_col$taxonRank %in% group,]

#####hacemos el fuzzy match
length_conc<-nrow(herbario_conc)
length_col<-nrow(catalogo_col)
output<-data.frame()
for(i in length_conc){
  for(j in length_col){
    species_fuzzy <- grabl(
      pattern = toupper(trimws(catalogo_col[j,"scientificName"])), 
      x = toupper(trimws(herbario_conc[i,"Nombre.Cientifico.abreviado"])),
      method = "lv", maxDist = 0,4)
    if (species_fuzzy){
      new_row<-cbind.data.frame(catalogo_col[j,c("scientificName", "taxonID")],herbario_conc[i,c("Nombre.Cientifico.abreviado", "taxa_id")])
      output<-rbind.data.frame(output, new_row)
    }
  }
}


View(output)