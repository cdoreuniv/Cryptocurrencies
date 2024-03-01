# Fonction pour lire les explications à partir du fichier texte
lire_explications <- function() {
  explications <- readLines("explication_crypto.txt")
  explications <- split(explications, cumsum(grepl("^#", explications)))
  explications <- lapply(explications, function(x) {
    name <- sub("^#\\s*", "", x[1])
    text <- paste(x[-1], collapse = "\n")
    return(list(name = name, text = text))
  })
  names(explications) <- sapply(explications, function(x) x$name)
  return(explications)
}


