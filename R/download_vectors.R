library(glue)
library(tidyverse)

languages <- "ca zh nl fr de el it ja ko pl pt ru es sv th tr"

language_codes <- str_split(languages, " ", simplify = T)[1,]

map(language_codes, function(x) {
  cat(paste0("Language: ", x), sep = "\n")
  download.file(paste0("https://s3-us-west-1.amazonaws.com/fasttext-vectors/word-vectors-v2/cc.", x, ".300.vec.gz"), paste0("pretrained_embeddings/cc.", x, ".vec.gz"), method = "wget")
})
