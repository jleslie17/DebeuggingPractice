stringrev <- function(str){
        vec <- strsplit(str, '')
        vec <- rev(unlist(vec))
        paste(vec, collapse = '')
}

crazify <- function(str) {
        vec <- strsplit(str, ' ')
        vec <- lapply(unlist(vec), stringrev)
        paste(vec, collapse = ' ')
}



test_it <- function(){
        sentences <- data.frame(
                titles = c('alphabet',
                           'lorum'),
                text = c('the quick brown fox jumps over the lazy dog',
                         'lorem ipsum dolor sit amet, consectetur adipiscing elit.'),
                stringsAsFactors = F
        )
        sentences$text <- crazify(sentences$text)
}