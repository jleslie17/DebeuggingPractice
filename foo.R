message <- function(x){
        if(x > 0){
                print("Hello")
        }else{
                print("Goodbye")
        }
}

f <- function(x) {
        r <- x - g(x)
        r
}
g <- function(y) {
        r <- y * h(y)
        r
}
h <- function(z) {
        r <- log(z)
        if (r < 10)
                r^2
        else r^3
}

SS <- function(mu, x) {
        d <- x - mu
        d2 <- d^2
        ss <- sum(d2)
        ss
}
set.seed(100) ## set the RNG seed so that the results are reproducible
x <- rnorm(100)
SS(1, x)


foo = function(x){
        if(x > 0) salutation = 'Hello' else
                salutation = 'Goodbye'
        salutation
}
