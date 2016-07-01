f <- file.choose ('massey_ratings_test.csv')
df<- read.csv(f)

num_teams <- length(unique(c(df$team1, df$team2)))
num_games <- nrow(df)
teams <- unique(c(df$team1, df$team2))

X <- matrix(0, nrow=num_teams, ncol=num_teams)
for(i in 1:nrow(df)){
        h <- match(df$team1[i], teams)
        a <- match(df$team2[i], teams)
        X[h, a] <- X[h, a] - 1
        X[a, h] <- X[a, h] - 1
}

for(i in 1:nrow(X)){
        X[i, i] <- abs(sum(X[i, ]))
}
X[nrow(X), ] <- 1

y <- sapply(teams, function(x, df){
        home <- subset(df, team1 == x)
        away <- subset(df, team2 == x)
        games_for <- sum(home$team1_games) + sum(away$team2_games)
        games_away <- sum(home$team2_games) + sum(away$team1_games)
        games_for - games_away
}, df=df)
y[length(y)] <- 0

games_for <- sapply(teams, function(x, df){
        home <- subset(df, team1 == x)
        away <- subset(df, team2 == x)
        home_games <- sum(home$team1_games)
        away_games <- sum(away$team2_games)
        home_games + away_games
}, df=df)

ratings <- solve(X, y)
names(ratings) <- teams

g <- matrix(0, nrow=num_teams, ncol=num_teams)
for(i in 1:nrow(df)){
        h <- match(df$team1[i], teams)
        a <- match(df$team2[i], teams)
        g[h, a] <- g[h, a] + 1
        g[a, h] <- g[a, h] + 1
}
for(i in 1:nrow(g)){
        g[i, i] <- sum(g[i, ])
}

grpf <- as.data.frame((diag(g) * ratings) - games_for)
row.names(grpf) <- teams

def_ratings <- solve(g, grpf[ ,1])
off_ratings <- ratings - def_ratings