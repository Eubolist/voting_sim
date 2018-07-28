# Main Script

library(dplyr)
library(magrittr)
library(ramify)

set.seed(74) # for reproducibility

n_posts <- 5e4
n_votes <- 1e5
n_users <- 1e5

rnorm.cens <- function(n, mean, sd, ceil, floor) {
  rnorm(n, mean, sd) %>% ramify::clip(., ceil, floor)
}

cast.vote <- function(uni) {
  # Choose post: Previously upvoted posts are more likely to be displayed
  i <- sample(c(1:nrow(d.posts)), 1, prob=d.posts$n_univotes)
  post <- d.posts[i,c(3:4)]
  
  # We sample the user from a normal distribution of two independent factors:
  # Emotional susceptibility to respond and thoroughness in fact-checking
  user <- c(rnorm.cens(n_users, 1, .3, 1e-6, 2), rnorm.cens(n_users, 1, .3, 1e-6, 2))
  
  # Probability of the user to respond to a given post is equal to 
  # the combined factors of user and post (user*post)
  user.prob <- (user*post) %>% unlist()
  
  # Only upvote (or do nothing)
  if (uni==TRUE) {
    vote <- sample(c(1,0), 1, prob=user.prob)
    d.posts[i,1] <- d.posts[i,1]+vote
  } else { # allow downvote:
    if ((4-sum(user.prob))/2<0) {lazy <- 0} else {lazy <- (4-sum(user.prob))/2}
    user.prob <- c(user.prob, lazy)
    vote <- sample(c(1,-1,0), 1, prob=user.prob)
    d.posts[i,2] <- d.posts[i,2]+vote
  }
  assign('d.posts', d.posts, envir=.GlobalEnv)
}

# Generate random dataframe of posts with varying degrees of thruthfulness
# and 'clickbait-emotional-factor'
d.posts <- tibble(
  n_univotes=rep(1, n_posts),
  n_bivotes=rep(1, n_posts),
  emot=rnorm.cens(n_posts, 1, .3, 1e-4, 2),
  fake=rnorm.cens(n_posts, 1, .3, 1e-4, 2)
)

# Using for-loops to cont. update votes 
# (influences how likely a post is to be displayed)

for (j in c(1:n_votes)) {
  if (j%%100==0) print(j)
  cast.vote(uni=TRUE)
}

for (k in c(1:n_votes)) {
  if (k%%100==0) print(k)
  cast.vote(uni=FALSE)
}


# Plot --------------------------------------------------------------------

library(plotly)

p <- plot_ly(d.posts, x = ~emot, y = ~fake, z = ~n_univotes,
             marker = list(color = ~n_univotes, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Emotional Response'),
                      yaxis = list(title = 'Fake-News Index'),
                      zaxis = list(title = 'Number of Reactions', range = c(-5, 18))),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Reactions',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))

q <- plot_ly(d.posts, x = ~emot, y = ~fake, z = ~n_bivotes,
             marker = list(color = ~n_bivotes, colorscale = c('#FFE1A1', '#683531'), 
                           showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Emotional Response'),
                      yaxis = list(title = 'Fake-News Index'),
                      zaxis = list(title = 'Number of Votes', range = c(-5, 18))),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Votes',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
