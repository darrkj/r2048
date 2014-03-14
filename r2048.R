
start <- rep(0, 16)

inits <- sample(1:16, 2)

start[inits] <- 2


board <- matrix(start, 4, 4)



print_board <- function(b) {
  for(i in seq(4)) {
    for (j in seq(4)) {
      if (board[i, j] == 0) {
        #cat('|   ')
        cat(' . ')
      } else {
        cat('', board[i, j])
        #cat(paste('| ', board[i, j], ' ', sep = ''))
      }
    }
    #cat('|\n')
  }
}

print_board(board)
