
start <- rep(0, 16)

inits <- sample(1:16, 2)

start[inits] <- 2

board <- matrix(start, 4, 4)

rows <- function(fun) apply(b, 1, fun)
cols <- function(fun) apply(b, 2, fun)

print_board <- function(b) {
  for(i in seq(4)) {
    for (j in seq(4)) {
      if (b[i, j] == 0) {
        #cat('|   ')
        cat('  .  ')
      } else {
        cat(' ', b[i, j], ' ')
        #cat(paste('| ', board[i, j], ' ', sep = ''))
      }
    }
    cat('\n')
  }
}


moves <- c('u', 'd', 'l', 'r')

# Is the given move valid
valid_move <- function(b, dir) {
  dir == 'd' & any(apply(sign(b), 2, diff) == -1) |
    dir == 'u' & any(apply(sign(b), 2, diff) == 1) |   
    dir == 'l' & any(apply(sign(b), 1, diff) == 1) |
    dir == 'r' & any(apply(sign(b), 1, diff) == -1)
}


two_in_order <- function(x) {
  x[1] == x[2] & x[1] != 0 |
    x[2] == x[3] & x[2] != 0 |
    x[3] == x[4] & x[3] != 0
}


condense <- function(b, dir) {
  dir == 'd' & any(apply(b, 2, two_in_order)) |
    dir == 'u' & any(apply(b, 2, two_in_order)) |   
    dir == 'l' & any(apply(b, 1, two_in_order)) |
    dir == 'r' & any(apply(b, 1, two_in_order))
}

# If there are no valid moves it is game over.
game_over <- function(b) {
  !any(valid_move(b, 'u'), valid_move(b, 'd'), 
      valid_move(b, 'l'), valid_move(b, 'r'))
}


# Give extra zeros to make length 4.
shift_left <- function(v) c(rep(0, 4 - length(v)), v)
shift_right <- function(v) c(v, rep(0, 4 - length(v)))

rotate <- function(x) t(apply(x, 2, rev))

ind <- function(x) {
  if (x[1] == x[2] & x[3] == x[4] & x[1] != 0 & x[3] != 0) {
    c(2*x[1], 2*x[3], 0, 0)
  } else if (x[1] == x[2] & x[1] != 0) {
    c(2*x[1], x[3], x[4], 0)
  } else if (x[2] == x[3] & x[2] != 0) {
    c(x[1], 2*x[2], x[4], 0)
  } else if (x[3] == x[4] & x[3] != 0) {
    c(x[1], x[2], 2*x[4], 0)
  } else x
}


ind2 <- function(x) {
  if (x[1] == x[2] & x[3] == x[4] & x[1] != 0 & x[3] != 0) {
    c(0, 0, 2*x[1], 2*x[3])
  } else if (x[1] == x[2] & x[1] != 0) {
    c(0, 2*x[1], x[3], x[4])
  } else if (x[2] == x[3] & x[2] != 0) {
    c(x[1], 0, 2*x[2], x[4])
  } else if (x[3] == x[4] & x[3] != 0) {
    c(x[1], x[2], 0, 2*x[4])
  } else x
}

# pad(b[, 1][b[, 1] != 0]
move <- function(b, dir) {
  if(dir == 'd') apply(b, 2, function(x) shift_left(x[x != 0]))
  else if (dir == 'u') apply(b, 2, function(x) shift_right(x[x != 0]))
  else if (dir == 'r') rotate(apply(b, 1, function(x) rev(shift_left(x[x != 0]))))
  else t(apply(b, 1, function(x) shift_right(x[x != 0])))
}


agg <- function(b, dir) {
  if(valid_move(b, dir) | condense(b, dir)) {
    b <- move(b, dir)
    b <- if(dir == 'd') apply(b, 2, ind)
    else if (dir == 'u') apply(b, 2, ind2)
    else if (dir == 'l') t(apply(b, 1, ind))
    else t(apply(b, 1, ind2))
    b <- move(b, dir)
    b[sample(which(b == 0), 1)] <- 2
    b
  } else b
}




play <- function(b) {
  print_board(board)
  while(!game_over(b)) {
    n <- scan(nmax = 1, quiet = T, what = character())
    b <- agg(b, n)
    print_board(b)
  }
  print('Game Over')
}





play(board)



