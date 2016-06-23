# # # vapply(integer(), identity) #fails : no FUN.VALUE
# # lapply(integer(), identity)
# # Map(identity, integer())
# # lapply(1:5, identity)
# # Map(identity, 1:5)
# # mapply(identity, 1:5)
# # mapply(identity, integer(0))
# # 
# 
# 
# vapply(list(x = 1:12, y = 1:12), head, integer(6))
# # vapply(list(x = 1:12, y = LETTERS[1:12]), head, character(3)) # fails
# vapply(list(), head, integer(0))
# 
# lapply(list(x = 1:12, y = 1:12), head)
# lapply(list(x = 1:12, y = LETTERS[1:12]), head)
# lapply(list(), head)
# 
# Map(head, list(x = 1:12, y = 1:12))
# Map(head, list(x = 1:12, y = LETTERS[1:12]))
# Map(head, list())
# 
# ###
# 
# vapply(list(x = 1:12, y = 1:12), head, integer(6))
# #>      x y
# #> [1,] 1 1
# #> [2,] 2 2
# #> [3,] 3 3
# #> [4,] 4 4
# #> [5,] 5 5
# #> [6,] 6 6
# 
# # vapply(list(x = 1:12, y = LETTERS[1:12]), head, character(3))
# vapply(list(), head, integer(0))
# #> <0 x 0 matrix>
# 
# 
# lapply(list(x = 1:12, y = 1:12), head)
# #> $x
# #> [1] 1 2 3 4 5 6
# #> 
# #> $y
# #> [1] 1 2 3 4 5 6
# #> 
# 
# lapply(list(x = 1:12, y = LETTERS[1:12]), head)
# #> $x
# #> [1] 1 2 3 4 5 6
# #> 
# #> $y
# #> [1] "A" "B" "C" "D" "E" "F"
# #> 
# 
# lapply(list(), head)
# #> list()
# 
# 
# Map(head, list(x = 1:12, y = 1:12))
# #> $x
# #> [1] 1 2 3 4 5 6
# #> 
# #> $y
# #> [1] 1 2 3 4 5 6
# #> 
# 
# Map(head, list(x = 1:12, y = LETTERS[1:12]))
# #> $x
# #> [1] 1 2 3 4 5 6
# #> 
# #> $y
# #> [1] "A" "B" "C" "D" "E" "F"
# #> 
# 
# Map(head, list())
# #> list()
# 
# 
# # 
# # 
# # gist.content <- list(
# #   description = "Is there a typesafe way to vapply over a mixed-content list?",
# #   public = "true",
# #   files = list("vapply-mixed-content-list.R" = list(
# #     content = "vapply(list(x = 1:12, y = 1:12), head, integer(6))
# # vapply(list(x = 1:12, y = LETTERS[1:12]), head, character(3))
# # vapply(list(), head, integer(0))
# # 
# # lapply(list(x = 1:12, y = 1:12), head)
# # lapply(list(x = 1:12, y = LETTERS[1:12]), head)
# # lapply(list(), head)
# # 
# # Map(head, list(x = 1:12, y = 1:12))
# # Map(head, list(x = 1:12, y = LETTERS[1:12]))
# # Map(head, list())")))
# # 
# # githubtools::github_setup()
# # 
# # gist <- jsonlite::toJSON(gist.content, pretty = TRUE, auto_unbox = TRUE)
# # github::create.gist(gist)
# 
# ## formatR
# ## tidy.source()
# ## tidy_eval
# 
# tidy_eval(text="vapply(list(x = 1:12, y = 1:12), head, integer(6))
# # vapply(list(x = 1:12, y = LETTERS[1:12]), head, character(3))
# vapply(list(), head, integer(0))
# 
# lapply(list(x = 1:12, y = 1:12), head)
# lapply(list(x = 1:12, y = LETTERS[1:12]), head)
# lapply(list(), head)
# 
# Map(head, list(x = 1:12, y = 1:12))
# Map(head, list(x = 1:12, y = LETTERS[1:12]))
# Map(head, list())", prefix="#> ")
