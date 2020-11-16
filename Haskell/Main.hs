import qualified LinkedList as LL

main =
  putStr "Creating a new linked list of integers\n"
  list = LL.Empty
  putStr LL.isEmpty list ? "It is empty" : "Fails"  
  -- Pushing 3, 1, 4, 1, and 5
  -- It is not empty
  -- Length is 5
  -- List is 5 -> 1 -> 4 -> 1 -> 3 -> X
  -- Head is 5
  -- Tail is 1 -> 4 -> 1 -> 3 -> X
  -- Tail length is 4
  -- Last is 3
  -- Reversed list is 3 -> 1 -> 4 -> 1 -> 5 -> X
