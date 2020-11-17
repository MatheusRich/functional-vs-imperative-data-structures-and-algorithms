import qualified LinkedList as LL

maybeToStr :: Show a => Maybe a -> String
maybeToStr (Just value) = show value
maybeToStr Nothing = "Fails"

main = do
  putStrLn "Creating a new linked list of integers"
  let list = LL.Empty
  if (LL.isEmpty list) then putStrLn "It is empty" else putStrLn "Fails"  
  putStrLn "Pushing 3, 1, 4, 1, and 5"
  let newList = LL.push 5 (LL.push 1 (LL.push 4 (LL.push 1 (LL.push 3 list))))
  if (not (LL.isEmpty newList)) then putStrLn "It is not empty" else putStrLn "Fails"  
  putStrLn ("Length is " ++ show (LL.length newList))
  putStrLn ("List is " ++ LL.toString(newList))
  putStrLn ("Head is " ++ maybeToStr (LL.head newList))
  putStrLn ("Tail is " ++ LL.toString (LL.tail newList))
  putStrLn ("Tail length is " ++ show (LL.length (LL.tail newList)))
  putStrLn ("Last is " ++ maybeToStr(LL.last newList))
  putStrLn ("Reversed list is " ++ LL.toString(LL.reverse newList))
