import System.IO

-- Function to ask for user input
askForInput :: String -> IO String
askForInput prompt = do
  putStrLn prompt
  getLine

-- Function to display menu options
displayMenu :: IO ()
displayMenu = do
  putStrLn "Select an option:"
  putStrLn "0. Create a new file"
  putStrLn "1. Open file and read its content"
  putStrLn "2. Add a fruit name to the file"
  putStrLn "3. Delete a fruit name from the file"
  putStrLn "Any other key to exit."

-- Function to open file and read its content
openAndReadFile :: FilePath -> IO ()
openAndReadFile file = do
  content <- readFile file
  putStrLn "File content:"
  putStrLn content

-- Function to add a fruit name to the file
addFruitName :: FilePath -> String -> IO ()
addFruitName file fruitName = do
  appendFile file (fruitName ++ ", ")
  putStrLn $ fruitName ++ " added to the file."

-- Function to delete a fruit name from the file
deleteFruitName :: FilePath -> Int -> IO ()
deleteFruitName file index = do
  content <- readFile file
  let fruits = words $ map (\c -> if c == ',' then ' ' else c) content
  if index > 0 && index <= length fruits
    then do
      let fruits' = take (index - 1) fruits ++ drop index fruits
      writeFile file $ unwords $ map (++ ", ") fruits'
      putStrLn $ "Fruit at index " ++ show index ++ " deleted from the file."
    else putStrLn "Invalid index."

-- Helper function to remove a character from a string
removeChar :: Char -> String -> String
removeChar c str = takeWhile (/= c) str ++ dropWhile (== c) str

-- Main function
main :: IO ()
main = do
  displayMenu
  choice <- askForInput "Enter your choice (0/1/2/3):"
  let filename = "fruits.txt"
  case choice of
    "0" -> do
      writeFile filename ""
      main
    "1" -> do
      openAndReadFile filename
      main
    "2" -> do
      fruitName <- askForInput "Enter fruit name:"
      addFruitName filename fruitName
      main
    "3" -> do
      index <- askForInput "Enter index of fruit to delete:"
      deleteFruitName filename (read index)
      main
    _ -> putStrLn "Invalid choice."
