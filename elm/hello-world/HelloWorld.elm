module HelloWorld exposing (..)

helloWorld : Maybe String -> String
helloWorld name =
  let who = case name of
    Just name -> name
    Nothing -> "World"
  in "Hello, " ++ who ++ "!"
