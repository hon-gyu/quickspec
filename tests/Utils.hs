module Utils where

sep :: String
sep = replicate 10 '='

fence :: String -> String -> String
fence lang content =
  "```" ++ lang ++ "\n" ++ content ++ "\n```"

h1 :: String -> String
h1 title = title ++ "\n" ++ sep

h2 :: String -> String
h2 title = title ++ "\n" ++ replicate 10 '-'
