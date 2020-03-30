-- make people noble

mknoble :: Bool -> String -> String
mknoble True  name = "Dame " ++ name
mknoble False name = "Sir " ++ name
