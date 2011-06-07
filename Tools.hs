module Tools (
    separator
) where


separator :: String -> String
separator msg = 
    let width = 80
        pre   = "-- "
        post  = replicate (width - length pre - length msg - 1) '-' in
    pre ++ msg ++ " " ++ post

