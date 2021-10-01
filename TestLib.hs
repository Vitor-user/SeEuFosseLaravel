module TestLib(
    assertTrue,
    assertFalse,
    assertEqual,
    assertNotEqual
) where


assertTrue :: Bool -> String -> IO()
assertTrue True message  = showSuccess message
assertTrue False message = showFail message

assertFalse :: Bool -> String -> IO()
assertFalse False message  = showSuccess message
assertFalse True message = showFail message

assertEqual :: (Eq a) => a -> a -> String -> IO()
assertEqual a b message
    | a == b = showSuccess message
    | otherwise = showFail message

assertNotEqual :: (Eq a) => a -> a -> String -> IO()
assertNotEqual a b message
    | a /= b = showSuccess message
    | otherwise = showFail message

showSuccess :: String -> IO()
showSuccess message = putStrLn ("[PASS] " ++ message)

showFail :: String -> IO()
showFail message = putStrLn ("   [FAIL] " ++ message)