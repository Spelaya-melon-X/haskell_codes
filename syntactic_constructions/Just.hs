

safeDiving :: Float -> Float -> Maybe Float
safeDiving _ 0 = Nothing
safeDiving x y = Just (x / y)

result_bad = safeDiving 10 0
result_good = safeDiving 10 5

extractValue :: Maybe Float -> Float
extractValue Nothing = 0
extractValue (Just x) = x

-- extractValue result_bad
-- extractValue result_good

main = do
    print $ extractValue result_bad
    print $ extractValue result_good