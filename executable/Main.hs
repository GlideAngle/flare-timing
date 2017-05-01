import Data.Waypoint (parse)

snippet :: String
snippet = unlines $
    [ "\n"
    , "B1101355206343N00006198WA0058700558\n"
    , "B1101455206259N00006295WA0059300556\n"
    , "B1101555206300N00006061WA0060300576\n"
    , "B1102055206337N00006201WA0062000586\n"
    , "B1102155206314N00005990WA0062200588\n"
    , "B1102255206417N00006098WA0063100596\n"
    ]

main :: IO ()
main = print $ parse snippet
