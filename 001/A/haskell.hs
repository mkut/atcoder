import Data.List

main = putStrLn . unwords . map show . (\x -> [maximum x - 1, minimum x - 1]) . map length . group . sort . (++"1234") . (!!1) . lines =<< getContents

-- vim: set expandtab:
