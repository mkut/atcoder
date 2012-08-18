main = print . (ans!!) . abs . (\[x,y] -> x-y) . map read . words =<< getLine

ans = [0,1,2,3,2,1,2,3,3,2] ++ map (+1) ans

-- vim: set expandtab:
