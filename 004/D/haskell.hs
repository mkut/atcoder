import System.IO
import Control.Monad
import Milib.Contest
import Milib.IO
import Milib.Data.Modulo.Int

main = contestMain printer solver parser

solver (n, m) = do
   

printer = hPrint

parser = liftM2 (,) number number

-- vim: set expandtab:
