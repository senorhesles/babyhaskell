
import Control.Monad.Trans.State
import System.Random

getRandomR :: Random a => (a,a) -> State StdGen a
getRandomR x = do generator <- get
                  let (value, newGenerator) = randomR x generator
                  put newGenerator
                  return value

rollDie :: State StdGen Int
rollDie = do
      i <- getRandomR (1,6)
      return i
