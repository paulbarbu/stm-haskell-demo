module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

    
-- http://rosettacode.org/wiki/Dining_philosophers#Haskell
type Fork = TMVar Int

newFork :: Int -> IO Fork
newFork i = newTMVarIO i

takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork

releaseFork :: Fork -> Int -> STM ()
releaseFork fork i = putTMVar fork i

runPhilosopher :: String -> (Fork, Fork) -> IO()
runPhilosopher name (left, right) = forever $ do
    putStrLn $ name ++ " is hungry"

    (leftNum, rightNum) <- atomically $ do
        leftNum <- takeFork left
        rightNum <- takeFork right
        return (leftNum, rightNum)

    putStrLn $ name ++ " got forks " ++ show leftNum ++ " and " ++ show rightNum ++ " and is eating"
    delay <- randomRIO (1, 10)
    threadDelay (delay * 1000000)
    putStrLn $ name ++ " is done eating. Going back to thinking."

    atomically $ do
        releaseFork left leftNum
        releaseFork right rightNum
    
    delay <- randomRIO (1, 10)
    threadDelay (delay * 1000000)

     
philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]
 
main :: IO String
main = do
    forks <- mapM newFork [1..5]
    let namedPhilosophers  = map runPhilosopher philosophers
        forkPairs          = zip forks (tail . cycle $ forks)
        philosophersWithForks = zipWith ($) namedPhilosophers forkPairs
   
    putStrLn "Running the philosophers. Press enter to quit."
   
    mapM_ forkIO philosophersWithForks
   
    -- All threads exit when the main thread exits.
    getLine
   


--   def dine(self):
--   fork1, fork2 = self.forkOnLeft, self.forkOnRight

--   while self.running:
--       fork1.acquire(True)
--       locked = fork2.acquire(False)
--       if locked: break
--       fork1.release()
--       print '%s swaps forks' % self.name
--       fork1, fork2 = fork2, fork1
--   else:
--       return

--   self.dining()
--   fork2.release()
--   fork1.release()



-- https://www.schoolofhaskell.com/user/alexanderaa/stm-examples

-- module Main where
    
-- import qualified Control.Concurrent.STM    as T

-- type Account = T.TMVar Float

-- openAccount :: Float -> T.STM (Account)
-- openAccount balance = T.newTMVar balance

-- transfer :: Account -> Account -> Float -> T.STM Float
-- transfer accountA accountB amount = do
--     startingBalanceA <- T.takeTMVar accountA
--     startingBalanceB <- T.takeTMVar accountB

--     let finalBalanceA = (startingBalanceA - amount)
--     let finalBalanceB = (startingBalanceB + amount)

--     T.putTMVar accountA finalBalanceA
--     T.putTMVar accountB finalBalanceB

--     return $ amount

-- main :: IO ()
-- main = do
--     accountA <- T.atomically (openAccount 20)
--     accountB <- T.atomically (openAccount 50)
--     amt <- T.atomically (transfer accountA accountB 30)
--     print $ amt









--     withdraw :: TVar Int -> Int -> STM ()
--     withdraw acc n = do
--         bal <- readTVar acc
--         if bal < n then retry
--         writeTVar acc (bal-n)
    
--     main :: IO ()
--     main = atomically $ do
--         withdraw a1 3
--         withdraw a2 7
                        


--     atomic (a1 > 3 && a2 > 7) { 
--         withdraw(a1, 3);
--         withdraw(a2, 7);
--      }
               


--     atomic {
--         if (n>k) then launch_missiles(); S2
--     }
     
     

--     atomic {
--         if (x != y)
--             while (true);
--     }

--     atomic {
--         x++;
--         y++;
--     }
