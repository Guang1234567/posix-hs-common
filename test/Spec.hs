import Test.QuickCheck

import qualified Algorithm.SortsSpec as AS

import qualified Demo.QuickCheck.CustomArbitrary as DQC

anal :: Args
anal = Args {replay = Nothing, maxSuccess = 1000, maxDiscardRatio = 1, maxSize = 1000, chatty = True, maxShrinks = 1}

minimal :: Args
minimal = Args {replay = Nothing, maxSuccess = 200, maxDiscardRatio = 1, maxSize = 200, chatty = True, maxShrinks = 1}

runTests :: Args -> IO ()
runTests args = do
    AS.runTests args
    DQC.runTests args
    return ()

{-
main :: IO ()
main = do
    putStrLn "\n\n\n=================== Start Test ===================\n\n\n"
    putStrLn "Choose test depth"
    putStrLn "1. Anal"
    putStrLn "2. Minimal"
    depth <- readLn :: IO Int
    if depth == 1 then runTests anal else runTests minimal
    putStrLn "\n\n\n===================  End Test  ===================\n\n\n"
---}
--{-
main :: IO ()
main = do
    putStrLn "\n\n\n=================== Start Test ===================\n\n\n"
    runTests minimal
    putStrLn "\n\n\n===================  End Test  ===================\n\n\n"

---}
{-
测试覆盖率:

stack test --coverage

hpc report ./.stack-work/install/x86_64-osx/lts-13.29/8.6.5/hpc/osx-hs-common/osx-hs-common-test/osx-hs-common-test.tix --hpcdir=./.stack-work/dist/x86_64-osx/Cabal-2.4.0.1/hpc

-}
testDebugger :: String -> IO Int
testDebugger str = do
    putStrLn str
    return 0