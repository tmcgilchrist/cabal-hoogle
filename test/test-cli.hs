import           Test.CabalHoogle.Main

main :: IO ()
main =
  disorderCliMain ["cabal exec -- cabal-hoogle"]
