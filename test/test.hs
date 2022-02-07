import qualified Test.CabalHoogle.Hoogle
import           Test.CabalHoogle.Main
import qualified Test.CabalHoogle.Package
import qualified Test.CabalHoogle.Process

main :: IO ()
main =
  disorderMain [
      Test.CabalHoogle.Hoogle.tests
    , Test.CabalHoogle.Package.tests
    , Test.CabalHoogle.Process.tests
    ]
