import qualified PropertyTestRBBag (properties)
import Test.Tasty
import qualified TestBag (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All tests"
    [ TestBag.tests,
      PropertyTestRBBag.properties
    ]
