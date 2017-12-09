import Types.FileSystem
import Tests.ArbitraryInstances ()
import Data.Aeson.Encode.Pretty
import Test.QuickCheck
import Data.ByteString.Lazy
import Types.AcidDB
import Storage.AcidDB

main :: IO ()
main = (generate $ arbitrary :: IO Object) >>= Data.ByteString.Lazy.putStr . encodePretty
