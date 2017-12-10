import Types.FileSystem
-- import Tests.ArbitraryInstances ()
import Data.Aeson.Encode.Pretty
import Test.QuickCheck
import qualified Data.ByteString.Lazy as DBL
import Types.AcidDB
import Storage.AcidDB
import Data.Acid
import Data.Acid.Advanced
import Other.IxSetAeson
import Storage.Object


-- main :: IO ()
-- main = (generate $ arbitrary :: IO Object) >>= DBL.putStr . encodePretty
