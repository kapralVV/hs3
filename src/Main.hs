import Types.FileSystem
import qualified Data.ByteString.Lazy as DBL
import Types.AcidDB
import Storage.AcidDB
import Data.Acid
import Data.Acid.Advanced
import Other.IxSetAeson
import Storage.Object
import Storage.FileData


-- main :: IO ()
-- main = (generate $ arbitrary :: IO Object) >>= DBL.putStr . encodePretty
