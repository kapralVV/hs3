import Test.Hspec
import Tests.Storage.Bucket
import Tests.Storage.Object

main :: IO ()
main = mapM_ hspec [ bucketTests
                   , objectTests
                   ]
