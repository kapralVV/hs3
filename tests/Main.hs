import Test.Hspec
import Tests.Storage.Bucket

main :: IO ()
main = mapM_ hspec [bucketTests]
