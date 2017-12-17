import Test.Hspec
import Bucket

main :: IO ()
main = mapM_ hspec [bucketTests]
