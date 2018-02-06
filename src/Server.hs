import Server.MainServer
import Types.AcidDB

import Data.Acid
import Servant.Server
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (logStdout)
import Control.Exception (bracket)

app :: AcidState AcidDB -> Application
app = serve api . serverMain

main :: IO ()
main = bracket
  (openLocalStateFrom "hs3db" initAcidDB)
  (\db -> createCheckpoint db >> closeAcidState db)
  (\db -> run 8081 (logStdout $ app db) )

