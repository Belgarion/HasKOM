module Types where

import Data.Map (Map)
import Control.Monad.Reader
import System.IO

newtype RefNo = RefNo { unRefNo :: Int } deriving (Show, Eq)
newRefNo :: Int -> RefNo
newRefNo = RefNo

data Env = Env {
    socket :: Handle,
    refno :: Int,
    myUid :: Int,
    myPasswd :: String,
    invisible :: Int,
    reqs :: Map Int Request
}

data Request = Request {
    unRequestRef :: Int,
    unRequestCmd :: Int,
    unRequestCallback :: Env -> String -> IO Env }

instance Show Request where
    show rq = "Request {" ++ (show $ unRequestRef rq) ++ ", " ++ (show $ unRequestCmd rq) ++ "}"


type Net = ReaderT Env IO
