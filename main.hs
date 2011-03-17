import Prelude hiding (catch)
import Network
import System.IO
import System.IO.Error (isEOFError)
import Text.Printf
import Control.Concurrent
import Control.Concurrent.STM
--import Control.Exception
import Control.OldException (finally, catch, Exception(..))
import qualified Data.Map as M

import Types

server = "10.0.0.110"
port = 4894

username = "HasKOM"
hostname = "HasKOM"
userhost = "HasKOM"

main = withSocketsDo $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering

    start h `catch` handler `finally` hClose h
        where
            handler (IOException e)
                    | isEOFError e = return ()
            handler e = putStrLn $ show e



start :: Handle -> IO ()
start h = do
    netChan <- atomically newTChan
    userChan <- atomically newTChan
    netTID <- spawn $ listenLoop (hGetLine h) netChan
    userTID <- spawn $ listenLoop getLine userChan
    let envar = Env { reqs = M.empty, refno = 0, socket = h }

    write' h $ "A" ++ show (length userhost) ++ "H" ++ userhost
    envar2 <- setversion envar

    mainLoop h netChan userChan envar2


spawn :: IO () -> IO ThreadId
spawn act = do
    mainTID <- myThreadId
    forkIO $ act `catch` throwTo mainTID

listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan = do
    v <- act
    atomically $ writeTChan chan v
    listenLoop act chan

mainLoop :: Handle -> TChan String -> TChan String -> Env -> IO ()
mainLoop h netChan userChan env = do
    input <- atomically $ select netChan userChan
    case input of
        -- from netChan to user
        Left str -> do
            newEnv <- handleNetwork h env (words str)
            hFlush stdout
            mainLoop h netChan userChan newEnv
        -- from userChan to net
        Right str -> do
            newEnv <- handleUserCommand env (words (str))
            hFlush h
            mainLoop h netChan userChan newEnv

select :: TChan a -> TChan b -> STM (Either a b)
select ch1 ch2 = do
    a <- readTChan ch1; return (Left a)
    `orElse` do
    b <- readTChan ch2; return (Right b)


handleNetwork :: Handle -> Env -> [String] -> IO Env
handleNetwork h env (str) = do
    --putStrLn (show (reqs env))
    putStrLn (unwords str)
    case str !! 0 !! 0 of
        '%' -> do -- error
            case str !! 1 of
                "0" -> print "No error" >> return env
                "2" -> print "Not implemented" >> return env
                "3" -> print "Obsolete call" >> return env
                "4" -> print "Invalid password" >> return env
                "5" -> print "String too long" >> return env
                "6" -> print "Login first" >> return env
                "7" -> print "Login disallowed" >> return env
                "8" -> print "Conference zero" >> return env
                "9" -> print "Undefined conference" >> return env
                "10" -> print "Undefined person" >> return env
                "11" -> print "Access denied" >> return env
                "12" -> print "Permission denied" >> return env
                "13" -> print "Not member" >> return env
                "14" -> print "No such text" >> return env
                "15" -> print "Text zero" >> return env
                "16" -> print "No such local text" >> return env
                "17" -> print "Local text zero" >> return env
                "18" -> print "Bad name" >> return env
                "19" -> print "Index out of range" >> return env
                "20" -> print "Conference exists" >> return env
                "21" -> print "Person exists" >> return env
                "22" -> print "Secret public" >> return env
                "23" -> print "Letterbox" >> return env
                "24" -> print "Ldb error" >> return env
                "25" -> print "Illegal misc" >> return env
                "26" -> print "Illegal info type" >> return env
                "27" -> print "Already recipient" >> return env
                "28" -> print "Already comment" >> return env
                "29" -> print "Already footnote" >> return env
                "30" -> print "Not recipient" >> return env
                "31" -> print "Not comment" >> return env
                "32" -> print "Not footnote" >> return env
                "33" -> print "Recipient limit" >> return env
                "34" -> print "Comment limit" >> return env
                "35" -> print "Footnote limit" >> return env
                "36" -> print "Mark limit" >> return env
                "37" -> print "Not author" >> return env
                "38" -> print "No connect" >> return env
                "39" -> print "Out of memory" >> return env
                "40" -> print "Server is crazy" >> return env
                "41" -> print "Client is crazy" >> return env
                "42" -> print "Undefined session" >> return env
                "43" -> print "Regexp error" >> return env
                "44" -> print "Not marked" >> return env
                "45" -> print "Temporary failure" >> return env
                "46" -> print "Long array" >> return env
                "47" -> print "Anonymous rejected" >> return env
                "48" -> print "Illegal aux item" >> return env
                "49" -> print "Aux item permission" >> return env
                "50" -> print "Unknown async" >> return env
                "51" -> print "Internal error" >> return env
                "52" -> print "Feature disabled" >> return env
                "53" -> print "Message not sent" >> return env
                "54" -> print "Invalid membership type" >> return env
                otherwise -> return env
        '=' -> do -- reply
            let ref = read (tail (str !! 0)) :: Int
            let request = M.lookup ref (reqs env)
            case request of
                Nothing -> return env
                Just request -> do
                    newEnv <- (unRequestCallback request) env (unwords str)
                    return newEnv {reqs = (M.delete ref (reqs newEnv))}
        otherwise -> return env


handleUserCommand :: Env -> [String] -> IO Env
--handleUserCommand env ("login":username:passwd:invisible:xs) = login env username passwd (read invisible)
handleUserCommand env ("login":[]) = print "Usage: login username" >> return env
handleUserCommand env ("login":username) = do
    putStr "Password: " >> hFlush stdout
    passwd <- getLine
    login env (unwords username) passwd 0

handleUserCommand env ("logout":xs) = logout env

handleUserCommand env ("disconnect":xs) = disconnect env >> return env

handleUserCommand env ("vilka":visible:invisible:xs) = whoIsOnDynamic env (read visible) (read invisible)
handleUserCommand env ("vilka":xs) = whoIsOnDynamic env 1 0

handleUserCommand env ("lista":"möten":xs) = listMeetings env
handleUserCommand env ("lista":"personer":xs) = listPersons env

handleUserCommand env ("skapa":"person":[]) = print "Usage: skapa person <username>" >> return env
handleUserCommand env ("skapa":"person":username) = do
    putStr "Password: " >> hFlush stdout
    passwd <- getLine
    createPerson env (unwords username) passwd

handleUserCommand env ("whoami":xs) = whoami env

handleUserCommand env ("what-i-do":str) = whatido (unwords str) env

handleUserCommand env ("getUnread":xs) = getUnreadConfs env
handleUserCommand env (str) = write' (socket env) (unwords str) >> return env


write :: Env -> Int -> String -> IO Env
write env cmd s = do
    write'' env cmd s doNothing

write' :: Handle -> String -> IO ()
write' h "" = return ()
write' h s = hPrintf h "%s\n" s >> printf "> %s\n" s

write'' :: Env -> Int -> String -> (Env -> String -> IO Env) -> IO Env
write'' env cmd s callback = do
    let h = (socket env)
    write' h (show (refno env) ++ " " ++ show cmd ++ " " ++ s)
    return env {refno = refno env + 1, reqs = M.insert (refno env) (Request {unRequestCmd = cmd, unRequestRef = (refno env), unRequestCallback = callback}) (reqs env)}

listen :: Handle -> IO ()
listen h = forever $ do
        s <- hGetLine h
        putStrLn s
    where
        forever a = a >> forever a



-- Format: ref-no request

getServerVersion :: Env -> IO Env
getServerVersion env = write env 75 ""

setversion :: Env -> IO Env
setversion env = write env 69 "6HHasKOM 14H0.01 pre-alpha"

disconnect :: Env -> IO Env
disconnect env = write env 55 "0"

whoami :: Env -> IO Env
whoami env = write env 56 ""

whoIsOnDynamic :: Env -> Int -> Int -> IO Env
whoIsOnDynamic env visible invisible = write env 83 (show (visible) ++ " " ++ show (invisible) ++ " 0")

getInfo :: Env -> IO Env
getInfo env = write env 94 ""

logout :: Env -> IO Env
logout env = write env 1 ""

login :: Env -> String -> String -> Int -> IO Env
--login env userid passwd invisible = write'' env 62 (show (userid) ++ " " ++ show (length passwd) ++ "H" ++ passwd ++ " " ++ show (invisible)) doLogin
login env username passwd invisible = do
    newEnv <- write'' env 76 (show (length username) ++ "H" ++ username ++ " 1 0") doLogin
    return newEnv {myPasswd = passwd, invisible = invisible}

whatido :: String -> Env -> IO Env
whatido s env = write env 4 (show (length s) ++ "H" ++ s)

rezLookup :: Env -> String -> Int -> Int -> IO Env
rezLookup env regex wantPersons wantConfs = write env 74 (show (length regex) ++ "H" ++ regex ++ " " ++ show (wantPersons) ++ " " ++ show (wantConfs))

lookupZName :: Env -> String -> Int -> Int -> IO Env
lookupZName env name wantPersons wantConfs = write env 76 (show (length name) ++ "H" ++ name ++ " " ++ show (wantPersons) ++ " " ++ show (wantConfs))

getClientName :: Env -> Int -> IO Env
getClientName env sessionNo = write env 70 (show sessionNo)

getClientVersion :: Env -> Int -> IO Env
getClientVersion env sessionNo = write env 71 (show sessionNo)

getSessionInfo :: Env -> Int -> IO Env
getSessionInfo env sessionNo = write env 84 (show sessionNo)

getUnreadConfs :: Env -> IO Env
getUnreadConfs env = write env 52 (show (myUid env))

listMeetings :: Env -> IO Env
listMeetings env = lookupZName env "" 0 1

listPersons :: Env -> IO Env
listPersons env = lookupZName env "" 1 0

createPerson :: Env -> String -> String -> IO Env
createPerson env username passwd = write env 89 (show (length username) ++ "H" ++ username ++ " " ++ show (length passwd) ++ "H" ++ passwd ++ " 0 0 {}")


-- callbacks
doNothing :: Env -> String -> IO Env
doNothing env _ = return env

doLogin :: Env -> String -> IO Env
doLogin env s = do
    let newEnv = env {myUid = read ((words s) !! ((length (words s)) - 2))}
    newEnv2 <- write newEnv 62 (show (myUid newEnv) ++ " " ++ show (length (myPasswd newEnv)) ++ "H" ++ (myPasswd newEnv) ++ " " ++ show (invisible newEnv))
    whatido "Kör HasKOM Klienten." newEnv2
