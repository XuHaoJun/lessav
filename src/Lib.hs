#!/usr/bin/env stack
-- stack script --resolver lts-16.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lib where
    -- ( 
    --   createAvTree,
    --   dumpDb
    -- ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import qualified Data.String.UTF8 as U8
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import Data.Text.Encoding
import Network.HTTP.Simple
import Data.Char
import Text.HTML.TagSoup
import Text.Printf
import System.Process
import System.Directory
import Text.Regex.TDFA
import Data.List.Split (splitWhen)
import Data.List (findIndex, insert, intercalate, groupBy, sortBy)
import System.FilePath
import Data.Maybe (catMaybes)
import Control.Concurrent (threadDelay, yield)
import qualified Control.Concurrent.Async.Pool as AsyncPool
import qualified Control.Concurrent.Async as Async

import qualified BroadcastChan as BChan

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Exception (Exception)
import Control.Immortal.Queue

import Control.DeepSeq (force, rnf, deepseq)
import Control.Exception (evaluate)

import Async (mapPool)
import Levenshtein (lev, levp)
import Av

dmmUrlAvId :: String -> String
dmmUrlAvId avId = toLower' $ map (\c -> if c == '-' then ' '; else c) avId

toLower' :: String -> String
toLower' s = (T.unpack .  T.toLower . T.pack) s

-- let machine go blind.
getDmmSearchUrl :: String -> String
getDmmSearchUrl avId = 
    p7 ++ p3 ++ p2 ++ p4 ++ p6 ++ dmmUrlAvId(avId)
    where
        p4 = "m.co.jp"
        p3 = "www."
        p2 = "dm"
        p6 = "/digital/-/list/search/=/?searchstr="
        p7 = "https://"


s :: String -> String
s a = a

-- TODO
-- may be collect all result?
-- check href match avId by edit distance.
type Url = String
dmmFindOne :: String -> IO(Maybe Url)
dmmFindOne avId = do
    req <- parseRequest $ getDmmSearchUrl avId
    response <- httpLBS req
    let code = (getResponseStatusCode response)
    let body = getResponseBody response
    putStrLn $ "[" ++ avId ++ "]" ++ " dmmFindOne code: " ++ show code
    return $ case code of
                 200 -> 
                     let link = L8.unpack $ findFirstLink $ parseTags body
                         maybeAvFullId = regexResultAvFullId $ link =~ avFullIdRegex
                     in case maybeAvFullId of
                            Nothing -> Nothing
                            Just avFullId -> if levp (toLower' avId) (toLower' avFullId) >= avIdFullIdSimilarity then Just link else Nothing
                 _   -> Nothing
    where 
        avIdFullIdSimilarity = 0.65
        regexResultAvFullId :: (String, String, String, [String]) -> Maybe String
        regexResultAvFullId = (\matchResult -> 
                                    let (_, _, _, submatchs) = matchResult 
                                    in if null submatchs
                                            then Nothing
                                            else Just $ head submatchs)
        avFullIdRegex :: String
        avFullIdRegex = "/cid=(.+)/"
        -- TODO
        -- handle head error or if null check
        findFirstLink = 
            fromAttrib "href" . head . 
            dropWhile (~/= s "<a>") .
            dropWhile (~/= s "<p class=tmb>") .
            dropWhile (~/= s "<ul id=list>")

dmmParseAvTable :: [Tag String] -> Av
dmmParseAvTable avTable = 
    let 
        actors = findActors avTable
        flated = flatTable avTable
        tagsIndex = case findIndex (\text -> text == "ジャンル：") flated of
                        Nothing -> -1
                        Just i -> i
        tags = if tagsIndex  < 0
               then []
               else filter (\text -> (not (all isSpace text))) $
                    splitWhen isSpace (flated !! (tagsIndex + 1))
        fullAvIdIndex = case findIndex (\text -> text == "品番：") flated of
                            Nothing -> -1
                            Just i -> i
        fullAvId = if fullAvIdIndex  < 0
                   then ""
                   else (flated !! (fullAvIdIndex + 1))
    in Av { avId="", fullAvId=fullAvId, jpDisplay="", url="", filePath="", actors=actors, tags=tags}
    where
        findActors = filter (\text -> (not (all isSpace text))) .
                     map fromTagText .
                     filter isTagText .
                     takeWhile(~/= s "</td>") .
                     dropWhile (~/= s "<span id=performer>") 
        flatTable = map (innerText . takeWhile(~/= s "</td>")) . sections (~== s "<td>")

-- TODO
-- go to know how [Char] String Text ByteString LazyByteString work.
l8ToUtf8 :: L8.ByteString -> String
l8ToUtf8 = T.unpack . decodeUtf8 . S8.pack . L8.unpack

dmmGetAvByUrl :: String -> IO(Maybe Av)
dmmGetAvByUrl url = do
    req <- parseRequest url
    response <- httpLBS req
    let code = (getResponseStatusCode response)
    putStrLn $ "dmmGetAvByUrl code: " ++ show code
    case code of
        200 -> 
            let 
                tags = parseTags( l8ToUtf8 $ getResponseBody response)
                avTable = findAvTable tags
                jpDisplay = findJpDisplay tags
                avFromTable = dmmParseAvTable avTable
            in return $ Just $ Av {
                avId = "",
                url = url,
                jpDisplay = jpDisplay,
                filePath = "",
                fullAvId = getFullAvId(avFromTable),
                actors = getActors(avFromTable),
                tags = getTags(avFromTable)
            }
        _ -> return Nothing
    where 
        findAvTable = takeWhile(~/= s "</table") .
                      dropWhile (~/= s "<table class=mg-b12>")
        findJpDisplay = innerText . takeWhile(~/= s "</h1>") . dropWhile (~/= s "<h1 id=title>")

dmmGetAv :: String -> IO(Maybe Av)
dmmGetAv avId = do
    maybeAvUrl <- dmmFindOne avId
    case maybeAvUrl of
        Nothing -> return Nothing
        Just avUrl -> (do
            !maybeAv <- dmmGetAvByUrl avUrl
            return $ case maybeAv of
                Nothing -> Nothing
                Just av -> Just $ (setAvId avId av))

dmmGetAv2 :: String -> IO(Maybe Av)
dmmGetAv2 avId = do
    maybeAvUrl <- dmmFindOne avId
    result <- (case maybeAvUrl of
            Nothing -> return Nothing
            Just avUrl -> (do
                !maybeAv <- dmmGetAvByUrl avUrl
                return $ case maybeAv of
                    Nothing -> Nothing
                    Just av -> Just $ (setAvId avId av)))
    return $ deepseq maybeAvUrl result

getAvIdFromDir :: FilePath -> IO [(String, FilePath)]
getAvIdFromDir dir  = do
    filePaths <- getDirectoryContents dir
    let onlyFiles = filter hasExtension filePaths
    return $ getIdPathPair onlyFiles
    where
        avRegex :: String
        avRegex = "^([a-zA-Z]+-[0-9]+)" 
        getIdPathPair :: [FilePath] -> [(String, FilePath)]
        getIdPathPair fs = foldl 
                            (\xs f ->
                                 let avIdMatch = (f =~ avRegex :: String)
                                 in if null avIdMatch
                                    then xs
                                    else insert (toLower avIdMatch, (dir </> f)) xs)
                            [] fs

outPutDir :: FilePath
outPutDir = "lessav_output"

createOutputDir :: IO ()
createOutputDir = do 
    createDirectoryIfMissing True outPutDir

numMaxConcurrentThreads :: Int
numMaxConcurrentThreads = 3

createAvTree2 :: IO ()
createAvTree2 = do
    _ <- createOutputDir
    currentDir <- getCurrentDirectory
    avIdWithFilePathPairs <- getAvIdFromDir currentDir
    let avIds = map fst avIdWithFilePathPairs
    let avFilePaths = map snd avIdWithFilePathPairs
    maybeAvsNoPath <- mapPool numMaxConcurrentThreads 
                              (\avId -> (do
                                av <- dmmGetAv avId
                                _ <- threadDelay 1000000
                                return av
                              ))
                              avIds
    let avsNoPath = catMaybes maybeAvsNoPath
    let avs = zipWith (\av f -> setFilePath f av) avsNoPath avFilePaths 
    -- TODO
    -- refactor that shit
    _ <- sequence_ $ map (\av -> sequence_ $
                        map (\actor ->
                                (do 
                                    _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor)
                                    _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor)
                                    _ <- sequence_ $ map (\tag -> (do 
                                                                      _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                      _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                      return ()
                                        )) (getTags av)
                                    return ())
                            ) (getActors av)
                        ) avs
    _ <- sequence_ $ map (\av -> sequence_ $
                        map (\tag ->
                                (do 
                                    _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag)
                                    _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag)
                                    _ <- sequence_ $ map (\actor -> (do 
                                                                      _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                      _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                      return ()
                                        )) (getActors av)
                                    return ())
                            ) (getTags av)
                        ) avs
    -- _ <- sequence_ $ concat $ map (\av -> map (\actor -> createDirectoryIfMissing True (outPutDir </> "actors" </> actor)) (getActors av)) avs
    -- _ <- sequence_ $ concat $ map (\av -> map (\actor -> createShortcut (getFilePath av) (outPutDir </> "actors" </> actor)) (getActors av)) avs
    -- _ <- sequence_ $ concat $ map (\av -> map (\tag -> createDirectoryIfMissing True (outPutDir </> "tags" </> tag)) (getTags av)) avs
    -- _ <- sequence_ $ concat $ map (\av -> map (\tag -> createShortcut (getFilePath av) (outPutDir </> "tags" </> tag)) (getTags av)) avs
    print avs
    where
        groups :: [String]
        groups = ["actors", "tags"] 

partition :: Int -> [a] -> [[a]]
partition n xs = partition' n xs []
  where
    partition' _ [] acc = reverse acc
    partition' n xs acc = partition' n (drop n xs) ((take n xs) : acc)

emptyMaybeAvs :: [IO(Maybe Av)]
emptyMaybeAvs = []

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

createAvTree :: IO ([Av])
createAvTree = do
    _ <- createOutputDir
    currentDir <- getCurrentDirectory
    avIdWithFilePathPairs <- getAvIdFromDir currentDir

    ioMaybeAvs <- AsyncPool.withTaskGroup 2 $ \g -> 
        (do
            as <- sequence $
                (map 
                     (\idFilePair -> 
                             (do 
                                let avId = fst idFilePair
                                let avFilePath = snd idFilePair
                                a1 <- AsyncPool.async g $ createAvTreeHelper currentDir idFilePair
                                return a1
                             )
                      )
                avIdWithFilePathPairs)
            result <- sequence $ map AsyncPool.wait as
            return result
            -- taskResult <- (AsyncPool.mapConcurrently g (\idFilePair -> 
            --             (do 
            --                 let avId = fst idFilePair
            --                 let avFilePath = snd idFilePair
            --                 return $ createAvTreeHelper currentDir idFilePair
            --             )
            --                                               )
            --                   avIdWithFilePathPairs
            --                ) 
            -- return taskResult
        )
    -- ioMaybeAvs' <- ioMaybeAvs
    -- maybeAvs <- sequence ioMaybeAvs
    -- let avs = catMaybes ioMaybeAvs
    -- let avs = catMaybes ioMaybeAvs
    -- maybeAvs <- ioMaybeAvs
    -- maybeAvs <- concat ioMaybeAvs
    -- let maybeAvs' = map (\pair -> do next <- sequence pair; return next; ) maybeAvs 
    -- maybeAvs'' <- sequence $ concat maybeAvs
    -- let maybeAvs = concat $ map (\xs -> sequence xs) ioMaybeAvs
    let avs = catMaybes ioMaybeAvs
    _ <- createJpDisplayGroup avs
    return avs 


data AvTask 
    = CreateAvTask(BChan.BroadcastChan BChan.In (Maybe Av), FilePath, String, FilePath)

queueConfig :: TQueue AvTask -> ImmortalQueue AvTask
queueConfig queue =
    ImmortalQueue
        { qThreadCount = 2
        , qPollWorkerTime = 1000
        , qPop = atomically $ readTQueue queue
        , qPush = atomically . writeTQueue queue
        , qHandler = performTask
        , qFailure = printError
        }
  where
    performTask :: AvTask -> IO ()
    performTask t = case t of
        CreateAvTask (inChan, currentDir, avId, filePath) -> do
            maybeAv <- createAvTreeHelper2 currentDir (avId, filePath)
            surcess <- maybeAv `deepseq` BChan.writeBChan inChan maybeAv
            return ()

    printError :: Exception e => AvTask -> e -> IO ()
    printError t err =
        let description = case t of
                CreateAvTask (inChan, currentDir, avId, filePath) ->
                    "CreateAv"
        in  putStrLn $ "Task `" ++ description ++ "` failed with: " ++ show err

createAvTree4 :: IO ()
createAvTree4 = do
    _ <- createOutputDir
    currentDir <- getCurrentDirectory
    avIdWithFilePathPairs <- getAvIdFromDir currentDir
    queue <- newTQueueIO
    workers <- processImmortalQueue $ queueConfig queue
    inChan <- BChan.newBroadcastChan
    outChan <- BChan.newBChanListener inChan
    let !inputs = (map (\pair -> (inChan, currentDir, fst pair, snd pair)) avIdWithFilePathPairs)
    -- _ <- atomically $ mapM (writeTQueue queue . CreateAvTask) input
    -- _ <- atomically $ mapM (writeTQueue queue . CreateAv) (map (\pair -> (inChan, currentDir, fst pair, snd pair)) avIdWithFilePathPairs)
    -- _ <- closeImmortalQueue workers
    maybeAvs <- loop queue outChan inputs [] (length avIdWithFilePathPairs) 0
    let !avs = catMaybes maybeAvs
    avs `deepseq` createJpDisplayGroup avs
    return ()
    where
        -- TODO
        -- Add timeout control
        -- loop :: BChan.BroadcastChan BChan.Out (Maybe Av) ->
        --         [(BChan.BroadcastChan BChan.In (Maybe Av), FilePath, String, FilePath)] ->
        --         [Maybe Av] -> Int -> Int -> IO ([Maybe Av])
        loop = (\queue outChan inputs avs targetLength times -> 
            (do
                let input = if null inputs then Nothing else Just $ head inputs
                _ <- (case input of
                        Nothing -> return ()
                        Just input -> atomically $ (writeTQueue queue . CreateAvTask) input
                     )
                _ <- threadDelay 3000000
                chanData <- BChan.readBChan outChan
                case chanData of
                    Nothing -> loop queue outChan (tail inputs) avs targetLength (times + 1)
                    Just maybeAv -> 
                        case maybeAv of
                            Nothing -> loop queue outChan (tail inputs) (Nothing : avs) targetLength (times + 1)
                            Just av -> 
                                    if (length avs) + 1 >= targetLength
                                        then return avs
                                        else loop queue outChan (tail inputs) (Just av : avs) targetLength (times + 1)
            )
               )

createJpDisplayGroup :: [Av] -> IO ()
createJpDisplayGroup avs = do
    let jpDisplayGroup = filter (\group -> (length group) > 1) $
                         groupBy (\x y -> levp (getJpDisplay x) (getJpDisplay y) >= 0.5)
                                 (sortBy (\x y -> compare (getJpDisplay x) (getJpDisplay y)) avs)
    _ <- sequence_ $ map (\group -> 
                            (do
                                let headAv = head group
                                let groupDir = outPutDir </> "title-group" </> (trim $ take 25 (getJpDisplay headAv))
                                _ <- createDirectoryIfMissing True groupDir
                                _ <- sequence_ $ map (\av -> 
                                                        (do
                                                            _ <- createShortcut (getFilePath av) groupDir
                                                            return ()
                                                        )
                                                     ) group
                                return ()
                            )
                         ) jpDisplayGroup
    return ()


createAvTreeHelper2 :: FilePath -> (String, FilePath) -> IO (Maybe Av)
createAvTreeHelper2 currentDir idFilePair = 
    (do 
        let avId = fst idFilePair
        let avFilePath = snd idFilePair
        maybeAv <-  dmmGetAv2 avId
        result <- (case maybeAv of
                   Nothing -> return Nothing
                   Just _av -> (do
                                   let !av = setFilePath avFilePath _av
                                   putStrLn $ "[" ++ (getAvId av) ++ "]" ++ " " ++ (getJpDisplay av)
                                   !_ <- sequence_ $ map (\actor ->
                                                               (do 
                                                                   _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor)
                                                                   _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor)
                                                                   _ <- sequence_ $ map (\tag -> (do 
                                                                                                       _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                                                       _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                                                       return ()
                                                                       )) (getTags av)
                                                                   return ())
                                                               ) (getActors av)


                                   _ <- sequence_ $ map (\tag ->
                                                              (do 
                                                                  _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag)
                                                                  _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag)
                                                                  _ <- sequence_ $ map (\actor -> (do 
                                                                                                    _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                                                    _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                                                    return ()
                                                                      )) (getActors av)
                                                                  return ())
                                                               ) (getTags av)

                                   return $ Just av
                               )
                    )
        return $ deepseq maybeAv result
    )

createAvTreeHelper :: FilePath -> (String, FilePath) -> IO (Maybe Av)
createAvTreeHelper currentDir idFilePair = 
    (do 
        let avId = fst idFilePair
        let avFilePath = snd idFilePair
        maybeAv <- dmmGetAv avId
        result <- (case maybeAv of
                   Nothing -> return Nothing
                   Just _av -> (do
                                   let !av = setFilePath avFilePath _av
                                   putStrLn $ "[" ++ (getAvId av) ++ "]" ++ " " ++ (getJpDisplay av)
                                   !_ <- sequence_ $ map (\actor ->
                                                               (do 
                                                                   _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor)
                                                                   _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor)
                                                                   _ <- sequence_ $ map (\tag -> (do 
                                                                                                       _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                                                       _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                                                       return ()
                                                                       )) (getTags av)
                                                                   return ())
                                                               ) (getActors av)


                                   _ <- sequence_ $ map (\tag ->
                                                              (do 
                                                                  _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag)
                                                                  _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag)
                                                                  _ <- sequence_ $ map (\actor -> (do 
                                                                                                    _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                                                    _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                                                    return ()
                                                                      )) (getActors av)
                                                                  return ())
                                                               ) (getTags av)

                                   return $ Just av
                               )
                    )
        return result
    )

createAvTree3 :: IO [Av]
createAvTree3 = do
    _ <- createOutputDir
    currentDir <- getCurrentDirectory
    avIdWithFilePathPairs <- getAvIdFromDir currentDir
    -- TODO
    -- refactor that shit
    maybeAvs <- mapPool numMaxConcurrentThreads 
                 (\idFilePair -> 
                            (do 
                                let avId = fst idFilePair
                                let avFilePath = snd idFilePair
                                !maybeAv <- dmmGetAv avId
                                !result <- (case maybeAv of
                                           Nothing -> return Nothing
                                           Just _av -> (do
                                                           let av = setFilePath avFilePath _av
                                                           putStrLn $ "[" ++ (getAvId av) ++ "]" ++ " " ++ (getJpDisplay av)
                                                           !_ <- sequence_ $ map (\actor ->
                                                                                       (do 
                                                                                           _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor)
                                                                                           _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor)
                                                                                           _ <- sequence_ $ map (\tag -> (do 
                                                                                                                               _ <- createDirectoryIfMissing True (outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                                                                               _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "actors" </> actor </> "tags" </> tag)
                                                                                                                               return ()
                                                                                               )) (getTags av)
                                                                                           return ())
                                                                                       ) (getActors av)


                                                           !_ <- sequence_ $ map (\tag ->
                                                                                      (do 
                                                                                          _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag)
                                                                                          _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag)
                                                                                          _ <- sequence_ $ map (\actor -> (do 
                                                                                                                            _ <- createDirectoryIfMissing True (outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                                                                            _ <- createShortcut (getFilePath av) (currentDir </> outPutDir </> "tags" </> tag </> "actors" </> actor)
                                                                                                                            return ()
                                                                                              )) (getActors av)
                                                                                          return ())
                                                                                       ) (getTags av)

                                                           return $ Just av
                                                       )
                                            )
                                return result

                            )
                 )
                 avIdWithFilePathPairs
    let avs = catMaybes maybeAvs
    let jpDisplayGroup = filter (\group -> (length group) > 1) $
                         groupBy (\x y -> levp (getJpDisplay x) (getJpDisplay y) >= 0.5)
                                 (sortBy (\x y -> compare (getJpDisplay x) (getJpDisplay y)) avs)
    _ <- sequence_ $ map (\group -> 
                            (do
                                let headAv = head group
                                let groupDir = outPutDir </> "title-group" </> take 25 (getJpDisplay headAv)
                                _ <- createDirectoryIfMissing True groupDir
                                _ <- sequence_ $ map (\av -> 
                                                        (do
                                                            _ <- createShortcut (getFilePath av) groupDir
                                                            return ()
                                                        )
                                                     ) group
                                return ()
                            )
                         ) jpDisplayGroup
    return avs 

defaultDbFilePath :: FilePath
defaultDbFilePath = (outPutDir </> "av-db.json")

dumpDb :: [Av] -> IO ()
dumpDb avs = do
    if not (null avs)
        then L8.writeFile defaultDbFilePath (JSON.encode avs)
        else return ()

getAvsFromDb :: IO (Maybe [Av])
getAvsFromDb = do
    s <- L8.readFile defaultDbFilePath 
    return (JSON.decode s)

createCsv :: [Av] -> IO()
createCsv avs = do
    writeFile filePath csvContent
    where
        filePath = outPutDir </> "av.csv"
        firstRaw = "番號,標題,演員"
        csvContent = firstRaw ++ "\n" ++ (concat (map (\av -> (printf "%s,%s,%s\n" (getAvId av) (getJpDisplay av) (intercalate "," (getActors av)))) avs))

-- TODO
-- Fix large memory used because fork too more process.
createShortcut :: FilePath -> FilePath -> IO ()
createShortcut targetPath shortcutDir = do
    _ <- runCommand command
    threadDelay 100000
    return ()
    where powershellCmd :: String
          powershellCmd = "powershell.exe -ExecutionPolicy Bypass -NoLogo -NonInteractive -NoProfile"
          shortcutPath :: FilePath
          shortcutPath = shortcutDir </> (take 25 ((takeFileName targetPath) ++ ".lnk"))
          command :: String
          command = (printf "%s -Command \"$PSDefaultParameterValues['Out-File:Encoding'] = 'utf8'; $ws = New-Object -ComObject WScript.Shell; $s = $ws.CreateShortcut('%s'); $s.TargetPath = '%s'; $s.Save()\""
                    powershellCmd shortcutPath targetPath)
