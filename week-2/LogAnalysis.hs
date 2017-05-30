module LogAnalysis where

import Log

parseInfoMessage :: String -> LogMessage
parseInfoMessage message = LogMessage Info timestamp message_string
  where timestamp = read . head $ words(message)
        message_string = unwords . drop 1 $ words(message)

parseWarningMessage :: String -> LogMessage
parseWarningMessage message = LogMessage Warning timestamp message_string
  where timestamp = read . head $ words(message)
        message_string = unwords . drop 1 $ words(message)

parseErrorMessage :: String -> LogMessage
parseErrorMessage message = LogMessage (Error severity) timestamp message_string
  where timestamp = read . head . drop 1 $ words(message)
        severity = read . head $ words(message)
        message_string = unwords . drop 2 $ words(message)

parseMessage :: String -> LogMessage
parseMessage message
  | x == 'I' = parseInfoMessage xs
  | x == 'W' = parseWarningMessage xs
  | x == 'E' = parseErrorMessage xs
  | otherwise = Unknown message
  where x = head message
        xs = tail message

parse :: String -> [LogMessage]
parse messages = map parseMessage $ lines(messages)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) message_tree = message_tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ ts _) (Node ltree (LogMessage t message_ts str) rtree)
  | ts < message_ts = Node (insert message ltree) (LogMessage t message_ts str) rtree
  | ts > message_ts = Node ltree (LogMessage t message_ts str) (insert message rtree)

build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree message Leaf)  = inOrder(ltree) ++ [message]
inOrder (Node Leaf message rtree)  = [message] ++ inOrder(rtree)
inOrder (Node ltree message rtree) =  inOrder(ltree) ++ [message] ++ inOrder(rtree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = [m | (LogMessage (Error s) ts m) <- inOrder . build $ logMessages, s >= 90]
