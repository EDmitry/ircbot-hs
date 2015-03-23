{-# LANGUAGE OverloadedStrings #-}
module GitLab where
       
import Data.Aeson
import Control.Applicative
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL

decode :: FromJSON a => BL.ByteString -> Maybe a
decode = Data.Aeson.decode

data Commit = Commit { id :: String
                     , message :: String
                     , author :: String
                     , email :: String  
                     , url :: String  
                     } deriving (Eq)

instance Show Commit where
  show x = mconcat [author x, " (", email x, ") committed: ", message x]

instance FromJSON Commit where
  parseJSON (Object o) = Commit <$>
                         o .: "id" <*>
                         o .: "message" <*>
                         ((o .: "author") >>= (.: "name")) <*>
                         ((o .: "author") >>= (.: "email")) <*>
                         o .: "url"
                         
data CommitHook = CommitHook { ref :: String
                             , projectId :: Int
                             , commits :: [Commit]
                             } deriving (Show, Eq) 

instance FromJSON CommitHook where
  parseJSON (Object o) = CommitHook <$>
                         o .: "ref" <*>
                         o .: "project_id" <*>
                         (o .: "commits" >>= parseJSON)
