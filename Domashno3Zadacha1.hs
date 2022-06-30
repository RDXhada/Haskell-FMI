import Data.List
main :: IO()
main = do
    print $ "Homework 3 exercise 1 by Dzhem Shenolov"
    print $ rf (Song "Mozart""The Marriage of Figaro Overture" 270) == "Summertime"
    print $ rf (Song "Gershwin""Summertime" 300) == "Rhapsody in Blue"
    print $ rf (Song "Queen""Bohemian Rhapsody" 355) -- Rhapsody in Blue, bonus test case
    print $ rf (Song "Gershwin""Rhapsody in Blue" 1100) == "Rhapsody in Blue"

type AuthorName = String
type SongName = String
type SongLength = Int
data Song = Song AuthorName SongName SongLength deriving (Eq)
data Playlist = Playlist [Song]

rf :: Song -> SongName
rf = recommender (Playlist songs)

recommender :: Playlist -> (Song -> SongName)
recommender (Playlist pl) (Song authorName' songName' songLength') 
    | [ sng | (Song auth sng len) <- pl, authorName' == auth && len > songLength' ] /= [] = head [ sng | (Song auth sng len) <- pl, authorName' == auth && len > songLength' ]
    | [ sng | (Song auth sng len) <- pl, len > songLength' ] /= [] = head [ sng | (Song auth sng len) <- pl, len > songLength' ] 
    | otherwise = songName'

songs :: [Song]
songs = [
    (Song "Mozart""The Marriage of Figaro Overture" 270),
    (Song "Gershwin""Summertime" 300),
    (Song "Queen""Bohemian Rhapsody" 355),
    (Song "Gershwin""Rhapsody in Blue" 1100)]
