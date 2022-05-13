import Data.List
main :: IO()
main = do
    --task 1
    print $ TwoD 5 6 == TwoD 7 8
    print $ ThreeD 5 6 7
    --task 2
    print $ findJudge 2 [(1, 2)] == 2
    print $ findJudge 3 [(1, 3), (2, 3)] == 3
    print $ findJudge 3 [(1, 3), (2, 3), (3, 1)] == -1
    print $ findJudge 3 [(1, 2), (2, 3)] == -1
    print $ findJudge 4 [(1, 3), (1, 4), (2, 3), (2, 4), (4, 3)] == 3
    
    --task 3
    print $ getSunk database == [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
   
    --task 4
    print $ getFeaturedStars "MGM" 1995 db == ["Jack Nicholson", "Sandra Bulloc"]
    print $ getFeaturedStars "USA Entertainm." 2001 db == ["Billy Bob Thornton", "Scarlett Johansson", "Orlando Bloom", "Cate Blanchett", "Liv Tyler"]

    print $ getPresident "Paramount" db == "Calvin Coolidge"
    print $ getPresident "Fox" db == "Ted Turner"
    print $ getPresident "USA Entertainm." db == "Stephen Spielberg"
--task 1
data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq)

 --task 2
findJudge :: Int -> [(Int, Int)] -> Int
findJudge n g = convert $ filter isJudge [1 .. n]
 where 
    convert :: [Int] -> Int
    convert [] = -1
    convert xs = head xs

    isJudge :: Int -> Bool
    isJudge x = trustsNobody x && trustedByEvryone x

    trustsNobody :: Int -> Bool
    trustsNobody x = null [ f | (f, _) <- g, f == x ]

    trustedByEvryone :: Int -> Bool
    trustedByEvryone x = n - 1 == (length $ nub $ map fst $ filter (\ (f, s) -> s == x) g)


--task 3

getSunk :: Database -> [(Name, [Name])]
getSunk (os, bs, ss) = [ (name, getSunkShips name) | name <- allBattleNames ]
 where
     allBattleNames :: [Name]
     allBattleNames = [ name | (Battle name _) <- bs ]

     allBattleNames' :: [Name]
     allBattleNames' = nub [ bN | (Outcome _ bN _) <- os ]

     getSunkShips :: Name -> [Name]
     getSunkShips battleName = [ ship | (Outcome ship bN result) <- os, result == "sunk" && bN == battleName ]


type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int

data Battle = Battle Name Date
 deriving (Show)
data Ship = Ship Name Class Launched
 deriving (Show)
data Outcome = Outcome Name Name Result
 deriving (Show)

type Database = ([Outcome], [Battle], [Ship])

outcomes :: [Outcome]
outcomes = [
    Outcome "Bismarck" "North Atlantic" "sunk",
    Outcome "California" "Surigao Strait" "ok",
    Outcome "Duke of York" "North Cape" "ok",
    Outcome "Fuso" "Surigao Strait" "sunk",
    Outcome "Hood" "North Atlantic" "sunk",
    Outcome "King George V" "North Atlantic" "ok",
    Outcome "Kirishima" "Guadalcanal" "sunk",
    Outcome "Prince of Wales" "North Atlantic" "damaged",
    Outcome "Rodney" "North Atlantic" "ok",
    Outcome "Schamhorst" "North Cape" "sunk",
    Outcome "South Dakota" "Guadalcanal" "damaged",
    Outcome "Tennessee" "Surigao Strait" "ok",
    Outcome "Washington" "Guadalcanal" "ok",
    Outcome "Prince of Wales" "Guadalcanal" "ok",
    Outcome "West Virginia" "Surigao Strait" "ok",
    Outcome "Yamashiro" "Surigao Strait" "sunk",
    Outcome "California" "Guadalcanal" "damaged"]

battles :: [Battle]
battles = [
    Battle "Guadalcanal" "1942-11-15",
    Battle "North Atlantic" "1941-05-25",
    Battle "North Cape" "1943-12-26",
    Battle "Surigao Strait" "1944-10-25" ]

ships :: [Ship]
ships = [
    Ship "California" "Tennessee" 1921,
    Ship "Haruna" "Kongo" 1916,
    Ship "Hiei" "Kongo" 1914,
    Ship "Iowa" "Iowa" 1943,
    Ship "Kirishima" "Kongo" 1915,
    Ship "Kongo" "Kongo" 1913,
    Ship "Missouri" "Iowa" 1944,
    Ship "Musashi" "Yamato" 1942,
    Ship "New Jersey" "Iowa" 1943,
    Ship "North Carolina" "North Carolina" 1941,
    Ship "Ramillies" "Revenge" 1917,
    Ship "Renown" "Renown" 1916,
    Ship "Repulse" "Renown" 1916,
    Ship "Resolution" "Renown" 1916,
    Ship "Revenge" "Revenge" 1916,
    Ship "Royal Oak" "Revenge" 1916,
    Ship "Royal Sovereign" "Revenge" 1916,
    Ship "Tennessee" "Tennessee" 1920,
    Ship "Washington" "North Carolina" 1941,
    Ship "Wisconsin" "Iowa" 1944,
    Ship "Yamato" "Yamato" 1941,
    Ship "Yamashiro" "Yamato" 1947,
    Ship "South Dakota" "North Carolina" 1941,
    Ship "Bismarck" "North Carolina" 1911,
    Ship "Duke of York" "Renown" 1916,
    Ship "Fuso" "Iowa" 1940,
    Ship "Hood" "Iowa" 1942,
    Ship "Rodney" "Yamato" 1915,
    Ship "Yanashiro" "Yamato" 1918,
    Ship "Schamhorst" "North Carolina" 1917,
    Ship "Prince of Wales" "North Carolina" 1937,
    Ship "King George V" "Iowa" 1942,
    Ship "West Virginia" "Iowa" 1942 ]

database :: Database
database = (outcomes, battles, ships)

--task 4
-- [ actorName | (Movie titleM year _ studio _) <- ms, (StarsIn actorName title) <- si, title == titleM && studoName == studio && movieYear == year ]
getFeaturedStars :: Name -> Year -> MovieDB -> [Name]
getFeaturedStars studoName movieYear (ms, _, si, _, _) = concatMap getActors getMovies
 where
     getMovies :: [Title]
     getMovies = map (\ (Movie title _ _ _ _) -> title) $ filter (\ (Movie title year _ studio _) -> studoName == studio && movieYear == year) ms
    --  getMovies = [ title | (Movie title year _ studio _) <- ms, studoName == studio && movieYear == year ]

     getActors :: Title -> [Name]
     getActors movie = map (\ (StarsIn actorName _) -> actorName) $ filter (\ (StarsIn _ title) -> title == movie) si
    --  getActors movie = [ actorName | (StarsIn actorName title) <- si, title == movie ]

getPresident :: Name -> MovieDB -> Name
getPresident studioName (_, _, _, ss, mes) = getName' getId'
 where
     getId = head [ pId | (Studio name pId) <- ss, name == studioName ]
     getId' = head $ map (\ (Studio _ pId) -> pId) $ filter (\ (Studio name _) -> name == studioName) ss
     
     getName id = head [ name | (MovieExec name pId _) <- mes, pId == id ]
     getName' id = head $ map (\ (MovieExec name _ _) -> name) $ filter (\ (MovieExec _ pId _) -> pId == id) mes

-- ["film1", "film2"]
-- concat $ map [[], []]

--type Name = String
type Title = String
type Address = String
type Year = Int
type Gender = Char
type Length = Int

type ProducerID = Int
type Networth = Integer

data Movie = Movie Title Year Length Name ProducerID
 deriving (Show)
data MovieStar = MovieStar Name Gender
 deriving (Show)
data StarsIn = StarsIn Name Title
 deriving (Show)

data Studio = Studio Name Int
 deriving (Show)
data MovieExec = MovieExec Name ProducerID Networth
 deriving (Show)

type MovieDB = ([Movie], [MovieStar], [StarsIn], [Studio], [MovieExec])

studios :: [Studio]
studios = [Studio "Disney" 199,
    Studio "USA Entertainm." 222,
    Studio "Fox" 333,
    Studio "Paramount" 123,
    Studio "MGM" 555]

movieExecs :: [MovieExec]
movieExecs = [MovieExec "George Lucas" 555 200000000,
    MovieExec "Ted Turner" 333 125000000,
    MovieExec "Stephen Spielberg" 222 100000000,
    MovieExec "Merv Griffin" 199 112000000,
    MovieExec "Calvin Coolidge" 123 20000000]

movies :: [Movie]
movies = [Movie "Pretty Woman" 1990 119 "Disney" 199,
    Movie "The Man Who Wasn't There" 2001 116 "USA Entertainm." 555,
    Movie "Logan's run" 1976 120 "Fox" 333,
    Movie "Star Wars" 1977 124 "Fox" 555,
    Movie "Empire Strikes Back" 1980 111 "Fox" 555,
    Movie "Star Trek" 1979 132 "Paramount" 222,
    Movie "Star Trek: Nemesis" 2002 116 "Paramount" 123,
    Movie "Terms of Endearment" 1983 132 "MGM" 123,
    Movie "The Usual Suspects" 1995 106 "MGM" 199,
    Movie "Gone With the Wind" 1938 238 "MGM" 123,
    Movie "The Fellowship of the Ring" 2001 178 "USA Entertainm." 222]

stars :: [MovieStar]
stars = [MovieStar "Jane Fonda" 'F',
    MovieStar "Alec Baldwin" 'M',
    MovieStar "Kim Basinger" 'F',
    MovieStar "Harrison Ford" 'M',
    MovieStar "Debra Winger" 'F',
    MovieStar "Jack Nicholson" 'M',
    MovieStar "Sandra Bullock" 'F',
    MovieStar "Orlando Bloom" 'M',
    MovieStar "Cate Blanchett" 'F',
    MovieStar "Liv Tyler" 'F',
    MovieStar "Billy Bob Thornton" 'M',
    MovieStar "Scarlett Johansson" 'F']

starsIn :: [StarsIn]
starsIn = [StarsIn "Kim Basinger" "Star Wars",
    StarsIn "Alec Baldwin" "Star Wars",
    StarsIn "Harrison Ford" "Star Wars",
    StarsIn "Harrison Ford" "Empire Strikes Back",
    StarsIn "Jack Nicholson" "The Usual Suspects",
    StarsIn "Jane Fonda" "Terms of Endearment",
    StarsIn "Jack Nicholson" "Terms of Endearment",
    StarsIn "Sandra Bulloc" "The Usual Suspects",
    StarsIn "Billy Bob Thornton" "The Man Who Wasn't There",
    StarsIn "Scarlett Johansson" "The Man Who Wasn't There",
    StarsIn "Orlando Bloom" "The Fellowship of the Ring",
    StarsIn "Cate Blanchett" "The Fellowship of the Ring",
    StarsIn "Liv Tyler" "The Fellowship of the Ring"]

db :: MovieDB
db = (movies, stars, starsIn, studios, movieExecs)
