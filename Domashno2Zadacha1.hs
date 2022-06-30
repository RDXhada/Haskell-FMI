import Data.List
main :: IO()
main = do
    print $ sunkK 0 database == [("Germany",0),("Gt.Britain",0)]
    print $ sunkK 2 database == [("Germany",0),("Japan",2),("Gt.Britain",0)]
    print $ sunkK 9 database == [("Germany",0),("USA",4),("Japan",2),("Gt.Britain",0)]
    print $ sunkK 20 database == [("Germany",0),("USA",4),("Japan",2),("Gt.Britain",0)]
    print $ sunkK 15 database == [("Germany",0),("USA",3),("Japan",2),("Gt.Britain",0)] -- false condition (i put it on purpose)
  

sunkK :: Int -> Database -> [(Name, Int)]
--out - outcomes
--bts - battles
-- sps - ships
-- out - outcomes
sunkK k (out, bts, sps, cls) = [(countryName, getSunkShipsNumber countryName) | countryName <- allClassNames, inCondition countryName]
 where
    allClassNames :: [Name]
    allClassNames = nub [classes | ( _ , _ , classes) <- cls]
--technically we do not need battles database

    getSunkShipsNumber :: Name -> Int
    getSunkShipsNumber byCountry = length [shipsPlaces | (shipsPlaces, _, result) <- out,(shipsPlaces2, classNames2, _) <- sps, (classNames, _ , countryName) <- cls, result == "sunk" && countryName == byCountry && classNames == classNames2 && shipsPlaces == shipsPlaces2]

    inCondition :: Name -> Bool
    inCondition insertCondition = getSunkShipsNumber insertCondition <= k


type Name = String
type Date = String
type Class = String
type Type = String
type Result = String
type Launched = Int
type Battle = (Name, Date)
type Ship = (Name, Class, Launched)
type Outcome = (Name, Name, Result)
type ShipClass = (Name, Type, Name)
type Database = ([Outcome], [Battle], [Ship], [ShipClass])

outcomes :: [Outcome]
outcomes = [
    ("Bismarck", "North Atlantic", "sunk"),
    ("California", "Surigao Strait", "ok"), 
    ("Duke of York","North Cape", "ok"), 
    ("Fuso", "Surigao Strait", "sunk"),
    ("Hood", "North Atlantic", "sunk"),
    ("King George V", "NorthAtlantic", "ok"), 
    ("Kirishima", "Guadalcanal", "sunk"),
    ("Prince of Wales", "North Atlantic", "damaged"), 
    ("Rodney","North Atlantic", "ok"), 
    ("Schamhorst", "North Cape","sunk"), 
    ("South Dakota", "Guadalcanal", "damaged"),
    ("Tennessee", "Surigao Strait", "ok"), 
    ("Washington","Guadalcanal", "ok"), 
    ("Prince of Wales", "Guadalcanal","ok"), 
    ("West Virginia", "Surigao Strait", "ok"),
    ("Yamashiro", "Surigao Strait", "sunk"), 
    ("California","Guadalcanal", "damaged") ]

battles :: [Battle]
battles = [ 
    ("Guadalcanal", "1942-11-15"), 
    ("North Atlantic","1941-05-25"),
    ("North Cape", "1943-12-26"),
    ("Surigao Strait", "1944-10-25") ]

ships :: [Ship]
ships = [ 
    ("California", "Tennessee", 1921),
    ("Haruna","Kongo", 1916), 
    ("Hiei", "Kongo", 1914), 
    ("Iowa", "Iowa", 1943),
    ("Kirishima", "Kongo", 1915), 
    ("Kongo", "Kongo", 1913),
    ("Missouri", "Iowa", 1944), 
    ("Musashi", "Yamato", 1942), 
    ("New Jersey", "Iowa", 1943), 
    ("North Carolina", "North Carolina", 1941), 
    ("Ramillies", "Revenge", 1917), 
    ("Renown", "Renown",1916), 
    ("Repulse", "Renown", 1916), 
    ("Resolution", "Renown", 1916), 
    ("Revenge", "Revenge", 1916), 
    ("Royal Oak", "Revenge", 1916), 
    ("Royal Sovereign", "Revenge", 1916), 
    ("Tennessee","Tennessee", 1920), 
    ("Washington", "North Carolina", 1941),
    ("Wisconsin", "Iowa", 1944), 
    ("Yamato", "Yamato", 1941),
    ("Yamashiro", "Yamato", 1947), 
    ("South Dakota", "North Carolina", 1941), 
    ("Bismarck", "North Carolina", 1911), 
    ("Duke of York", "Renown", 1916), 
    ("Fuso", "Iowa", 1940), 
    ("Hood", "Iowa", 1942), 
    ("Rodney", "Yamato", 1915), 
    ("Yanashiro", "Yamato", 1918), 
    ("Schamhorst", "North Carolina", 1917), 
    ("Prince of Wales", "North Carolina", 1937), 
    ("King George V","Iowa", 1942), 
    ("West Virginia", "Iowa", 1942) ]

classes :: [ShipClass]
classes = [
    ("Bismarck", "bb", "Germany"), 
    ("Iowa", "bb", "USA"), 
    ("Kongo", "bc", "Japan"), 
    ("North Carolina", "bb", "USA"), 
    ("Renown", "bc", "Gt.Britain"), 
    ("Revenge", "bb", "Gt.Britain"), 
    ("Tennessee", "bb", "USA"), 
    ("Yamato", "bb", "Japan")]

database :: Database
database = (outcomes, battles, ships, classes)
