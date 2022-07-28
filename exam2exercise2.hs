main :: IO()

main = do
    print $ cP [Present, Absent, Absent] == False
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False


cP :: StudentRecord -> Bool
cP = canPass (1,2)

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (n, k) = (\ studrec -> canPass' studrec 0 0)
 where
  canPass' [] absent late = absent <= n && late <= k 
  canPass' (r:rec) absCount lateCount
    | absCount > n || lateCount > k = False 
    | r == Absent = canPass' rec (absCount + 1) 0
    | r == Late =   canPass' rec (absCount) (lateCount + 1)
    | otherwise =   canPass' rec (absCount) 0
 
type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present
 deriving(Eq)
type StudentRecord = [Attendance]
