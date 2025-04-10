data Name = Name { firstName ::String, lastName ::String }
instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student { studentId :: Int, gradeLevel :: GradeLevel, studentName::Name} deriving Show

students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde")),
            (Student 2 Junior (Name "Leslie" "Silko")),
            (Student 3 Freshman (Name "Judith" "Butler")),
            (Student 4 Senior (Name "Guy" "Debord")),
            (Student 5 Sophomore (Name "Jean" "Baudrillard")),
            (Student 6 Junior (Name "Julia" "Kristeva"))
            ]

-- _select関数は単なるfmapである
_select :: ( a -> b ) -> [a] -> [b]
_select prop vals = do
    val <- vals
    return (prop val)

{-
val <- vals
意味： vals というリストから一つずつ val を取り出す。

これは vals = [1,2,3] のとき：
val = 1
val = 2
val = 3
と3回繰り返されます（暗黙に繰り返される！）
-}

{-
return (prop val)
val に prop（関数）を適用して、prop val を計算する。
return は、「prop val をリストとして返す」という意味になる（リストモナドなので）。
例：return 2 = [2]
各 val に対して [prop val] が作られ、すべてが結合されて最終的な [b] ができる。
-}

{-
やってることはmapと同じ！！！
map f xs ≡ do { x <- xs; return (f x) }

_select は map と同じ操作を リストモナド + do記法 で書いたバージョンです。
-}

{-
実行例
_select (*2) [1,2,3]
→
[2,4,6]
-}

-- クエリのフィルタリングが可能
import Control.Monad

_where :: ( a -> Bool ) -> [a] -> [a]
_where test vals = do
    val <- vals
    guard (test val)
    return val

-- Haskell の リストモナド + guard関数 を使って、filter のような動作を実現する関数 _where を定義しています。

{-
guard (test val)
test val を評価し、True ならそのまま処理を継続。

False ならそのルート（その val）を捨てる（＝無視する）。

guard の戻り値は () だが、副作用として フィルタリングの効果を持つ！

guard は「条件を満たさないと、そのパスを打ち切る」という意味で、モナドで条件分岐をするための道具です。
-}

startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)

data Teacher = Teacher { teacherId::Int, teacherName::Name } deriving Show

teachers::[Teacher]
teachers = [ Teacher 100 (Name "Simone" "DeBeauvior"), Teacher 200 (Name "Susan" "Sontag")]

data Course = Course { courseId::Int, courseTitle::String, teacher::Int } deriving Show

courses::[Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

_join :: [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1, d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

--　似たSQL
-- SELECT * FROM data1, data2 WHERE data1.id = data2.id;

-- _hinq関数はクエリの再構築を可能にする
_hinq selectQuery joinQuery whereQuery = (\joinData ->
                                            (\whereResult ->
                                                selectQuery whereResult)
                                            (whereQuery joinData)
                                        ) joinQuery

{-
joinQueryの例
joinQuery = [("Alice", 1), ("Bob", 2), ("Carol", 3)]
-}

{-
実行例

selectQuery = map fst
joinQuery = [("Alice", 1), ("Bob", 2), ("Carol", 3)]
whereQuery = filter (\(_, id) -> id == 1)

result = _hinq selectQuery joinQuery whereQuery
=> ["Alice"]
-}

{-
イメージは、_hinq select join where = select (where join)
-}