-- 1 準備

-- 名前のモデル化
data Name = Name { firstName :: String, lastName :: String }
instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

-- 学年のモデル化
data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq, Ord, Enum, Show)

-- Studentのモデル化
data Student = Student { studentId :: Int, gradeLevel :: GradeLevel, studentName::Name } deriving Show

-- データベースのモデル化　リストで表現
students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde")),
            (Student 2 Junior (Name "Leslie" "Silko")),
            (Student 3 Freshman (Name "Judith" "Butler")),
            (Student 4 Senior (Name "Guy" "Debord")),
            (Student 5 Sophomore (Name "Jean" "Baudrillard")),
            (Student 6 Junior (Name "Julia" "Kristeva"))
            ]




-- 2 リストに対する基本的なクエリの準備

-- _select関数は単なるfmapである
_select :: ( a -> b ) -> [a] -> [b]
_select prop vals = do
    val <- vals
    return (prop val)

{-
fmap（または map）と同じことを、do記法とリストモナドを使って表現している。
実質的には map prop vals と同じ処理。

prop
これは関数です。引数として受け取ります。

vals
これはリストです。これも引数で受け取ります。

do val <- vals
これは リストモナド の文法で、vals から要素を1つずつ取り出して val に代入します。
裏ではリスト内包と同じ意味になります。

ex
vals = [1,2,3] のとき：
val = 1
val = 2
val = 3
と3回繰り返されます。

return (prop val)
val に prop（関数）を適用して、prop val を計算する。
return は、リストモナドという文脈なので、「prop val をリストとして返す」という意味になる。
例：return 2 = [2]
各 val に対して [prop val] が作られ、すべてが結合されて最終的な [b] ができる。
ex
_select (*2) [1,2,3]
→
val = 1 → return (1*2) = [2]
val = 2 → return (2*2) = [4]
val = 3 → return (3*2) = [6]


do 記法（糖衣構文）の背後で どのように処理が進んでいるか？

やってることはmapと同じ！！！
map f xs = do { x <- xs; return (f x) }
_select は map と同じ操作を リストモナド + do記法 で書いたバージョンです。

実行例
_select (*2) [1,2,3]
→
[2,4,6]

関数を適用した結果を : で cons するのは、その都度すぐ です。すべての関数適用が終わってから一気に cons されるわけではありません。
_select (+1) [1,2,3]
= 2 : 3 : 4 : []

-}

-- _where
import Control.Monad

_where :: ( a -> Bool ) -> [a] -> [a]
_where test vals = do
    val <- vals
    guard (test val)
    return val

{-
guard
guard :: (Alternative f) => Bool -> f ()
guard False = empty
guard True = pure () のように動く。

guard (test val)
test val を評価し、True ならそのまま処理を継続。
False ならそのルート（その val）を捨てる（＝無視する）。

guard の戻り値は () だが、副作用として フィルタリングの効果を持つ！
guard は「条件を満たさないとそのパスを打ち切る」という意味で、モナドで条件分岐をするための道具です。
-}

-- _whereに渡すヘルパー関数の例
startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)





-- 3 joinを実装するために、追加でテーブルを準備。

-- Teacherのモデル化
data Teacher = Teacher { teacherId :: Int, teacherName :: Name } deriving Show

-- Teacherのテーブルをリストでモデル化
teachers::[Teacher]
teachers = [ Teacher 100 (Name "Simone" "DeBeauvior"), Teacher 200 (Name "Susan" "Sontag")]

-- Courseのモデル化
data Course = Course { courseId::Int, courseTitle::String, teacher::Int } deriving Show

-- Courseのテーブルをリストでモデル化
courses::[Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

-- 内部結合
_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1, d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs

--　似たSQL
-- SELECT * FROM data1, data2 WHERE data1.id = data2.id;





-- 4 クエリの組み立てをSQLに近づける

-- _hinq関数は、式の書き方をSQLに近づける
-- ただし、where句がない場合に対処できない。Haskellではデフォルト引数も使えない。　
_hinq selectQuery joinQuery whereQuery = (\joinData ->
                                            (\whereResult ->
                                                selectQuery whereResult)
                                            (whereQuery joinData)
                                        ) joinQuery
{-
joinQuery を whereQuery に通し、それを selectQuery に渡す
という 3ステップの処理を、関数合成せずにラムダ式で書いたものです。

_hinq s j w = s (w j)
これをラムダ式で書いていたので少し見慣れないだけでした！

ラムダ式の順序
joinData = joinQuery
whereResult = whereQuery joinQuery
output = selectQuery (whereQuery joinQuery)

joinQueryの例
joinQuery = [("Alice", 1), ("Bob", 2), ("Carol", 3)]

実行例
selectQuery = map fst
joinQuery = [("Alice", 1), ("Bob", 2), ("Carol", 3)]
whereQuery = filter (\(_, id) -> id == 1)
result = _hinq selectQuery joinQuery whereQuery
=> ["Alice"]

ステップ 1：関数に joinQuery を渡す
→ (\whereResult -> selectQuery whereResult) (whereQuery [("Alice", 1), ("Bob", 2), ("Carol", 3)])

ステップ 2：whereQuery を適用する
filter (\(_, id) -> id == 1) [("Alice", 1), ("Bob", 2), ("Carol", 3)]
→ [("Alice", 1)] -- id が 1 のものだけ残る
結果
(\whereResult -> selectQuery whereResult) [("Alice", 1)]

ステップ 3：selectQuery を適用する
selectQuery [("Alice", 1)]
→ map fst [("Alice", 1)]
→ ["Alice"]

まとめ
_hinq select join where
→ (\j -> (\w -> select w) (where j)) join
→ (\w -> select w) (where join)
→ select (where join)
-}




-- 5 クエリを表すHINQ型を作成する

-- 準備
-- リストからモナドへ一般化
-- 型クラス制約を追加必要。
import Control.Applicative
-- _select :: ( a -> b ) -> [a] -> [b]
_select :: Monad m => ( a -> b ) -> m a -> m b

-- _where :: ( a -> Bool ) -> [a] -> [a]
_where :: ( Monad m, Alternative m ) => (a -> Bool) -> m a -> m a
-- Alternative m　　フィルターのような「値を通す／落とす」制御がしたいとき
{-
class Applicative f => Alternative f where
    empty  :: f a
    (<|>)  :: f a -> f a -> f a

　empty は「失敗」や「空っぽ」を表します。
　<|> は「選択肢の中から成功するものを選ぶ」操作です。

リストをフィルターする例
_where even [1,2,3,4]  -- => [2,4]
→中身の処理はこれ
do val <- [1,2,3,4]
   guard (even val)
   return val
-}

-- _join :: [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)



finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") .courseTitle . snd))