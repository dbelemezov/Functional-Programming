main :: IO()
main = do
    print $ getFeaturedStars "MGM" 1995 db == ["Jack Nicholson", "Sandra Bulloc"]
    print $ getFeaturedStars "USA Entertainm." 2001 db == ["Billy Bob Thornton","Scarlett Johansson", "Orlando Bloom", "Cate Blanchett", "Liv Tyler"]
    print $ getPresident "Paramount" db == "Calvin Coolidge"
    print $ getPresident "Fox" db == "Ted Turner"
    print $ getPresident "USA Entertainm." db == "Stephen Spielberg"
    print $ getHigherProductions "Calvin Coolidge" db == ["Pretty Woman","The Man Who Wasn't There","Logan's run","Star Wars","Empire Strikes Back","Star Trek","The Usual Suspects","The Fellowship of the Ring"]
    print $ getHigherProductions "Stephen Spielberg" db == ["Pretty Woman","The Man Who Wasn't There","Logan's run","Star Wars","Empire Strikes Back","The Usual Suspects"]
    print $ getHigherProductions "George Lucas" db == []
    print $ toBinaryIndexed t1 == t1result
    print $ toBinaryIndexed t2 == t2result

type Name = String
type Title = String
type Address = String
type Year = Int
type Gender = Char
type Length = Int
type ProducerID = Int
type Networth = Integer

data Movie = Movie Title Year Length Name ProducerID
  deriving (Show, Eq)
data MovieStar = MovieStar Name Gender
  deriving (Show, Eq)
data StarsIn = StarsIn Name Title
  deriving (Show, Eq)
data Studio = Studio Name Int
  deriving (Show, Eq)
data MovieExec = MovieExec Name ProducerID Networth
  deriving (Show, Eq)

type MovieDB = ([Movie], [MovieStar], [StarsIn], [Studio], [MovieExec])

studios :: [Studio]
studios = 
    [Studio "Disney" 199,
    Studio "USA Entertainm." 222,
    Studio "Fox" 333,
    Studio "Paramount" 123,
    Studio "MGM" 555]
movieExecs :: [MovieExec]
movieExecs = 
    [MovieExec "George Lucas" 555 200000000,
    MovieExec "Ted Turner" 333 125000000,
    MovieExec "Stephen Spielberg" 222 100000000,
    MovieExec "Merv Griffin" 199 112000000,
    MovieExec "Calvin Coolidge" 123 20000000]
movies :: [Movie]
movies = 
    [Movie "Pretty Woman" 1990 119 "Disney" 199,
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
stars = 
    [MovieStar "Jane Fonda" 'F',
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
starsIn = 
    [StarsIn "Kim Basinger" "Star Wars",
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

getTitle ::  Movie -> Title
getTitle (Movie a b c d e) = a

getYear :: Movie -> Year
getYear (Movie a b c d e) = b

getStudioName :: Movie -> Name
getStudioName (Movie a b c d e) = d

moviesInList :: MovieDB -> [Movie]
moviesInList (a, b, c, d, e) = a

getTitles :: [Movie] -> [Title]
getTitles [] = []
getTitles xs = getTitle(head xs) : getTitles (tail xs)

getMoviesByyearAndStudio :: Name -> Int -> MovieDB -> [Title]
getMoviesByyearAndStudio studio year (a, b, c, d, e) = getTitles $ filter(\x ->(getStudioName x)==studio && (getYear x)==year) (moviesInList(a, b, c, d, e))

starsInList :: MovieDB -> [StarsIn]
starsInList (a, b, c, d, e) = c

getStar :: StarsIn -> Name
getStar (StarsIn a b) = a

getStars :: [StarsIn] -> [Name]
getStars [] = []
getStars xs = getStar(head xs) : getStars (tail xs)

gettitleByStarsIn :: StarsIn -> Title
gettitleByStarsIn (StarsIn a b) = b

getFeaturedStars :: Name -> Int -> MovieDB -> [Name]
getFeaturedStars studio year (a, b, c, d, e) =  getStars $ filter(\x ->elem (gettitleByStarsIn x) (getMoviesByyearAndStudio studio year (a, b, c, d, e)) ) (starsInList(a, b, c, d, e))

-----------------------------------------------------------------------------------------------------------------

getStudioNumber :: Studio -> Int
getStudioNumber (Studio a b) = b

getProducer :: MovieExec -> Name
getProducer (MovieExec a b c) = a

getNumberByMovieExec :: MovieExec -> Int
getNumberByMovieExec (MovieExec a b c) = b

movieExecsInList :: MovieDB -> [MovieExec]
movieExecsInList (a, b, c, d, e) = e

getStudios :: MovieDB -> [Studio]
getStudios (a, b, c, d, e) = d

getStudioName1 :: Studio -> Name
getStudioName1 (Studio a b) = a

getStudioByName :: Name -> MovieDB -> Studio
getStudioByName name (a, b, c, d, e) = head (filter(\x -> (getStudioName1 x) == name )(getStudios(a, b, c, d, e)))

getPresident :: Name -> MovieDB -> Name
getPresident studio (a, b, c, d, e) = getProducer $ head $ filter (\x -> (getNumberByMovieExec x) == getStudioNumber(getStudioByName studio  (a, b, c, d, e))) (movieExecsInList (a, b, c, d, e))

-----------------------------------------------------------------------------------------------------------------

getNetworthByMovieExec :: MovieExec -> Integer
getNetworthByMovieExec (MovieExec a b c) = c

getMovieExecByName :: Name -> MovieDB -> MovieExec
getMovieExecByName name (a, b, c, d, e) = head (filter(\x -> (getProducer x) == name )(movieExecsInList(a, b, c, d, e)))

getNumbers :: [MovieExec] -> [Int]
getNumbers [] = []
getNumbers xs = getNumberByMovieExec(head xs) : getNumbers (tail xs)

getExecsBiggerNumber :: Name -> MovieDB -> [Int]
getExecsBiggerNumber producer (a, b, c, d, e) = getNumbers $ (filter(\x -> getNetworthByMovieExec(getMovieExecByName producer (a, b, c, d, e)) < getNetworthByMovieExec x )(movieExecsInList(a, b, c, d, e)))

getProducerIdByMovie :: Movie -> ProducerID
getProducerIdByMovie (Movie a b c d e) = e

getHigherProductions :: Name -> MovieDB -> [Name]
getHigherProductions producer (a, b, c, d, e) = getTitles $ (filter(\x ->elem (getProducerIdByMovie x) (getExecsBiggerNumber producer (a, b, c, d, e)) )(moviesInList(a, b, c, d, e)))

----------------------------------------------------------------------------------------------------------------------------------------------------

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)
                                                              
t1 :: BTree Char
t1 = Node 'a' (Node 'b' Nil (Node 'd' Nil Nil))(Node 'c' (Node 'f' (Node 'e' Nil Nil) Nil ) Nil)

t2 :: BTree Int
t2 =  Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))           
               
t1result :: (Num a)=>  BTree (Char,a)
t1result = Node ('a',2) (Node ('b',0) Nil (Node ('d',1) Nil Nil)) (Node ('c',5) (Node ('f',4) (Node ('e',3) Nil Nil) Nil) Nil)

t2result ::  (Num a)=> BTree (Int,a)
t2result = Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))           

size :: (Eq a) => BTree a -> Int
size (Nil) = 0
size (Node _ lt rt) = 1 + size(lt) + size(rt)

toBinaryIndexed :: (Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil
toBinaryIndexed (Node v lt rt) = helper (Node v lt rt) 0
  where
    helper Nil _ = Nil
    helper (Node v lt rt) curr = (Node (v , ((size lt) + curr)) (helper lt curr) (helper rt ((size lt) + curr + 1))) 
