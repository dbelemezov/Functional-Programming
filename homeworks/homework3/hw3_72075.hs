main :: IO()
main = do
 print $ getMoviesLongerThan "Star Wars" db == ["Star Trek", "Terms of Endearment", "Gone With the Wind", "The Fellowship of the Ring"]
 print $ getMoviesLongerThan "The Fellowship of the Ring" db == ["Gone With the Wind"]
 print $ getMaleActorsIn "Terms of Endearment" db == ["Jack Nicholson"]
 print $ getMaleActorsIn "Star Wars" db == ["Alec Baldwin", "Harrison Ford"]
 print $ getFemaleActorsFrom 1983 db == ["Jane Fonda"]
 print $ getFemaleActorsFrom 2001 db == ["Scarlett Johansson","Cate Blanchett", "Liv Tyler"]

 print $ degr t1 5 == 1
 print $ degr t1 6 == 4
 print $ degr t1 7 == 3
 print $ degr t1 18 == 1
 print $ degr t2 's' == 1
 print $ degr t2 'k' == 4
 print $ degr t2 '1' == 3

type Name = String
type Title = String
type Year = Int
type Gender = Char
type Length = Int

data Movie = Movie Title Year Length 
 deriving (Show, Eq)
data MovieStar = MovieStar Name Gender
 deriving (Show, Eq)
data StarsIn = StarsIn Name Title 
 deriving (Show, Eq)

type MovieDB = ([Movie], [MovieStar], [StarsIn])

movies :: [Movie]
movies = [Movie "The Man Who Wasn't There" 2001 116,
 Movie "Logan's run" 1976 120,
 Movie "Star Wars" 1977 124,
 Movie "Empire Strikes Back" 1980 111,
 Movie "Star Trek" 1979 132,
 Movie "Star Trek: Nemesis" 2002 116,
 Movie "Terms of Endearment" 1983 132,
 Movie "The Usual Suspects" 1995 106,
 Movie "Gone With the Wind" 1938 238,
 Movie "The Fellowship of the Ring" 2001 178]

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
db = (movies, stars, starsIn)

-------------------------------------------------------------------

getTitle ::  Movie -> Title
getTitle (Movie a b c) = a

getLength ::  Movie -> Length
getLength (Movie a b c) = c

getLengthByTitle :: Title -> MovieDB -> Length
getLengthByTitle title (a, b, c) = getLength(  head (filter(\x -> (getTitle x) == title )(moviesInList (a,b,c))))

moviesInList :: MovieDB -> [Movie]
moviesInList (a, b, c) = a

getTitles :: [Movie] -> [Title]
getTitles [] = []
getTitles xs = getTitle(head xs) : getTitles (tail xs)

getMoviesLongerThan :: Title -> MovieDB -> [Title]
getMoviesLongerThan title (a,b,c) = getTitles $ filter(\x ->(getLength x) > (getLengthByTitle title (a, b, c))) (moviesInList (a,b,c))

----------------------------------------------------------------------

getMovieByTitle :: Title -> MovieDB -> Movie
getMovieByTitle title (a, b, c) = head (filter(\x -> (getTitle x) == title )(moviesInList (a,b,c)))

getStars :: MovieDB -> [StarsIn]
getStars (a, b, c) = c

getTitleByStar :: StarsIn -> Title
getTitleByStar (StarsIn a b) = b

getMovieActors :: Title -> MovieDB -> [StarsIn]
getMovieActors title (a, b, c) = filter(\x -> title == (getTitleByStar x)) (getStars (a, b, c))

getName :: StarsIn -> Name
getName (StarsIn a b) = a

getName2 :: MovieStar -> Name
getName2 (MovieStar a b) = a

getStarsNames :: [StarsIn] -> [Name] 
getStarsNames [] = []
getStarsNames xs = getName(head xs) : getStarsNames (tail xs)

getStars2 :: MovieDB -> [MovieStar]
getStars2 (a, b, c) = b

getGender :: MovieStar -> Gender
getGender (MovieStar a b) = b

getGenderByName :: Name ->  MovieDB -> Gender
getGenderByName name (a, b, c) = getGender $ head (filter(\x -> (getName2 x) == name )(getStars2 (a,b,c)))

getMaleActorsIn :: Title -> MovieDB -> [Name]
getMaleActorsIn title (a, b, c) = filter(\x -> (getGenderByName x (a, b, c)) == 'M' ) (getStarsNames(getMovieActors title (a, b, c)))

---------------------------------------------------------------------

getYearFromMovie :: Movie -> Year
getYearFromMovie (Movie a b c) = b

getMovieByYear :: Year -> MovieDB -> [Movie]
getMovieByYear year (a, b, c) = filter(\x -> year == (getYearFromMovie x)) (moviesInList (a, b, c))

getStarsByMovie :: Movie -> MovieDB -> [StarsIn]
getStarsByMovie movie (a, b, c) = filter(\x -> getTitle movie == (getTitleByStar x)) (getStars (a, b, c))

getStarsByMovies :: [Movie] -> MovieDB -> [StarsIn]
getStarsByMovies [] (a, b, c) = []
getStarsByMovies xs (a, b, c) = (getStarsByMovie(head xs) (a, b, c)) ++ getStarsByMovies (tail xs)(a, b, c)

getFemaleActorsFrom :: Year -> MovieDB -> [Name]
getFemaleActorsFrom year (a, b, c) = filter(\x -> (getGenderByName x (a, b, c)) == 'F' ) (getStarsNames(getStarsByMovies (getMovieByYear year (a,b,c)) (a, b, c)))

-----------------------------------------------------

data NTree a = Nil | Node a [NTree a]
 deriving (Show, Eq)

t1 :: (Num a) => NTree a
t1 = Node 8 [Node 7 [Node 4 [Nil], Node 5 [Nil]], Node 6 [Node 10 [Nil], Node 15 [Nil] , Node 13 [Nil]], Node 18 [Nil]]

t2 ::  NTree Char
t2 = Node '1'[Node 'f' [Node 'H' [Nil], Node 'a'[Nil]], Node 'm'[Node 's' [Nil]], Node 'i'[Node 'k'[Node 'e' [Nil], Node 'l'[Nil],Node 'L'[Nil]]]]

degr :: (Eq a) => NTree a -> a -> Int
degr Nil _ = 0
degr (Node node xs) search = sum ((map count xs) ++ map (\x->degr x search) xs )
  where 
    count Nil = 0
    count (Node v xs) = if (node == search) || ( v == search) then 1 else 0