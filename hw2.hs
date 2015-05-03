
import Prelude

data Friend = Kris
             | Blanche
             | Blake
             | Steve
             | Diego
             | Luis
             | Ales
             | Haleigh
             | Ryan
             | Zach
             deriving Show

kris :: Friend
kris = Kris

listOFriends :: [Friend]
listOFriends = [Kris, Blanche, Blake, Steve, Diego, Luis, Ales, Haleigh, Ryan, Zach]

isBoy :: Friend -> Bool
isBoy Kris = True
isBoy Blanche = False
isBoy Blake = True
isBoy Steve = True
isBoy Diego = True
isBoy Luis = True
isBoy Ales = True
isBoy Haleigh = False
isBoy Ryan = True
isBoy Zach = True

-- Could also make it shorter like this

isBoy2 :: Friend -> Bool
isBoy2 Blanche = False
isBoy2 Haleigh = False
isBoy2 _ = True

-- Example of a Data type that's not just an enumeration

data FailableDouble = Failure
                    | OK Double
                    deriving Show

ex01 = Failure
ex02 = OK 3.4

-- What is the type of OK? It takes as inputs Double, and outputs types of FailableDouble

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
saveDiv x y = OK (x / y)

safeDiv' :: Double -> Double -> Maybe Double
safeDiv' _ 0 = Nothing
safeDiv' x y = Just (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- Data Constructors can have more than one argument

data Person = Person String Int Friend --store a persons name, age, and favorite friend
              deriving Show


blake :: Person
blake = Person "Brent" 31 Kris

stan :: Person
stan = Person "Stan" 94 Blanche

-- Question: How to make people instances of different types...have Friend be type alias of Person

getAge :: Person -> Int
getAge (Person _ a _) = a

{-
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
-}
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

ex05 = case "Hello" of
        []      -> 3
        ('H':s) -> length s
        _       -> 7

-- Most of what we've done is just sugar for case expression

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                    Failure -> 0
                    OK d -> d

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l


data Tree = Leaf Char
          | Node Tree Int Tree
          deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

data MessageType = Info | Warning | Error Int deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String | Unknown String deriving (Show, Eq)


