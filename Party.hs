import Employee
import Data.Tree

main = print("hello")
glCons :: Employee -> GuestList -> GuestList
glCons dude@(Emp n fundiv) (GL olde fun) = GL (dude : olde) (fun+fundiv)

instance Monoid GuestList where
	mempty = GL [] 0
	mappend (GL emp1 fun1) (GL emp2 fun2) = (GL (emp1++emp2) (fun1+fun2))

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL ex x) (GL ey y) = if x >= y
							then (GL ex x)
							else (GL ey y)

treeFold :: (a->[b]->b) -> b -> Tree a -> b
treeFold f b (Node a c) = f (b ++ (a : treeFold c))

treeFold2 :: (Employee -> [GuestList] -> GuestList) -> GuestList -> Tree Employee -> GuestList
treeFold2 
--findBottom :: Tree GuestList -> [GuestList] -> [GuestList]
--findBottom (Node (a) []) b = a : b
--findBottom (Node (a) (c))) b = fmap findBottom ((c) (b))

findRootBoss :: Tree GuestList -> GuestList
findRootBoss (Node a c) = a

findEmpUnder :: Tree GuestList -> [GuestList]
findEmpUnder (Node a b) = fmap findRootBoss b

combineToUnder :: [GuestList] -> GuestList
combineToUnder a = foldr (mappend) mempty a

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob@(Emp b f) (x) = ((glCons bob (foldr mappend (GL [] 0) (map fst x))), (foldr mappend (GL [] 0) (map snd x)))

maxFun :: Tree Employee -> GuestList
maxFun 