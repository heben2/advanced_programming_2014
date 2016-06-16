module SalsaParser_test (test) 
where

import SalsaAst
import SalsaParser as P
import Test.QuickCheck

definitions :: [String]
definitions = ["viewdef", "rectangle", "circle", "group", "view"]

reserved :: [String]
reserved = definitions++colour

colour :: [String]
colour = ["blue", "plum", "red", "green", "orange"]

colorT :: String -> Colour
colorT c = case c of 
    "blue" -> Blue
    "plum" -> Plum
    "red" -> Red
    "green" -> Green
    "orange" -> Orange


vIdentFirst :: String
vIdentFirst = ['A'..'Z']

sIdentFirst :: String
sIdentFirst = ['a'..'z']

nameChars :: String
nameChars = '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

numberString :: String
numberString = ['0'..'9']


newtype TestVarCol = TestVarCol (String, Colour)

instance Arbitrary TestVarCol where
  arbitrary = do
        col <- elements colour
        return $ TestVarCol(col, colorT col)

newtype TestVarVIdent = TestVarVIdent (String, Ident)
  deriving (Ord, Eq, Show)

instance Arbitrary TestVarVIdent where
  arbitrary = do 
        c <- elements vIdentFirst
        cs <- listOf $ elements nameChars
        let vIdent = c:cs
        return $ TestVarVIdent(vIdent, vIdent)

newtype TestVarVIdentMany = TestVarVIdentMany (String, [Ident])
  deriving (Ord, Eq, Show)

instance Arbitrary TestVarVIdentMany where 
  arbitrary = do 
        c <- elements vIdentFirst
        cs <- listOf $ elements nameChars
        let vIdent = c:cs
        let group = "["++vIdent++"]" -- With only one element atm.
        return $ TestVarVIdentMany(group, [vIdent])

newtype TestVarSIdent = TestVarSIdent (String, Ident)
  deriving (Ord, Eq, Show)

instance Arbitrary TestVarSIdent where
  arbitrary = do 
        c <- elements sIdentFirst
        cs <- listOf $ elements nameChars
        let vIdent = c:cs
        if vIdent `elem` reserved then do 
                cc <- elements nameChars
                return $ TestVarSIdent(vIdent++[cc], vIdent++[cc])
        else return $ TestVarSIdent(vIdent, vIdent)

newtype TestPrim = TestPrim (String, Expr)

instance Arbitrary TestPrim where
  arbitrary = do 
        c <- elements numberString
        cs <- listOf $ elements numberString
        let prim = c:cs
        return $ TestPrim(prim, Const $ read prim)

newtype TestVarDef = TestVarDef (String, Definition)

instance Arbitrary TestVarDef where
  arbitrary = do
    def <- elements definitions
    case def of
        "viewdef" -> do
                (TestVarVIdent (sv,v)) <- arbitrary
                (TestPrim (se1,e1)) <- arbitrary
                (TestPrim (se2,e2)) <- arbitrary
                let definition = def++" "++sv++" "++se1++" "++se2
                return $ TestVarDef(definition, Viewdef v e1 e2)
        "rectangle" -> do
                (TestVarCol (sc, c)) <- arbitrary
                (TestVarSIdent (sv,v)) <- arbitrary
                (TestPrim (se1,e1)) <- arbitrary
                (TestPrim (se2,e2)) <- arbitrary
                (TestPrim (se3,e3)) <- arbitrary
                (TestPrim (se4,e4)) <- arbitrary
                let definition = def++" "++sv++" "++se1++" "++se2++" "++se3++" "++se4++" "++sc
                return $ TestVarDef(definition, Rectangle v e1 e2 e3 e4 c)
        "circle" -> do
                (TestVarCol (sc, c)) <- arbitrary
                (TestVarSIdent (sv,v)) <- arbitrary
                (TestPrim (se1,e1)) <- arbitrary
                (TestPrim (se2,e2)) <- arbitrary
                (TestPrim (se3,e3)) <- arbitrary
                let definition = def++" "++sv++" "++se1++" "++se2++" "++se3++" "++sc
                return $ TestVarDef(definition, Circle v e1 e2 e3 c)
        "view" -> do
                (TestVarVIdent (sv,v)) <- arbitrary
                let definition = def++" "++sv
                return $ TestVarDef(definition, View v)
        "group" -> do
                (TestVarVIdent (sv,v)) <- arbitrary
                (TestVarVIdentMany (svs,vs)) <- arbitrary
                let definition = def++" "++sv++" "++svs
                return $ TestVarDef(definition, Group v vs)


newtype TestDefCom = TestDefCom (String, DefCom)
    deriving (Eq, Show)

instance Arbitrary TestDefCom where
  arbitrary = do
        (TestVarDef (sv,v)) <- arbitrary
        return $ TestDefCom(sv++" ", Def v)


newtype TestProgram = TestProgram (String, Program)
    deriving (Eq, Show)

instance Arbitrary TestProgram where
  arbitrary = do
        (TestDefCom (sv,v)) <- arbitrary
        return $ TestProgram(sv, [v])

prop_pProg :: TestProgram -> Bool
prop_pProg (TestProgram (s,r)) =
    parseStringRight s == r

test :: IO ()
test = quickCheck prop_pProg

--To avoid parseError not being deriving Eq...
parseStringRight :: String -> Program
parseStringRight s = 
    case P.parseString s of
        Left _ -> []
        Right p -> p