module Refactor
    ( compilationUnit
    , Refactor(..)
    ) where

import           Control.Monad.State
import qualified Data.Char             as Char
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Language.Java.Syntax
import qualified System.Random         as Random
import           System.Random.Shuffle (shuffle')

-- Refactorings:
-- * Selection of imports / reordering imports
-- * Shuffle type declarations (class / interface)
-- * Identifier renaming
-- * Shuffling of class members


-- Refactoring state data
data Refactor =
    Refactor
        { randDoubles :: [[Double]]
        , randStrings :: [String]
        , randInts    :: [[Int]]
        , renaming    :: Map.Map Ident Ident
        , selectProb  :: Double
        }

-- Keep maybe or return Nothing
probKeep :: Maybe a -> Double -> Double-> Maybe a
probKeep m c r =
    if r > c then m else Nothing

-- | Keep or reject list of items
probFilter :: Double -> [a] -> State Refactor [a]
probFilter c xs = do
    rs <- getRandomDoubles
    return $ catMaybes $ zipWith (\r x -> probKeep (Just x) c r) rs xs

-- Get infinite list of random Doubles
getRandomDoubles :: State Refactor [Double]
getRandomDoubles = do
    ref@Refactor{ randDoubles = (x: xs) } <- get
    put (ref { randDoubles = xs } )
    return x

getRandomInts :: State Refactor [Int]
getRandomInts = do
    ref@Refactor{ randInts = (x: xs) } <- get
    put (ref { randInts = xs } )
    return x

-- | Get random String
getRandomString :: State Refactor String
getRandomString = do
    ref@Refactor { randStrings = (x: xs) } <- get
    put (ref { randStrings = xs } )
    return x

-- | Generate new ident
newIdent :: Ident -> (String -> String) -> State Refactor Ident
newIdent old f = do
    i <- f `fmap` getRandomString
    ref@Refactor { renaming = r } <- get
    put (ref { renaming = Map.insert old (Ident i) r} )
    return (Ident i)

-- Get renamed ident if present, otherwise use original
getIdent :: Ident -> State Refactor Ident
getIdent old = do
    Refactor { renaming = r } <- get
    return (Map.findWithDefault old old r)

-- | Refactor compilation unit with some randomness
compilationUnit :: CompilationUnit -> State Refactor CompilationUnit
compilationUnit (CompilationUnit packageDecl importDecls typeDecls) =
  do
    nImportDecls <- probFilter 0.5 importDecls
    shuffledImports <- shuffle nImportDecls
    mapM_ collectTypeDeclName typeDecls
    ntypeDecls <- mapM typeDeclRename typeDecls >>= shuffle

    return $
        CompilationUnit
            packageDecl
            shuffledImports
            ntypeDecls

shuffle :: [a] -> State Refactor [a]
shuffle [] = return []
shuffle xs = do
    x:_ <- getRandomInts
    let gen  = Random.mkStdGen x
    return (shuffle' xs (length xs) gen)

classBody :: ClassBody -> State Refactor ClassBody
classBody (ClassBody decls) = do
    mapM_ collectDeclNames decls
    nDecls <- mapM decl decls
    shuffledDecls <- shuffle nDecls
    return $ ClassBody shuffledDecls

varDeclId :: VarDeclId -> State Refactor VarDeclId
varDeclId (VarId i) = do
    i' <- newIdent i id
    return (VarId i)
varDeclId (VarDeclArray a) = do
    d <- varDeclId a
    return (VarDeclArray a)

formalParam :: FormalParam -> State Refactor FormalParam
formalParam (FormalParam m t b d) = do
    nD <- varDeclId d
    nT <- typ t
    return (FormalParam m nT b nD)

collectDeclNames :: Decl -> State Refactor ()
collectDeclNames (MemberDecl (FieldDecl _ _ varDecls)) = do
    mapM_ collectVarDecl varDecls
    return ()
collectDeclNames (MemberDecl (MethodDecl _ _ _ old _ _ _ _)) = do
    newIdent old id
    return ()
collectDeclNames (MemberDecl (MemberClassDecl c)) = do
    collectClassDeclName c
    return ()
collectDeclNames _ =
    return ()

decl :: Decl -> State Refactor Decl
decl (MemberDecl (FieldDecl m t varDecls)) = do
    mapM_ collectVarDecl varDecls
    nVarDecls <- mapM varDecl varDecls
    nT <- typ t
    return $ MemberDecl (FieldDecl m nT nVarDecls)
decl (MemberDecl (MethodDecl m t ty old p e mexp b)) = do
    s <- getIdent old
    nP <- mapM formalParam p
    nB <- methodBody b
    return $ MemberDecl (MethodDecl m t ty s nP e mexp nB)
decl (MemberDecl (MemberClassDecl c)) = do
    collectClassDeclName c
    nD <- classDecl c
    return (MemberDecl (MemberClassDecl nD))
decl (MemberDecl (ConstructorDecl m tps old p e (ConstructorBody i body))) = do
    nI <- getIdent old
    nP <- mapM formalParam p
    nBody <- mapM blockStmt body
    return (MemberDecl (ConstructorDecl m tps nI nP e (ConstructorBody i nBody)))
decl x =
    return x

lhs :: Lhs -> State Refactor Lhs
lhs (NameLhs (Name ns)) = do
    nNames <- mapM getIdent ns
    return (NameLhs (Name nNames))
lhs (ArrayLhs (ArrayIndex a exps)) = do
    nA <- expr a
    nExps <- mapM expr exps
    return (ArrayLhs (ArrayIndex nA nExps))
lhs (FieldLhs f) = do
    f' <- fieldAccess f
    return (FieldLhs f')

-- | Refactor expression
expr :: Exp -> State Refactor Exp
expr (ExpName (Name ns)) = do
    nNames <- mapM getIdent ns
    return (ExpName (Name nNames))
expr (Assign l o e) = do
    nLhs <- lhs l
    nExpr <- expr e
    return (Assign nLhs o nExpr)
expr (BinOp e1 o e2) = do
    el <- expr e1
    er <- expr e2
    return (BinOp el o er)
expr (PostIncrement e) = do
    nE <- expr e
    return (PostIncrement nE)
expr (PostDecrement e) = do
    nE <- expr e
    return (PostDecrement nE)
expr (PreIncrement e) = do
    nE <- expr e
    return (PreIncrement nE)
expr (PreDecrement e) = do
    nE <- expr e
    return (PreDecrement nE)
expr (PrePlus e) = do
    nE <- expr e
    return (PrePlus nE)
expr (PreMinus e) = do
    nE <- expr e
    return (PreMinus nE)
expr (PreBitCompl e) = do
    nE <- expr e
    return (PreBitCompl nE)
expr (PreNot e) = do
    nE <- expr e
    return (PreNot nE)
expr (MethodInv inv) =
    MethodInv `fmap` methodInvocation inv
expr (ArrayAccess (ArrayIndex a exps)) = do
    nA <- expr a
    nExps <- mapM expr exps
    return (ArrayAccess (ArrayIndex nA nExps))
expr (FieldAccess f) = do
    f' <- fieldAccess f
    return (FieldAccess f')
expr x = return x

methodInvocation :: MethodInvocation -> State Refactor MethodInvocation
methodInvocation (MethodCall (Name names) args) = do
    nNames <- mapM getIdent names
    nArgs <- mapM expr args
    return (MethodCall (Name nNames) nArgs)
methodInvocation x = return x

fieldAccess :: FieldAccess -> State Refactor FieldAccess
fieldAccess (PrimaryFieldAccess e i) = do
    nE <- expr e
    x <- getIdent i
    return (PrimaryFieldAccess nE x)
fieldAccess x = return x

forInit :: ForInit -> State Refactor ForInit
forInit (ForLocalVars m t decls) = do
    mapM_ collectVarDecl decls
    nDecls <- mapM varDecl decls
    return (ForLocalVars m t nDecls)
forInit (ForInitExps exps) =
    ForInitExps `fmap` mapM expr exps

stmt :: Stmt -> State Refactor Stmt
stmt (StmtBlock b) = do
    nB <- block b
    return (StmtBlock nB)
stmt (ExpStmt e) = do
    nE <- expr e
    return (ExpStmt nE)
stmt (IfThen e s) = do
    nE <- expr e
    nS <- stmt s
    return (IfThen nE nS)
stmt (IfThenElse e s1 s2) = do
    nE <- expr e
    nS1 <- stmt s1
    nS2<- stmt s2
    return (IfThenElse nE nS1 nS2)
stmt (While e s) = do
    nE <- expr e
    nS <- stmt s
    return (While nE nS)
stmt (Return e) =
    case e of
        Just x -> do
            y <- expr x
            return (Return (Just y))
        Nothing ->
            return (Return e)
stmt (BasicFor i e exps s) = do
    nInit <- case i of
        Just x -> do
            y <- forInit x
            return (Just y)
        Nothing ->
            return Nothing
    nE <- case e of
        Just x -> do
            xE <- expr x
            return (Just xE)
        Nothing -> return Nothing
    nEs <- case exps of
        Just xs -> do
            xEs <- mapM expr xs
            return (Just xEs)
        Nothing -> return Nothing

    nStmt <- stmt s
    return (BasicFor nInit nE nEs nStmt)
stmt (Assert e e2) = do
    x <- expr e
    x2 <-
        case e2 of
            Nothing ->
                return Nothing
            Just a -> do
                nA <- expr a
                return (Just nA)
    return (Assert x x2)

stmt x = return x


typ :: Type -> State Refactor Type
typ (RefType (ClassRefType (ClassType cr))) = do
    Refactor {renaming = r} <- get

    let nCr = map (\(i, t) -> (Map.findWithDefault i i r, t)) cr

    return (RefType (ClassRefType (ClassType nCr)))
typ (RefType (ArrayType at)) = do
    nAt <- typ at
    return (RefType (ArrayType nAt))
typ x = return x

blockStmt :: BlockStmt -> State Refactor BlockStmt
blockStmt (BlockStmt s) = do
    nS <- stmt s
    return (BlockStmt nS)
blockStmt (LocalVars m t varDecls) = do
    mapM_ collectVarDecl varDecls
    nDecls <- mapM varDecl varDecls
    nT <- typ t
    return (LocalVars m nT nDecls)
blockStmt (LocalClass d) = do
    nDecl <- classDecl d
    return (LocalClass nDecl)

block :: Block -> State Refactor Block
block (Block b) = do
    nB <- mapM blockStmt b
    return (Block nB)

methodBody :: MethodBody -> State Refactor MethodBody
methodBody (MethodBody (Just b)) = do
    nBody <- block b
    return (MethodBody (Just nBody))
methodBody x = return x

collectVarDecl :: VarDecl -> State Refactor ()
collectVarDecl (VarDecl (VarId i) _) = do
    _ <- newIdent i id
    return ()
collectVarDecl _ = return ()


varInit :: VarInit -> State Refactor VarInit
varInit (InitExp e) = do
    nE <- expr e
    return (InitExp nE)
varInit (InitArray (ArrayInit is)) = do
    nIs <- mapM varInit is
    return (InitArray (ArrayInit nIs))

-- | Var decls
varDecl :: VarDecl -> State Refactor VarDecl
varDecl (VarDecl (VarId old) i) = do
    s <- getIdent old
    ini <-
        case i of
            Just a -> do
                 nA <- varInit a
                 return (Just nA)
            Nothing ->
                return Nothing

    return $ VarDecl (VarId s) ini
varDecl x = return x

classDecl :: ClassDecl -> State Refactor ClassDecl
classDecl (ClassDecl m old ps r t b) = do
    ident <- getIdent old
    nBody <- classBody b
    return (ClassDecl m ident ps r t nBody)
classDecl (EnumDecl m old t b) = do
    ident <- getIdent old
    return $ EnumDecl m ident t b

collectClassDeclName :: ClassDecl -> State Refactor ()
collectClassDeclName (ClassDecl _ old _ _ _ _) = do
    _ <- newIdent old capitalize
    return ()
collectClassDeclName (EnumDecl _ old _ _) = do
    _ <- newIdent old capitalize
    return ()

collectTypeDeclName :: TypeDecl -> State Refactor ()
collectTypeDeclName decl =
    case decl of
        ClassTypeDecl d ->
            collectClassDeclName d
        InterfaceTypeDecl (InterfaceDecl _ _ old _ _ _) -> do
            _ <- newIdent old capitalize
            return ()

-- | Rename class name
typeDeclRename :: TypeDecl -> State Refactor TypeDecl
typeDeclRename decl =
    case decl of
        ClassTypeDecl d -> do
            nDecl <- classDecl d
            return (ClassTypeDecl nDecl)
        InterfaceTypeDecl (InterfaceDecl k m old t r b) -> do
            ident <- getIdent old
            return (InterfaceTypeDecl (InterfaceDecl k m ident t r b))

-- | Capitalize string
capitalize :: String -> String
capitalize [] = []
capitalize (x: xs) = Char.toUpper x: xs
