{-# LANGUAGE OverloadedStrings #-}

import Language.Haskell.Exts
import System.Environment
import System.IO
import Text.PrettyPrint.ANSI.Leijen

main :: IO ()
main = do
    args <- getArgs
    expS <- case args of
        [] -> getContents
        ["-"] -> getContents
        xs -> return $ unwords xs
    case parseWithMode defaultParseMode
                { extensions = [ EnableExtension ImplicitParams
                               , EnableExtension KindSignatures
                               , EnableExtension MagicHash
                               , EnableExtension MultiParamTypeClasses
                               , EnableExtension NPlusKPatterns
                               , EnableExtension RankNTypes
                               , EnableExtension RecordPuns
                               , EnableExtension RecordWildCards
                               , EnableExtension ScopedTypeVariables
                               , EnableExtension TypeFamilies
                               , EnableExtension UnboxedTuples
                               ] }
            expS of
        ParseOk e -> do
            print e
            displayIO stdout . renderPretty 0.4 100 $ explain e
            putChar '\n'
        s@ParseFailed{} -> error $ show s

explain :: Exp -> Doc
-- variables
explain (Var q) = explainQname False q

-- implicit variables, ?a
explain (IPVar i) = explainIname i

-- constructors, Foo
explain (Con c) = explainQname False c

-- literals, 3, "foo", 'a', 1.0
explain (Lit l) = explainLit l

-- infix application
explain (InfixApp e1 q e2) = parens $
    explain e1 <+> explainQop q <+> explain e2

-- applications, a b
explain (App a b) = parens $ explain a <+> explain b

-- negation
explain (NegApp a) = parens $ "-" <> explain a

-- lambdas
explain (Lambda _ ps e) = parens $ ("\\" <> hsep (map explainP ps)) <+> "->" <+> explain e

explainP :: Pat -> Doc
explainP (PVar n)            = unName False n empty
explainP (PLit l)            = explainLit l
explainP (PNeg n)            = "-" <> explainP n
explainP (PNPlusK n k)       = unName False n empty <+> "+" <+> integer k
explainP (PInfixApp p1 n p2) = explainP p1 <+> explainQname True n <+> explainP p2
explainP (PApp qn ps)        = hsep $ explainQname False qn : map explainP ps
explainP (PTuple box ps)     = boxUp box . cat . punctuate comma $ map explainP ps
explainP (PList ps)          = brackets . cat . punctuate comma $ map explainP ps
explainP (PParen p)          = parens $ explainP p
explainP (PRec qn fs)        = explainQname False qn <+> braces (cat . punctuate comma $ map explainPF fs)
explainP (PAsPat n p)        = unName False n empty <> "@" <> explainP p
explainP PWildCard           = "_"
explainP (PIrrPat p)         = "~" <> explainP p
explainP (PatTypeSig _ p t)  = explainP p <+> "::" <+> explainT t

explainT :: Type -> Doc
explainT (TyForall (Just ns) _ t) = hsep ("forall" : map explainTVB ns) <+> "." <+> explainT t
explainT (TyForall Nothing ass t) = cat (punctuate comma $ map explainAsst ass) <+> "=>" <+> explainT t
explainT (TyFun t1 t2)            = explainT t1 <+> explainT t2
explainT (TyTuple box ts)         = boxUp box . cat . punctuate comma $ map explainT ts
explainT (TyList t)               = braces $ explainT t
explainT (TyApp t1 t2)            = parens $ explainT t1 <+> explainT t2
explainT (TyVar n)                = unName False n empty
explainT (TyCon qn)               = explainQname False qn

explainAsst :: Asst -> Doc
explainAsst (ClassA qn ts) = explainQname False qn <+> hsep (map explainT ts)
explainAsst (EqualP t1 t2) = explainT t1 <+> "~" <+> explainT t2

explainTVB :: TyVarBind -> Doc
explainTVB (KindedVar n k) = parens $ unName False n empty <+> "::" <+> explainK k
explainTVB (UnkindedVar n) = unName False n empty

explainK :: Kind -> Doc
explainK KindStar = "*"

explainPF :: PatField -> Doc
explainPF (PFieldPat qn p) = explainQname False qn <+> "=" <+> explainP p
explainPF (PFieldPun n) = unName False n empty
explainPF PFieldWildcard = ".."

boxUp :: Boxed -> Doc -> Doc
boxUp Boxed = parens
boxUp Unboxed = enclose "(# " " #)"

-- qualified or unqualified name -> string
explainQname :: Bool -> QName -> Doc
explainQname b (UnQual n) = unName b n empty
explainQname b (Qual (ModuleName m) n) = unName b n (string m <> ".")
explainQname _ (Special UnitCon) = "()"
explainQname _ (Special Cons) = ":"

-- implicit name -> string
explainIname :: IPName -> Doc
explainIname (IPDup d) = "?" <> string d
explainIname (IPLin l) = "%" <> string l

explainLit :: Literal -> Doc
explainLit (Char c)       = squotes . text . drop 1 . init $ show c
explainLit (String s)     = dquotes . text . drop 1 . init $ show s
explainLit (Int i)        = integer i
explainLit (Frac r)       = parens $ rational r
explainLit (PrimInt i)    = integer i <> "#"
explainLit (PrimWord w)   = integer w <> "##"
explainLit (PrimFloat r)  = explainLit (Frac r) <> "#"
explainLit (PrimDouble d) = explainLit (Frac d) <> "##"
explainLit (PrimChar c)   = explainLit (Char c) <> "#"
explainLit (PrimString s) = explainLit (String s) <> "#"

explainQop :: QOp -> Doc
explainQop (QVarOp n) = explainQname True n
explainQop (QConOp n) = explainQname True n

-- name to string
unName :: Bool -> Name -> Doc -> Doc
unName b (Ident s) = \d -> if b then enclose "`" "`" $ d <> string s else d <> string s
unName b (Symbol s) = \d -> if b then d <> string s else parens $ d <> string s
