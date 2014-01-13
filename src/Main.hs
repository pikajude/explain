{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mconcat)
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
                { extensions = [ EnableExtension Arrows
                               , EnableExtension BangPatterns
                               , EnableExtension ImplicitParams
                               , EnableExtension KindSignatures
                               , EnableExtension MagicHash
                               , EnableExtension MultiParamTypeClasses
                               , EnableExtension NPlusKPatterns
                               , EnableExtension QuasiQuotes
                               , EnableExtension RankNTypes
                               , EnableExtension RecordPuns
                               , EnableExtension RecordWildCards
                               , EnableExtension ScopedTypeVariables
                               , EnableExtension TemplateHaskell
                               , EnableExtension TransformListComp
                               , EnableExtension TupleSections
                               , EnableExtension TypeFamilies
                               , EnableExtension UnboxedTuples
                               ] }
            expS of
        ParseOk e -> do
            print e
            displayIO stdout . renderPretty 0.4 10000 . hang 4 $ explain e
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
explain (Lambda _ ps e) = parens $ ("\\" <> fillSep (map explainP ps)) <+> "->" <+> explain e

-- do
explain (Do sms) = fillSep $ "do" : lbrace : punctuate semi (map explainStmt sms) ++ [rbrace]

-- let
explain (Let bs e) = fillSep ["let", bds, "in", explain e] where
    bds = cat $ case explainB bs of
              [] -> []
              qs -> lbrace : " " : punctuate (semi <> " ") qs ++ [" ", rbrace]

explain (If a b c) = fillSep ["if", explain a, "then", explain b, "else", explain c]

explain (Case e as) = fillSep $ "case" : explain e : "of" : lbrace : punctuate semi (map explainAlt as) ++ [rbrace]

-- mdo
explain (MDo sms) = fillSep $ "mdo" : lbrace : punctuate semi (map explainStmt sms) ++ [rbrace]

explain (Tuple b es) = boxUp b . cat . punctuate comma $ map explain es

explain (TupleSection b es) = boxUp b . cat . punctuate comma $ map (maybe empty explain) es

explain (List es) = brackets . cat . punctuate comma $ map explain es

explain (Paren e) = parens $ explain e

explain (LeftSection e q) = parens . fillSep $ [explain e, explainQop q]

explain (RightSection q e) = parens . fillSep $ [explainQop q, explain e]

explain (RecConstr qn fus) = fillSep $ explainQname False qn : lbrace : punctuate comma (map explainFU fus) ++ [rbrace]

explain (RecUpdate r fus) = fillSep $ explain r : lbrace : punctuate comma (map explainFU fus) ++ [rbrace]

explain (EnumFrom e) = brackets $ fillSep [explain e, ".."]

explain (EnumFromThen e t) = brackets . fillSep $ [explain e <> ",", explain t, ".."]

explain (EnumFromTo e n) = brackets $ fillSep [explain e, "..", explain n]

explain (EnumFromThenTo e t n) = brackets $ fillSep [explain e <> ",", explain t, "..", explain n]

explain (ListComp e qs) = brackets . fillSep $ explain e : "|" : punctuate comma (map explainQS qs)

explain (ParComp e qss) = brackets . fillSep $ explain e : "|" : punctuate " |" (map (fillSep . punctuate comma . map explainQS) qss)

explain (ExpTypeSig _ e t) = parens . fillSep $ [explain e, "::", explainT t]

explain (VarQuote qn) = "'" <> explainQname False qn

explain (TypQuote qn) = "''" <> explainQname False qn

explain (BracketExp br) = explainBracket br

explain (SpliceExp sp) = explainSplice sp

explain (QuasiQuote s st) = fillSep ["[" <> string s <> "|", string st, "|]"]

explain XTag{}       = error "HSP not yet supported."
explain XETag{}      = error "HSP not yet supported."
explain XPcdata{}    = error "HSP not yet supported."
explain XExpTag{}    = error "HSP not yet supported."
explain XChildTag{}  = error "HSP not yet supported."
explain CorePragma{} = error "CORE pragmas not yet supported."
explain SCCPragma{}  = error "SCC pragmas not yet supported."
explain GenPragma{}  = error "GENERATED pragmas not yet supported."

explain (Proc _ p e) = fillSep ["proc", explainP p, "->", explain e]

explain (LeftArrApp e1 e2) = parens $ fillSep [explain e1, "-<", explain e2]

explain (RightArrApp e1 e2) = parens $ fillSep [explain e1, ">-", explain e2]

explain (LeftArrHighApp e1 e2) = parens $ fillSep [explain e1, "-<<", explain e2]

explain (RightArrHighApp e1 e2) = parens $ fillSep [explain e1, ">>-", explain e2]

explainSplice :: Splice -> Doc
explainSplice (IdSplice s) = "$" <> string s
explainSplice (ParenSplice e) = "$" <> parens (explain e)

explainQS :: QualStmt -> Doc
explainQS (QualStmt st) = explainStmt st
explainQS (ThenTrans e) = fillSep ["then", explain e]
explainQS (ThenBy e r) = fillSep ["then", explain e, "by", explain r]
explainQS (GroupBy e) = fillSep ["then group by", explain e]
explainQS (GroupUsing e) = fillSep ["then group using", explain e]
explainQS (GroupByUsing e1 e) = fillSep ["then group by", explain e1, "using", explain e]

explainFU :: FieldUpdate -> Doc
explainFU (FieldUpdate q e) = fillSep [explainQname False q, "=", explain e]
explainFU (FieldPun n) = unName False n empty
explainFU FieldWildcard = ".."

explainAlt :: Alt -> Doc
explainAlt (Alt _ p gs bs) = fillSep $ explainP p : explainGAs gs ++ explainB bs

explainGAs :: GuardedAlts -> [Doc]
explainGAs (UnGuardedAlt e) = ["->", explain e]
explainGAs (GuardedAlts gs) = map
    (\(GuardedAlt _ ss e) -> fillSep $ "|" : punctuate comma (map explainStmt ss) ++ ["->", explain e]) gs

explainStmt :: Stmt -> Doc
explainStmt (Generator _ p e) = fillSep [explainP p, "<-", explain e]
explainStmt (Qualifier e) = explain e
explainStmt (LetStmt bs) = fillSep $ "let" : lbrace : punctuate semi (explainB bs) ++ [rbrace]
explainStmt (RecStmt _) = error "Recursive statements not yet supported."

explainB :: Binds -> [Doc]
explainB (BDecls ds) = map explainDec ds
explainB (IPBinds is) = map (\(IPBind _ n e) -> fillSep [explainIname n, "<-", explain e]) is

explainDec :: Decl -> Doc
explainDec (TypeSig _ ns t) =
    fillSep [cat $ punctuate comma (map (\n -> unName False n empty) ns), "::", explainT t]
explainDec (PatBind _ p t r bds) = fillSep $ pat ++ explainRhs r ++ explainWhere bds
    where pat = case t of
                    Nothing -> [explainP p]
                    Just ty -> [explainP p, "::", explainT ty]
explainDec TypeDecl{}         = error "Type declarations not supported in let bindings."
explainDec TypeFamDecl{}      = error "Type declarations not supported in let bindings."
explainDec DataDecl{}         = error "Data declarations not supported in let bindings."
explainDec GDataDecl{}        = error "Data declarations not supported in let bindings."
explainDec DataFamDecl{}      = error "Data declarations not supported in let bindings."
explainDec TypeInsDecl{}      = error "Type instance declarations not supported in let bindings."
explainDec DataInsDecl{}      = error "Data instance declarations not supported in let bindings."
explainDec GDataInsDecl{}     = error "Data instance declarations not supported in let bindings."
explainDec ClassDecl{}        = error "Class declarations not supported in let bindings."
explainDec InstDecl{}         = error "Instance declarations not supported in let bindings."
explainDec DerivDecl{}        = error "`Deriving' clauses not supported in let bindings."
explainDec InfixDecl{}        = error "Fixity declarations not supported in let bindings."
explainDec DefaultDecl{}      = error "Default type declarations not supported in let bindings."
explainDec SpliceDecl{}       = error "Splice declarations not supported in let bindings."
explainDec (FunBind ms)       = cat . punctuate (semi <> " ") $ map explainM ms
explainDec ForImp{}           = error "Foreign imports not supported in let bindings."
explainDec ForExp{}           = error "Foreign exports not supported in let bindings."
explainDec RulePragmaDecl{}   = empty
explainDec DeprPragmaDecl{}   = empty
explainDec WarnPragmaDecl{}   = empty
explainDec InlineSig{}        = empty
explainDec InlineConlikeSig{} = empty
explainDec SpecSig{}          = empty
explainDec SpecInlineSig{}    = empty
explainDec InstSig{}          = empty
explainDec AnnPragma{}        = empty

explainM :: Match -> Doc
explainM (Match _ n ps Nothing rh bs) = fillSep $
    unName False n empty : map explainP ps ++ explainRhs rh ++ explainWhere bs
explainM (Match _ _ _ t _ _) = error $ show t

explainWhere :: Binds -> [Doc]
explainWhere bs = case explainB bs of
    [] -> []
    bq -> "where" : lbrace : bq ++ [rbrace]

explainRhs :: Rhs -> [Doc]
explainRhs (UnGuardedRhs e) = ["=", explain e]
explainRhs (GuardedRhss gs) = "|" : map explainGR gs

explainGR :: GuardedRhs -> Doc
explainGR (GuardedRhs _ stmts e) = fillSep $
    punctuate comma (map explainStmt stmts) ++ ["=", explain e]

explainP :: Pat -> Doc
explainP (PVar n)            = unName False n empty
explainP (PLit l)            = explainLit l
explainP (PNeg n)            = "-" <> explainP n
explainP (PNPlusK n k)       = unName False n empty <+> "+" <+> integer k
explainP (PInfixApp p1 n p2) = explainP p1 <+> explainQname True n <+> explainP p2
explainP (PApp qn ps)        = fillSep $ explainQname False qn : map explainP ps
explainP (PTuple box ps)     = boxUp box . cat . punctuate comma $ map explainP ps
explainP (PList ps)          = brackets . cat . punctuate comma $ map explainP ps
explainP (PParen p)          = parens $ explainP p
explainP (PRec qn fs)        = explainQname False qn <+> braces (cat . punctuate comma $ map explainPF fs)
explainP (PAsPat n p)        = unName False n empty <> "@" <> explainP p
explainP PWildCard           = "_"
explainP (PIrrPat p)         = "~" <> explainP p
explainP (PatTypeSig _ p t)  = fillSep [explainP p, "::", explainT t]
explainP (PViewPat e p)      = fillSep [explain e, "->", explainP p]
explainP PRPat{}             = error "Regular patterns not yet supported."
explainP PXTag{}             = error "HSP not yet supported."
explainP PXETag{}            = error "HSP not yet supported."
explainP PXPcdata{}          = error "HSP not yet supported."
explainP PXPatTag{}          = error "HSP not yet supported."
explainP PXRPats{}           = error "HSP not yet supported."
explainP PExplTypeArg{}      = error "Explicit type arguments not yet supported."
explainP (PQuasiQuote s1 s2) = mconcat ["[", string s1, "| ", string s2, " ]"]
explainP (PBangPat p)        = "!" <> explainP p

explainT :: Type -> Doc
explainT (TyForall (Just ns) _ t) = hsep ("forall" : map explainTVB ns) <+> "." <+> explainT t
explainT (TyForall Nothing ass t) = fillSep [cat (punctuate comma $ map explainAsst ass), "=>", explainT t]
explainT (TyFun t1 t2)            = fillSep [explainT t1, "->", explainT t2]
explainT (TyTuple box ts)         = boxUp box . cat . punctuate comma $ map explainT ts
explainT (TyList t)               = braces $ explainT t
explainT (TyApp t1 t2)            = parens $ explainT t1 <+> explainT t2
explainT (TyVar n)                = unName False n empty
explainT (TyCon qn)               = explainQname False qn
explainT (TyParen t)              = parens $ explainT t
explainT (TyInfix t1 qn t2)       = fillSep [explainT t1, explainQname True qn, explainT t2]
explainT (TyKind t k)             = fillSep [explainT t, "::", explainK k]

explainAsst :: Asst -> Doc
explainAsst (ClassA qn ts) = explainQname False qn <+> fillSep (map explainT ts)
explainAsst (EqualP t1 t2) = fillSep [explainT t1, "~", explainT t2]
explainAsst (InfixA t1 qn t2) = fillSep [explainT t1, explainQname True qn, explainT t2]
explainAsst (IParam i t) = fillSep [explainIname i, "::", explainT t]

explainTVB :: TyVarBind -> Doc
explainTVB (KindedVar n k) = parens $ unName False n empty <+> "::" <+> explainK k
explainTVB (UnkindedVar n) = unName False n empty

explainK :: Kind -> Doc
explainK KindStar = "*"
explainK KindBang = "!"
explainK (KindFn k1 k2) = fillSep [explainK k1, "->", explainK k2]
explainK (KindParen k) = parens $ explainK k
explainK (KindVar n) = unName False n empty

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
explainQname _ (Special ListCon) = "[]"
explainQname _ (Special FunCon) = "->"
explainQname _ (Special (TupleCon b i)) = boxUp b . mconcat $ replicate (i - 1) ","
explainQname _ (Special UnboxedSingleCon) = "(# #)"

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

explainBracket :: Bracket -> Doc
explainBracket (ExpBracket e) = fillSep ["[|", explain e, "|]"]
explainBracket (PatBracket p) = fillSep ["[p|", explainP p, "|]"]
explainBracket (TypeBracket t) = fillSep ["[t|", explainT t, "|]"]
explainBracket (DeclBracket ds) = fillSep $ "[d|" : punctuate semi (map explainDec ds) ++ ["|]"]

-- name to string
unName :: Bool -> Name -> Doc -> Doc
unName b (Ident s) = \d -> if b then enclose "`" "`" $ d <> string s else d <> string s
unName b (Symbol s) = \d -> if b then d <> string s else parens $ d <> string s
