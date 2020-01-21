 module Hw1 where

type Mapping = [(String, String, String)]
data AST = EmptyAST | ASTNode String AST AST deriving (Show, Read)

writeExpression :: (AST, Mapping) -> String
evaluateAST :: (AST, Mapping) -> (AST, String)
-- DO NOT MODIFY OR DELETE THE LINES ABOVE -- 
-- IMPLEMENT writeExpression and evaluateAST FUNCTION ACCORDING TO GIVEN SIGNATURES -- 

getValue (ASTNode v _ _) = v
    
tupleToString (var, "num", val) = var ++ "=" ++ val
tupleToString (var, "str", val) =  var  ++ "=" ++ "\"" ++ val ++ "\""

helperMap [] = []
helperMap (m:[]) = (tupleToString m) ++ " in "
helperMap (m:ms) = (tupleToString m) ++ ";" ++ (helperMap ms)

helperAST EmptyAST = []
helperAST (ASTNode "plus" l r) = "(" ++ helperAST(l) ++ "+" ++ helperAST(r) ++")"
helperAST (ASTNode "times" l r) = "(" ++ helperAST(l) ++ "*" ++ helperAST(r) ++")"
helperAST (ASTNode "cat" l r) = "(" ++ helperAST(l) ++ "++" ++ helperAST(r) ++")"
helperAST (ASTNode "negate" l EmptyAST) = "(" ++ "-" ++ helperAST(l) ++ ")"
helperAST (ASTNode "len" l EmptyAST) = "(" ++ "length " ++ helperAST(l) ++ ")"
helperAST (ASTNode "num" l EmptyAST) = getValue(l) 
helperAST (ASTNode "str" l EmptyAST) = "\"" ++ getValue(l) ++ "\""
helperAST (ASTNode var EmptyAST EmptyAST) = var

writeExpression (EmptyAST, []) = []
writeExpression (tree, []) = helperAST tree
writeExpression (tree, m) ="let " ++ (helperMap m) ++ (helperAST tree)

subsHelper EmptyAST m = EmptyAST
subsHelper (ASTNode v l r) m@(var, ty, val) = if v == var then (ASTNode ty (ASTNode val EmptyAST EmptyAST) EmptyAST) else ASTNode v (subsHelper l m) (subsHelper r m)
                                                                                                                                                                                                                                                                                                 

subs tree [] = tree
subs tree  (m:ms) = subs (subsHelper tree m) ms

evaluate (EmptyAST) = []
evaluate (ASTNode "plus" l r) =  show ((read $ evaluate(l) :: Int) + (read $ evaluate(r) :: Int))
evaluate (ASTNode "times" l r) = show ((read $ evaluate(l) :: Int) * (read $ evaluate(r) :: Int))
evaluate (ASTNode "negate" l EmptyAST) = show (- (read $(evaluate l) :: Int))
evaluate (ASTNode "len" l EmptyAST) = show (length (evaluate l))
evaluate (ASTNode "cat" l r) =  evaluate(l) ++ evaluate(r) 
evaluate (ASTNode "num" l EmptyAST) = getValue(l) 
evaluate (ASTNode "str" l EmptyAST) = getValue(l)

evaluateAST (tree, m) = (x, evaluate x) where x = subs tree m














