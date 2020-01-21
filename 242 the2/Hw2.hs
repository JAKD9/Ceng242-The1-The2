module Hw2 where

data ASTResult = ASTError String | ASTJust (String, String, Int) deriving (Show, Read)
data ASTDatum = ASTSimpleDatum String | ASTLetDatum String deriving (Show, Read)
data AST = EmptyAST | ASTNode ASTDatum AST AST deriving (Show, Read)

isNumber :: String -> Bool
eagerEvaluation :: AST -> ASTResult
normalEvaluation :: AST -> ASTResult
-- DO NOT MODIFY OR DELETE THE LINES ABOVE -- 
-- IMPLEMENT isNumber, eagerEvaluation and normalEvaluation FUNCTIONS ACCORDING TO GIVEN SIGNATURES -- 

control char
    | char == '0' = True
    | char == '1' = True
    | char == '2' = True
    | char == '3' = True
    | char == '4' = True
    | char == '5' = True
    | char == '6' = True
    | char == '7' = True
    | char == '8' = True
    | char == '9' = True
    | otherwise = False
 
isNumberHelper [] = True
isNumberHelper (s:sx) = control s && isNumberHelper sx

isNumber [] = False
isNumber ('-':sx) = sx /= [] && isNumberHelper sx
isNumber l = isNumberHelper l

getValue EmptyAST = ""
getValue (ASTNode (ASTSimpleDatum val) l r) = val
getValue (ASTNode (ASTLetDatum val) l r) = val

getLeft EmptyAST = EmptyAST
getLeft (ASTNode d l r) = l

getRight EmptyAST = EmptyAST
getRight (ASTNode d l r) = r

findVar var (x:xs)
    | fst x == var = snd x
    | fst x /= var = (findVar var xs)



eagerCheck (ASTNode (ASTLetDatum var) left right) stack 
    | (snd checkLeft) /= "noError" = checkLeft 
    | snd checkRight /= "noError" = checkRight
    | otherwise = ("num","noError")
    where checkLeft = eagerCheck left stack
          newstack = [(var,checkLeft)] ++ stack
          checkRight = eagerCheck right newstack
          leftVal = fst checkLeft
          rightVal = fst checkRight
eagerCheck (ASTNode (ASTSimpleDatum "num") left right) stack = if isNumber leftValue then ("num","noError") else ("num","the value " ++"'" ++ leftValue ++ "'" ++ " is not a number!") where leftValue = getValue left
eagerCheck (ASTNode (ASTSimpleDatum "str") left right) stack = ("str","noError")    
eagerCheck (ASTNode (ASTSimpleDatum "plus") left right) stack 
    | snd checkLeft /= "noError" = checkLeft 
    | snd checkRight /= "noError" = checkRight
    | fst checkLeft /= "num" || fst checkRight /= "num" = ("num","plus operation is not defined between " ++ leftVal ++ " and " ++ rightVal ++ "!" )
    | otherwise = ("num","noError")
    where checkLeft = eagerCheck left stack
          checkRight = eagerCheck right stack
          leftVal = fst checkLeft
          rightVal = fst checkRight
eagerCheck (ASTNode (ASTSimpleDatum "times") left right) stack 
    | snd checkLeft /= "noError" = checkLeft 
    | snd checkRight /= "noError" = checkRight
    | fst checkLeft /= "num" || fst checkRight /= "num" = ("num","times operation is not defined between " ++ leftVal ++ " and " ++ rightVal ++ "!" )
    | otherwise = ("num","noError")
    where checkLeft = eagerCheck left stack
          checkRight = eagerCheck right stack
          leftVal = fst checkLeft
          rightVal = fst checkRight
eagerCheck (ASTNode (ASTSimpleDatum "cat") left right) stack 
    | snd checkLeft /= "noError" = checkLeft 
    | snd checkRight /= "noError" = checkRight
    | fst checkLeft /= "str" || fst checkRight /= "str" = ("str","cat operation is not defined between " ++ leftVal ++ " and " ++ rightVal ++ "!" )
    | otherwise = ("str","noError")
    where checkLeft = eagerCheck left stack
          checkRight = eagerCheck right stack
          leftVal = fst checkLeft
          rightVal = fst checkRight
eagerCheck (ASTNode (ASTSimpleDatum "negate") left right) stack 
    | snd checkLeft /= "noError" = checkLeft
    | fst checkLeft /= "num" = ("num","negate operation is not defined on str!")
    | otherwise = ("num","noError")
    where checkLeft = eagerCheck left stack
eagerCheck (ASTNode (ASTSimpleDatum "len") left righ) stack 
    | snd checkLeft /= "noError" = checkLeft
    | fst checkLeft /= "str" = ("num","len operation is not defined on num!")
    | otherwise = ("num","noError")
    where checkLeft = eagerCheck left stack
eagerCheck (ASTNode (ASTSimpleDatum var) left right ) stack = findVar var stack






normalCheck (ASTNode (ASTLetDatum var) left right) stack = normalCheck right newstack where newstack = [(var, normalCheck left stack)] ++ stack
normalCheck (ASTNode (ASTSimpleDatum "num") left right) stack = if isNumber leftValue then ("num","noError") else ("num","the value " ++"'" ++ leftValue ++ "'" ++ " is not a number!") where leftValue = getValue left
normalCheck (ASTNode (ASTSimpleDatum "str") left right) stack = ("str","noError")    
normalCheck (ASTNode (ASTSimpleDatum "plus") left right) stack 
    | snd checkLeft /= "noError" = checkLeft 
    | snd checkRight /= "noError" = checkRight
    | fst checkLeft /= "num" || fst checkRight /= "num" = ("num","plus operation is not defined between " ++ leftVal ++ " and " ++ rightVal ++ "!" )
    | otherwise = ("num","noError")
    where checkLeft = normalCheck left stack
          checkRight = normalCheck right stack
          leftVal = fst checkLeft
          rightVal = fst checkRight
normalCheck (ASTNode (ASTSimpleDatum "times") left right) stack 
    | snd checkLeft /= "noError" = checkLeft 
    | snd checkRight /= "noError" = checkRight
    | fst checkLeft /= "num" || fst checkRight /= "num" = ("num","times operation is not defined between " ++ leftVal ++ " and " ++ rightVal ++ "!" )
    | otherwise = ("num","noError")
    where checkLeft = normalCheck left stack
          checkRight = normalCheck right stack
          leftVal = fst checkLeft
          rightVal = fst checkRight
normalCheck (ASTNode (ASTSimpleDatum "cat") left right) stack 
    | snd checkLeft /= "noError" = checkLeft 
    | snd checkRight /= "noError" = checkRight
    | fst checkLeft /= "str" || fst checkRight /= "str" = ("str","cat operation is not defined between " ++ leftVal ++ " and " ++ rightVal ++ "!" )
    | otherwise = ("str","noError")
    where checkLeft = normalCheck left stack
          checkRight = normalCheck right stack
          leftVal = fst checkLeft
          rightVal = fst checkRight
normalCheck (ASTNode (ASTSimpleDatum "negate") left right) stack 
    | snd checkLeft /= "noError" = checkLeft
    | fst checkLeft /= "num" = ("num","negate operation is not defined on str!")
    | otherwise = ("num","noError")
    where checkLeft = normalCheck left stack
normalCheck (ASTNode (ASTSimpleDatum "len") left righ) stack 
    | snd checkLeft /= "noError" = checkLeft
    | fst checkLeft /= "str" = ("num","len operation is not defined on num!")
    | otherwise = ("num","noError")
    where checkLeft = normalCheck left stack
normalCheck (ASTNode (ASTSimpleDatum var) left right ) stack = findVar var stack
      
tupleFST (ASTJust (a,_,_)) = a
tupleSND (ASTJust (_,a,_)) = a
tupleTRD (ASTJust (_,_,a)) = a    
         

  
eagerEval (ASTNode (ASTSimpleDatum "plus") l r) stack = (ASTJust (p,"num",0)) where p = show ((read (tupleFST left) :: Int) + (read (tupleFST right) :: Int))
                                                                                    left = eagerEval l stack
                                                                                    right = eagerEval r stack
eagerEval (ASTNode (ASTSimpleDatum "times") l r) stack = (ASTJust (t,"num",0)) where t = show ((read (tupleFST left) :: Int) * (read (tupleFST right) :: Int))
                                                                                     left = eagerEval l stack
                                                                                     right = eagerEval r stack                                                                                     
eagerEval (ASTNode (ASTSimpleDatum "negate") l r) stack = (ASTJust (n,"num",0)) where n = show((read (tupleFST left) :: Int) * (-1))
                                                                                      left = eagerEval l stack
eagerEval (ASTNode (ASTSimpleDatum "cat") l r) stack = (ASTJust (c,"str",0)) where c = tupleFST left ++ tupleFST right
                                                                                   left = eagerEval l stack
                                                                                   right = eagerEval r stack
                                                                                    
eagerEval (ASTNode (ASTSimpleDatum "len") l r) stack = (ASTJust (le,"num",0)) where le = show (length (tupleFST left))
                                                                                    left = eagerEval l stack
eagerEval (ASTNode (ASTSimpleDatum "num") l r) stack = (ASTJust (getValue l , "num", 0))  
eagerEval (ASTNode (ASTSimpleDatum "str") l r) stack = (ASTJust (getValue l , "str", 0)) 
eagerEval (ASTNode (ASTLetDatum var1) l r) stack = (eagerEval r ([(var1,left)] ++ stack)) where left = eagerEval l stack
eagerEval (ASTNode (ASTSimpleDatum var2) l r) stack = find where find = findVar var2 stack

 
normalEval (ASTNode (ASTSimpleDatum "plus") l r) stack = (ASTJust (p,"num",z2)) where p = show ((read (tupleFST left) :: Int) + (read (tupleFST right) :: Int))
                                                                                      left = normalEval l stack
                                                                                      right = normalEval r stack
                                                                                      z2 = (tupleTRD left) + (tupleTRD right) + 1 
normalEval (ASTNode (ASTSimpleDatum "times") l r) stack = (ASTJust (t,"num",z2)) where t = show ((read (tupleFST left) :: Int) * (read (tupleFST right) :: Int))
                                                                                       left = normalEval l stack
                                                                                       right = normalEval r stack
                                                                                       z2 = (tupleTRD left) + (tupleTRD right) + 1 
normalEval (ASTNode (ASTSimpleDatum "negate") l r) stack = (ASTJust (n,"num",z1)) where n = show((read (tupleFST left) :: Int) * (-1))
                                                                                        left = normalEval l stack
                                                                                        z1 = (tupleTRD left) + 1 
normalEval (ASTNode (ASTSimpleDatum "cat") l r) stack = (ASTJust (c,"str",z2)) where c = tupleFST left ++ tupleFST right
                                                                                     left = normalEval l stack
                                                                                     right = normalEval r stack
                                                                                     z2 = (tupleTRD left) + (tupleTRD right) + 1 
normalEval (ASTNode (ASTSimpleDatum "len") l r) stack = (ASTJust (le,"num",z1)) where le = show (length (tupleFST left))
                                                                                      left = normalEval l stack
                                                                                      z1 = (tupleTRD left) + 1 
normalEval (ASTNode (ASTSimpleDatum "num") l r) stack = (ASTJust (getValue l , "num", 0))  
normalEval (ASTNode (ASTSimpleDatum "str") l r) stack = (ASTJust (getValue l , "str", 0)) 
normalEval (ASTNode (ASTLetDatum var1) l r) stack = (normalEval r ([(var1,left)] ++ stack)) where left = normalEval l stack
normalEval (ASTNode (ASTSimpleDatum var2) l r) stack = find where find = findVar var2 stack
   
   
eagerCounter EmptyAST = 0    
eagerCounter tree
    | value  == "plus" = eagerCounter left + eagerCounter right + 1
    | value  == "times" = eagerCounter left + eagerCounter right + 1
    | value  == "cat" = eagerCounter left + eagerCounter right + 1
    | value  == "negate" = eagerCounter left + 1
    | value  == "len" = eagerCounter left + 1
    | value  == "num" = 0
    | value  == "str" = 0
    | otherwise = eagerCounter left + eagerCounter right
    where value = getValue tree
          left = getLeft tree
          right = getRight tree       
eagerEvaluation tree
    | snd error == "noError" = ASTJust (tupleFST ans,tupleSND ans, step) 
    | otherwise = ASTError (snd error)
    where ans = (eagerEval tree [])
          error = eagerCheck tree []
          step = eagerCounter tree
     
normalEvaluation tree
    | snd error == "noError" = normalEval tree []
    | otherwise = ASTError (snd error)
    where error = normalCheck tree []
           
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

