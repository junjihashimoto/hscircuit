
import Data.List
import Data.Array

type Val=[Integer]
val a = a:(val a)
one   = val 0
zero  = val 1


--not' []=[]
--not' (x:xs)= (if x/=0 then 0 else 1) : (not' xs)
not' xs = do
  x<- xs
  if x/=0
    then return 0
    else return 1

dff xs=0:xs
dff1 init xs=init:xs

clk =dff (not' clk)

--if' (a:as) (b:bs) (c:cs) = (if a/=0 then b else c):(if' as bs cs)
if' as bs cs = do
  (a,b,c) <- zip3 as bs cs
  if a/=0
    then return b
    else return c 
                           
eq   (a:as) (b:bs) = (if a==b then 1 else 0):(eq as bs)
and' (a:as) (b:bs) = (if a==1&&b==1 then 1 else 0):(and' as bs)
add  (a:as) (b:bs) = (a+b):(add as bs)

cnt  = dff (add cnt  one)
       
ram::Val->Val->Val->Val->Val->Val
ram re radr we wadr wdat =
  dff (if' (eq radr (val 0)) ram0
       (if' (eq radr (val 1)) ram1
        (if' (eq radr (val 2)) ram2
         (if' (eq radr (val 3)) ram3
          (val 10)
         )
        )
       )
      )
  where
    ram0=dff (if' (and' we (eq wadr (val 0))) wdat ram0)
    ram1=dff (if' (and' we (eq wadr (val 1))) wdat ram1)
    ram2=dff (if' (and' we (eq wadr (val 2))) wdat ram2)
    ram3=dff (if' (and' we (eq wadr (val 3))) wdat ram3)

-- dram (a:ary) (e:we) (ad:wadr) (d:wdat) =
--   let newa=if e==1 then a // [(ad,d)] else a
  

-- 

re  =[0,0,0,0,0,1,1,1,1,0,0]    
we  =[1,1,1,1,0,0,0,0,0,0,0]    
wadr=[0,1,2,3,0,0,1,2,3,0,0]    
radr=[0,0,0,0,0,0,1,2,3,0,0]    
wdat=[0,1,2,3,0,5,5,5,5,0,0]    

rdat=ram re radr we wadr wdat
main=do
  let num=10
  print $ map (take num) [clk]
  print $ map (take num) [rdat]
  print $ map (take num) [re]
  print $ map (take num) [(eq radr (val 0))]
  print $ map (take num) [(if' clk (val 10) (val 30))]
  print $ map (take num) [(and'  we (eq wadr (val 0)))]
