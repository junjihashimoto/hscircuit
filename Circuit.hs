module Circuit where
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

if' as bs cs = do
  (a,b,c) <- zip3 as bs cs
  if a/=0
    then return b
    else return c 
                           
eq   (a:as) (b:bs) = (if a==b then 1 else 0):(eq as bs)
and' (a:as) (b:bs) = (if a==1&&b==1 then 1 else 0):(and' as bs)
add  (a:as) (b:bs) = (a+b):(add as bs)

cnt  = dff (add cnt  one)
       
-- ram::Val->Val->Val->Val->Val->Val
-- ram re radr we wadr wdat =
--   dff (if' (eq radr (val 0)) ram0
--        (if' (eq radr (val 1)) ram1
--         (if' (eq radr (val 2)) ram2
--          (if' (eq radr (val 3)) ram3
--           (val 10)
--          )
--         )
--        )
--       )
--   where
--     ram0=dff (if' (and' we (eq wadr (val 0))) wdat ram0)
--     ram1=dff (if' (and' we (eq wadr (val 1))) wdat ram1)
--     ram2=dff (if' (and' we (eq wadr (val 2))) wdat ram2)
--     ram3=dff (if' (and' we (eq wadr (val 3))) wdat ram3)

ram4::Val->Val->Val->Val->Val->Val
ram4 re radr we wadr wdat =
  dff .
  (sel radr 0) .
  (sel radr 1) .
  (sel radr 2) .
  (sel radr 3) $ zero
  where
    ram v=dff (if' (and' we (eq wadr (val v))) wdat (ram v))
    sel adr v x =if' (eq adr (val v)) (ram v) x

seta ary we wadr wdat = do
  (ary_,e_,adr_,dat_)<-zip4 ary we wadr wdat
  if(e_==1)
    then return $ ary_ // [(adr_,dat_)]
    else return ary_

geta ary re radr = do
  (ary_,e_,adr_)<-zip3 ary re radr
  if(e_==1)
    then return $ ary_ ! adr_
    else return 0

s2ram size re radr we wadr wdat =do
  dff $ geta ram re radr
  where
    s    = size-1
    ary  = array (0,s) $ zip [0..s] [0..]
    ram = dff1 ary (seta ram we wadr wdat)

s1ram size cs we adr wdat =do
  dff $ geta ram cs adr
  where
    s    = size-1
    ary  = array (0,s) $ zip [0..s] [0..]
    ram  = dff1 ary (seta ram (and' cs we) adr wdat)
  


sdram size csa adra wea wdata 
           csb adrb web wdatb =do
  zip (dff $ geta ram csa adra) (dff $ geta ram csb adrb)
  where
    s    = size-1
    ary  = array (0,s) $ zip [0..s] [0..]
    ram = dff1 ary (seta
                    (seta ram (and' csa wea) adra wdata)
                    (and' csb web) adrb wdatb)

