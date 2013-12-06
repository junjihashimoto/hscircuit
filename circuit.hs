
import Data.List

one   = 1    :one
zero  = 0    :zero
not' []=[]
not' (x:xs)= (if x/=0 then 0 else 1) : (not' xs)
dff init xs=init:xs

clk =dff 1 (not' clk)

if' a b c = if (head a)/=0
            then (head b):(if' (tail a) (tail b) (tail c))
            else (head c):(if' (tail a) (tail b) (tail c))

eq a b= if (head a)==(head b)
        then 1:(eq (tail a) (tail b))
        else 0:(eq (tail a) (tail b))

b = if' one cnt zero 

add a b = ((head a)+(head b)) : (add (tail a) (tail b))

cnt  = dff 0 (add cnt  one)
cnt2 = dff 0 (add cnt2 one)

ram re radr we wadr wdat =
  (if' (eq re (eq radr 0)) ram0 
   (if' (eq re (eq radr 1)) ram1
    (if' (eq re (eq radr 2)) ram2
     (if' (eq re (eq radr 3)) ram3
      zero
     )
    )
   )
  )
  where
    ram0=dff 0 (if' (eq  we (eq wadr 0)) wdat ram0)
    ram1=dff 0 (if' (eq  we (eq wadr 1)) wdat ram1)
    ram2=dff 0 (if' (eq  we (eq wadr 2)) wdat ram2)
    ram3=dff 0 (if' (eq  we (eq wadr 3)) wdat ram3)

main=do
  print $ map (take 5) [cnt,cnt2,b]
