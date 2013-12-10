import Circuit
import Data.List  

re  =[0,0,0,0,0,1,1,1,1,0,0]++re  
we  =[1,1,1,1,0,0,0,0,0,0,0]++we      
wadr=[0,1,2,3,0,0,1,2,3,0,0]++wadr    
radr=[0,0,0,0,0,0,1,2,3,0,0]++radr    
wdat=[0,1,2,3,0,5,5,5,5,0,0]++wdat    

r2dat =s2ram 4 re radr we wadr wdat
r1dat =s1ram 4 (and' re we) we (if' we wadr radr) wdat
-- rddat=dram 4 re radr we wadr wdat
main' x =do
  print $ head x
  main' $ tail x

main=do
  main' $ zip7 re we wadr radr wdat r2dat r1dat
