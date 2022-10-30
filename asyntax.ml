type asyn =
  | Int of int
  | Float of float
  | Add of asyn * asyn
  | Addf of asyn * asyn
  | Sub of asyn * asyn
  | Subf of asyn * asyn
  | Mul of asyn * asyn
  | Mulf of asyn * asyn
  | Div of asyn * asyn
  | Mod of asyn * asyn
  | Inttype of asyn
  | Floattype of asyn

exception MauvaisType

let rec typage a = match a with
  | Add(a,b) -> let ta = typage a and tb = typage b in if ta = 'i' && tb = 'i' then ta else raise MauvaisType
  | Addf(a,b) -> let ta = typage a and tb = typage b in if ta = 'f' && tb = 'f' then ta else raise MauvaisType
  | Sub(a,b) -> let ta = typage a and tb = typage b in if ta = 'i' && tb = 'i' then ta else raise MauvaisType
  | Subf(a,b) -> let ta = typage a and tb = typage b in if ta = 'f' && tb = 'f' then ta else raise MauvaisType
  | Mul(a,b) -> let ta = typage a and tb = typage b in if ta = 'i' && tb = 'i' then ta else raise MauvaisType
  | Mulf(a,b) -> let ta = typage a and tb = typage b in if ta = 'f' && tb = 'f' then ta else raise MauvaisType
  | Div(a,b) -> let ta = typage a and tb = typage b in if ta = 'i' && tb = 'i' then ta else raise MauvaisType
  | Mod(a,b) -> let ta = typage a and tb = typage b in if ta = 'i' && tb = 'i' then ta else raise MauvaisType
  | Inttype(_) -> 'i'
  | Floattype(_) -> 'f'
  | Int(_) -> 'i'
  | Float(_) -> 'f'



