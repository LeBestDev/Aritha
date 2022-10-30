open Asyntax
(*Création du buffer*)
let code = Buffer.create 16
(*Fonction pour revenir à la ligne*)
let revenir_ligne code = Buffer.add_string code "\n"
(*En-tête du code assembleur*)
let initialisation code = 
  Buffer.add_string code "\t.text"; 
  revenir_ligne code ; 
  Buffer.add_string code "\t.globl main" ; 
  revenir_ligne code ; 
  Buffer.add_string code "main:" ; 
  revenir_ligne code
(*Création d'un second buffer qui stockera les déclarations de flotant, il sera ensuite inséré à la fin du code*)
let declare_float = Buffer.create 16
(*Compteur de flotant*)
let nb_float = ref 0
(*Fonction qui permet de déclarer un flotant*)
let ajout_float_var code x = 
  Buffer.add_string code ("F"^string_of_int !nb_float^":");
  revenir_ligne code;
  Buffer.add_string code ("\t.double "^string_of_float x);
  revenir_ligne code;
  incr nb_float

(*Parcours de l'arbre syntaxique abstrait*)
(*Pour les flotants, on modifie la pile à la main en deplaçant %rsp*)
(*Pour les entiers, on utilise popq et pushq*)
let rec main_code c = match c with
  | Inttype(a) -> 
    main_code a;
    if typage a = 'f' then (
    Buffer.add_string code "\tmovsd (%rsp), %xmm0";
    revenir_ligne code;
    Buffer.add_string code "\taddq $8, %rsp";
    revenir_ligne code;
    Buffer.add_string code "\tcvttsd2si %xmm0, %rax";
    revenir_ligne code;
    Buffer.add_string code "\tpushq %rax";
    revenir_ligne code;)
  | Floattype(a) ->
    main_code a;
    if typage a = 'i' then (
    Buffer.add_string code "\tpopq %rax";
    revenir_ligne code;
    Buffer.add_string code "\tcvtsi2sd %rax, %xmm0";
    revenir_ligne code;
    Buffer.add_string code "\tsubq $8, %rsp";
    revenir_ligne code;
    Buffer.add_string code "\tmovsd %xmm0, (%rsp)";
    revenir_ligne code;)
  | Add(a,b) ->
    main_code a;
    main_code b;
    Buffer.add_string code "\tpopq %rax" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpopq %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\taddq %rax, %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpushq %rbx" ;
    revenir_ligne code 
  | Sub(a,b) ->
    main_code a;
    main_code b; 
    Buffer.add_string code "\tpopq %rax" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpopq %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tsubq %rax, %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpushq %rbx" ;
    revenir_ligne code 
  | Mul(a,b) ->
    main_code a;
    main_code b;
    Buffer.add_string code "\tpopq %rax" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpopq %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\timulq %rax, %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpushq %rbx" ;
    revenir_ligne code 
  | Div(a,b) ->
    main_code a;
    main_code b;
    Buffer.add_string code "\txor %rdx, %rdx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpopq %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpopq %rax" ;
    revenir_ligne code ;
    Buffer.add_string code "\tidivq %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpushq %rax" ;
    revenir_ligne code
  | Mod(a,b) ->
    main_code a;
    main_code b;
    Buffer.add_string code "\txor %rdx, %rdx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpopq %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpopq %rax" ;
    revenir_ligne code ;
    Buffer.add_string code "\tidivq %rbx" ;
    revenir_ligne code ;
    Buffer.add_string code "\tpushq %rdx" ;
    revenir_ligne code 
  | Int(i) ->
    Buffer.add_string code "\tpushq $" ;
    Buffer.add_string code (string_of_int i) ;
    revenir_ligne code
  | Addf(a,b) ->
    main_code a;
    main_code b;
    Buffer.add_string code "\tmovsd (%rsp), %xmm1" ;
    revenir_ligne code ;
    Buffer.add_string code "\tmovsd 8(%rsp), %xmm0" ;
    revenir_ligne code ;
    Buffer.add_string code "\taddq $16, %rsp" ;
    revenir_ligne code ;
    Buffer.add_string code "\taddsd %xmm1, %xmm0" ;
    revenir_ligne code ;
    Buffer.add_string code "\tmovsd %xmm0, -8(%rsp)" ;
    revenir_ligne code ;
    Buffer.add_string code "\tsubq $8, %rsp" ;
    revenir_ligne code ;
  | Subf(a,b) ->
    main_code a;
    main_code b;
    Buffer.add_string code "\tmovsd (%rsp), %xmm1" ;
    revenir_ligne code ;
    Buffer.add_string code "\tmovsd 8(%rsp), %xmm0" ;
    revenir_ligne code ;
    Buffer.add_string code "\taddq $16, %rsp" ;
    revenir_ligne code ;
    Buffer.add_string code "\tsubsd %xmm1, %xmm0" ;
    revenir_ligne code ;
    Buffer.add_string code "\tmovsd %xmm0, -8(%rsp)" ;
    revenir_ligne code ;
    Buffer.add_string code "\tsubq $8, %rsp" ;
    revenir_ligne code ;
  | Mulf(a,b) ->
    main_code a;
    main_code b;
    Buffer.add_string code "\tmovsd (%rsp), %xmm1" ;
    revenir_ligne code ;
    Buffer.add_string code "\tmovsd 8(%rsp), %xmm0" ;
    revenir_ligne code ;
    Buffer.add_string code "\taddq $16, %rsp" ;
    revenir_ligne code ;
    Buffer.add_string code "\tmulsd %xmm1, %xmm0" ;
    revenir_ligne code ;
    Buffer.add_string code "\tmovsd %xmm0, -8(%rsp)" ;
    revenir_ligne code ;
    Buffer.add_string code "\tsubq $8, %rsp" ;
    revenir_ligne code ;
  | Float(f) ->
    Buffer.add_string code ("\tmovsd F"^ string_of_int !nb_float^", %xmm0") ;
    revenir_ligne code ;
    Buffer.add_string code "\tmovsd %xmm0, -8(%rsp)" ;
    revenir_ligne code;
    Buffer.add_string code "\tsubq $8, %rsp" ;
    revenir_ligne code ;
    ajout_float_var declare_float f

(*Fonction qui écrit la fin du code, notamment les déclarations de flotants*)
let finalisation code typage =( match typage with
|'i' -> 
  Buffer.add_string code "\tpopq %rax" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmovq %rax, %rdi" ;
  revenir_ligne code ;
  Buffer.add_string code "\tcall print_int" ;
  revenir_ligne code ;
  Buffer.add_string code "\tret" ;
  revenir_ligne code ; revenir_ligne code ;
  Buffer.add_string code "print_int:" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmov %rdi, %rsi" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmov $convint, %rdi" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmov $0, %rax" ;
  revenir_ligne code ;
  Buffer.add_string code "\tcall printf" ;
  revenir_ligne code ;
  Buffer.add_string code "\tret" ;
  revenir_ligne code ; revenir_ligne code ;
|'f'->
  Buffer.add_string code "\tmovsd (%rsp), %xmm0" ;
  revenir_ligne code ;
  Buffer.add_string code "\taddq $8, %rsp" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmovq %xmm0, %rdi" ;
  revenir_ligne code ;
  Buffer.add_string code "\tcall print_float" ;
  revenir_ligne code ;
  Buffer.add_string code "\tret" ;
  revenir_ligne code ; revenir_ligne code ;
  Buffer.add_string code "print_float:" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmovq %rsp, %rbp" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmovq %rdi, %xmm0" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmovl $convfloat, %edi" ;
  revenir_ligne code ;
  Buffer.add_string code "\tmovl $1, %eax" ;
  revenir_ligne code ;
  Buffer.add_string code "\tcall printf" ;
  revenir_ligne code ;
  Buffer.add_string code "\tnop" ;
  revenir_ligne code ;
  Buffer.add_string code "\tleave" ;
  revenir_ligne code ;
  Buffer.add_string code "\tret" ;
  revenir_ligne code ;
  Buffer.add_string code "\t\tnop" ;
  revenir_ligne code ;
  Buffer.add_string code "\tleave" ;
  revenir_ligne code ;
  Buffer.add_string code "\tret" ;
  revenir_ligne code ; revenir_ligne code ;
  |_->());

let s = Buffer.contents declare_float in
Buffer.add_string code s;
revenir_ligne code;
Buffer.add_string code "\t.data" ;
revenir_ligne code ;
Buffer.add_string code "convint:" ;
revenir_ligne code ;
Buffer.add_string code "\t.string \"%d\"";
revenir_ligne code; revenir_ligne code;
Buffer.add_string code "convfloat:" ;
revenir_ligne code ;
Buffer.add_string code "\t.string \"%f\"";
revenir_ligne code

let efface () =
  Buffer.clear code;
  Buffer.clear declare_float;
  nb_float := 0

(*Fonction qui écrit le code assembleur*)
let compile_code a =
  let typage = typage a in
  initialisation code;
  main_code a;
  finalisation code typage;
  Buffer.contents code

(*Fonction qui écrit le code assembleur dans un fichier*)
let affiche_code nom_fichier code =
  let nom = String.sub nom_fichier 0 (String.length nom_fichier - 4) in
  let oc = open_out (nom^".s") in
  (* clear code.s*)
  output_string oc "";
  output_string oc code;
  efface ();
  close_out oc