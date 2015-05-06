%token <string> ID
%token LPAREN
%token RPAREN
%token EOF

%start <Ptbtree.Ptbtree.string_tree list> treel

%%

treel: l = treev* ; EOF {l} ;

treev:
  | LPAREN ; t = tree ; RPAREN { t }
  ;


tree:
  | LPAREN ; n = ID ; t = ID    ; RPAREN  { Leaf(n,t)}
  | LPAREN ; n = ID ; l = tree+ ; RPAREN  { Node(n,l)}
  ;
