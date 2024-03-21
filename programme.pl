% :- consult('nomfichierexemple').
% CHARGER UN FICHIER EXEMPLE OU INSERER UN EXEMPLE 

% BENAHMED Neil 21200977 & KADEM Hocine 21309534

/*
╭───────────────────────────────────────────────────╮
│                EXEMPLE DE TEST                    │
╰───────────────────────────────────────────────────╯
*/
% Tbox 
equiv(sculpteur,and(personne,some(aCree,sculpture))).
equiv(auteur,and(personne,some(aEcrit,livre))).
equiv(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))).
equiv(parent,and(personne,some(aEnfant,anything))).
equiv(animal,not(personne)).

% concept atomique
cnamea(personne).
cnamea(livre).
cnamea(objet).
cnamea(sculpture).
cnamea(anything).
cnamea(nothing).

% concept non atomique
cnamena(auteur).
cnamena(editeur).
cnamena(sculpteur).
cnamena(parent).
cnamena(animal).

% instances
iname(michelAnge).
iname(david).
iname(sonnets).
iname(vinci).
iname(joconde).
iname(garfield).
iname(jon). 

% rôles
rname(aCree).
rname(aEcrit).
rname(aEdite).
rname(aEnfant).
rname(aDessine).

% instanciation de concept
inst(michelAnge,personne).
inst(david,sculpture).
inst(sonnets,livre).
inst(vinci,personne).
inst(joconde,objet).
inst(garfield,animal).
inst(jon,personne). 

% instanciation de rôle
instR(michelAnge, david, aCree).
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).
instR(jon, garfield, aEnfant). % Propriètere de Garfield (son papa)


/*
╭───────────────────────────────╮
│          main                 │
├───────────────────────────────┤
│                               │
│       ╭───────────────────╮   │
│       │                   │   │
│       │     ┏━━━━━━━━━┓   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┗━━━━━━━━━┛   │   │
│       │                   │   │
│       ╰───────────────────╯   │
│                               │
╰───────────────────────────────╯
*/

/*
╭───────────────────────────────────────────────────╮
│               Programme principale                │
╰───────────────────────────────────────────────────╯
*/
main :-

    % Execution de la première étape 
    print_colored('╭───────────────────────────────────────╮\n', 35),
    print_colored('│     Début de la première étape        │\n', 35),
    print_colored('╰───────────────────────────────────────╯\n\n', 35),

    premiere_etape(Tbox, Abi, Abr),   

    print_colorednl('Affichage des boxs transformés : ',36),
    writeBoxs(Tbox,Abi,Abr),

    % Execution de la deuxième étape  
    print_colored('╭───────────────────────────────────────╮\n', 35),
    print_colored('│    Début de la deuxième étape         │\n', 35),
    print_colored('╰───────────────────────────────────────╯\n\n', 35),    

    deuxieme_etape(Abi, Abi1, Tbox),

    print_colorednl('Affichage des boxs avec instertions de la négation de la proposition a vérifier: ',36),
    writeBoxs(Tbox,Abi1,Abr),

    % Execution de la troisème étape 
    print_colored('╭───────────────────────────────────────╮\n', 35),
    print_colored('│    Début de la troisième étape        │\n', 35),
    print_colored('╰───────────────────────────────────────╯\n\n', 35),

    (troisieme_etape(Abi1, Abr)->
        print_colorednl('[RES] Il existe une feuille ouverte : la proposition initiale est NON VALIDE :(', 31);
        print_colorednl('[RES] Toutes les feuilles sont fermées : la proposition initiale est VALIDE :)', 35)),nl,
    write('[PROG] Programme terminé avec succès ............"),').

main.
/*
╭──────────────────────────────────────────────╮
│              Première étape                  │
╰──────────────────────────────────────────────╯
*/
premiere_etape(Tboxt, Abct, Abr) :-
    % Récupération des boxs

    % Récupérer la Tbox
    setof((CA, CG), equiv(CA, CG), Tbox), 

    % Récupérer la Abox d'insertion de concept
    setof((I1, I2), inst(I1, I2), Abc),         

    % Récupérer la Abox d'insertion de rôle
    setof((I1, I2, R), instR(I1, I2, R), Abr),   
    
    % Affichage des boxs
    print_colorednl('Affichage des boxs : ',36),
    writeBoxs(Tbox,Abc,Abr),

    % Vérification des boxs
    % Vérification de la Tbox
    writenl('[PROG] Vérification de la TBox ............'), 
    (verifTbox(Tbox) ->
        writenl('[PROG] Vérification de la TBox réussi ............');
        print_colorednl('[ERROR] Erreur syntaxique dans la TBox',31), nl, halt),
    
    % Vérification de la Abox
    writenl('[PROG] Vérification de la ABox ............'),
    (verifAbox(Abc, Abr) ->
        writenl('[PROG] Vérification de la ABox réussi ............');
        print_colorednl('[ERROR] Erreur syntaxique dans la ABox',31), nl, halt),
    
    % Vérification que la Tbox n'est pas circulaire
    (nonCirclTbox(Tbox) ->
        writenl('[PROG] Tbox non circulaire ............') ;
        print_colorednl('[ERROR] Tbox circulaire',31), nl, halt),

    % Transformation des boxs : mise sous forme normale négative et simplification des concepts non atomique 
    writenl('[PROG] Simplification des boxs ............'), 
    traitement_box(Tbox, Tboxt),
    traitement_box(Abc, Abct),
    writenl('[PROG] Traitement de la Tbox et des Abox terminé avec succès ............'),nl.


/*
╭──────────────────────────────────────────────╮
│              Deuxième étape                  │
╰──────────────────────────────────────────────╯
*/
deuxieme_etape(Abi, Abi1, Tbox) :-

    % Choix de la proposition
    print_colorednl('Entrez le numéro du type de proposition que vous voulez démontrer :',36), 
    print_colorednl('1 - type I : C (Vérifié qu\'une instance I appartient a un concept C)',36), 
    print_colorednl('2 - type C1 \u2293 C2 \u22d0 \u22a5 (Vérifier si deux concepts C1 et C2 sont disjoints)',36), 
    read(NumProposition),

    (
        NumProposition =:= 1 -> acquisition_prop_type1(Abi, Abi1, Tbox), !
    ;
        NumProposition =:= 2 -> acquisition_prop_type2(Abi, Abi1, Tbox), !
    ;
        print_colorednl('Veuillez choisir une des deux proposition : saisir 1. ou 2. pour choisir',31),nl,
        deuxieme_etape(Abi, Abi1, Tbox)
    ).

/*
╭──────────────────────────────────────────────╮
│              Troisième étape                 │
╰──────────────────────────────────────────────╯
*/
troisieme_etape(Abi, Abr) :-

    % Trie de la Abox en plusieurs liste d'assertion de concept
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls),
    writenl('[PROG] Trie de la Abox terminé avec succès ............'),

    % Lancement de l'algortihme de résolution
    resolution(Lie, Lpt, Li, Lu, Ls, Abr).




/*
╭───────────────────────────────╮
│          PARTIE 1             │
├───────────────────────────────┤
│                               │
│       ╭───────────────────╮   │
│       │                   │   │
│       │     ┏━━━━━━━━━┓   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┗━━━━━━━━━┛   │   │
│       │                   │   │
│       ╰───────────────────╯   │
│                               │
╰───────────────────────────────╯
*/
/*
╭───────────────────────────────╮
│ Vérification sémantique       │        
╰───────────────────────────────╯
*/
cname(CA) :- cnamea(CA).
cname(CN) :- cnamena(CN).
concept(C) :- cname(C). % vérification des concept atomique ou non atomique
role(R) :- rname(R). % vérification des identificateur de rôles
instance(I) :- iname(I). % vérification des identificateur d'instances

% vérification de la grammaire de la logique ALC
concept(not(C1)) :- concept(C1).
concept(and(C1,C2)) :- concept(C1), concept(C2).
concept(or(C1,C2)) :- concept(C1), concept(C2).
concept(some(R,C1)) :- role(R), concept(C1).
concept(all(R,C1)) :- role(R), concept(C1).

/*
╭───────────────────────────────╮
│ Vérification syntaxique       │        
╰───────────────────────────────╯
*/

% vérification Tbox : 
verifTbox([]).
verifTbox([(C,D) | LTbox]) :- verifTbox(LTbox), concept(C), concept(D).

% vérification Abox des concepts :
verifAboxConcept([]).
verifAboxConcept([(I,C) | LConcept]) :- verifAboxConcept(LConcept), instance(I), concept(C).

% vérification Abox des rôles :
verifAboxRole([]).
verifAboxRole([(I1,I2,R) | LRole]) :- verifAboxRole(LRole), instance(I1), instance(I2), role(R).

% vérification Abox :
verifAbox(LConcept,LRole) :- verifAboxConcept(LConcept), verifAboxRole(LRole).


/*
╭──────────────────────────────────────────────╮
│ Vérification des concept auto-référent       │        
╰──────────────────────────────────────────────╯
*/
% verification si une Tbox est non circulaire
nonCirclTbox([]).
nonCirclTbox([(C,D) | LTbox]) :-  
                        \+ autoref((C,D), LTbox),
                        nonCirclTbox(LTbox)
                        .

% tester si un concept n’est pas auto-référent
autoref((C,D), _) :- 
                        cname(X), % X est un concept atomique ou non atomique
                        in(X,D), % X est dans l'expression conceptuelle D
                        X = C % Si C est dans l'expression conceptuelle D alors on a une autoréférence
                        .
autoref((C,D), L) :- 
                        cname(X), % X est un concept atomique ou non atomique
                        in(X,D), % X est dans l'expression conceptuelle D
                        equiv(X,Z), % Z est l'expression conceptuelle de X 
                        autoref((C,Z) , L) % Récursion sur autoref avec C et Z
                        .

% verifie si le premier paramètre est inclut dans le deuxième paramètre
in(X,X).
in(X,and(A,_)) :- in(X,A).
in(X,and(_,B)) :- in(X,B).
in(X,or(A,_)) :- in(X,A).
in(X,or(_,B)) :- in(X,B).
in(X,not(A)) :- in(X,A).
in(X,some(_,A)) :- in(X,A).
in(X,any(_,A)) :- in(X,A).

/*
╭───────────────────────╮
│ Traitement box        │        
╰───────────────────────╯
*/
traitement_box([],[]).
traitement_box([(C,D)|LT],[(C,DR)|LTRes]) :- 
                                            transAtom(D,DA), % transforme l'expression conceptuelle D en une expression 
                                                             % où ne figurent plus que des identificateurs de concepts atomiques
                                            nnf(DA,DR), % transforme DR en forme normal negative
                                            traitement_box(LT,LTRes) % récurssion
                                            .

% transforme l'expression conceptuelle E en une expression EA où ne figurent plus que des identificateurs de concepts atomiques

% Si l'element est atomique on arrête
transAtom(E,E) :- cnamea(E). 

% On remplace E par son expression conceptuelle 
transAtom(E,EA) :- cnamena(E), equiv(E,DE), transAtom(DE,EA). % ici on vérifie si E est non atomique pour pouvoir récupérer son expression conceptuelle si c'est le cas.

% On developpe chaque membre en utilisant la récurssion pour arriver au élements atomiques ou non atomiques
transAtom(not(E),not(EA)) :- transAtom(E,EA). 
transAtom(and(E1,E2),and(EA1,EA2)) :- transAtom(E1,EA1), transAtom(E2,EA2).
transAtom(or(E1,E2),or(EA1,EA2)) :- transAtom(E1,EA1), transAtom(E2,EA2).
transAtom(some(R,E),some(R,EA)) :- transAtom(E,EA).
transAtom(all(R,E),all(R,EA)) :- transAtom(E,EA).



/*
╭───────────────────────────────╮
│          PARTIE 2             │
├───────────────────────────────┤
│                               │
│       ╭───────────────────╮   │
│       │                   │   │
│       │     ┏━━━━━━━━━┓   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┗━━━━━━━━━┛   │   │
│       │                   │   │
│       ╰───────────────────╯   │
│                               │
╰───────────────────────────────╯
*/

/*   
╭──────────────────────────────────────────────╮  
│ Traitement des proposition du type 1  :      │          
╰──────────────────────────────────────────────╯  
*/

acquisition_prop_type1(Abi,Abil,Tbox) :- 
    read_prop_type1(I,C),              % lecture de l'input de la proposition du type I : C
    transAtom(C,CS),                   % transforme le concepte C en un concepte composée uniquement d'élement atomique
    nnf(not(CS),NCS),                  % mise en forme normal negative
    concatene(Abi, [(I,NCS)], Abil).   % ajout de la negation de l'instance de concepte dans la Abox

read_prop_type1(I, C) :-
    print_colorednl("Vérification de la proposition : I : C",36),
    repeat,
    (
        print_colorednl("Saisir l'instance I :",36),
        read(I),
        (
            instance(I) ->
                true
            ;
                print_colorednl("%w n\'est pas une instance déclarée", [I], 31),
                print_colorednl("Veuillez recommencer", 31),nl,
                fail
        )
    ),
    repeat,
    (
        print_colorednl("Saisir le concept C :",36),
        read(C),
        (
            concept(C) ->
                true
            ;
                print_colorednl("%w n\'est pas un concept déclaré", [C], 31),
                print_colorednl("Veuillez recommencer", 31),nl,
                fail
        )
    ),
    nl,print_colorednl("Proposition à vérifier : [%w : %w]",[I,C],33),
    print_colorednl("Ajout de la négation de cette proposition : [%w : \u00AC(%w)]",[I,C],33),nl.

/*
╭──────────────────────────────────────────────╮
│ Traitement des proposition du type 2  :      │        
╰──────────────────────────────────────────────╯
*/
acquisition_prop_type2(Abi,Abil,Tbox) :- 
    read_prop_type2(C1, C2),            % lecture de l'input de la propposition du type C1 ⊓ C2 ⊑ ⊥
    transAtom(C1, CS1),                 % transforme le concepte C1 en un concepte composée uniquement d'élement atomique
    transAtom(C2, CS2),                 % IDEM pour C2
    nnf(and(CS1,CS2),C1interC2),        % mise en forme normal negative
   
    % ajout de la proposition ∃ inst, inst : C1 ⊓ C2 dans la Abox
    genere(I), % Création de l'instance %(instance(I),)
    print_colorednl("Ajout de la négation de cette proposition : [\u2203 %w, %w : %w \u2A05 %w]",[I,I,C1,C2],'33'),
    concatene(Abi, [(I,C1interC2)], Abil). % Insertion dans la Abox

read_prop_type2(C1, C2) :-
    print_colorednl("Vérification de la proposition C1 \u2293 C2 \u22d0 \u22a5",36),
    repeat,
    (
        print_colorednl("Saisir l'instance C1 :",36),
        read(C1),
        (
            concept(C1) ->
                true
            ;
                print_colorednl('%w n\'est pas un concept déclaré', [C1],31),
                print_colorednl('Veuillez recommencer',31),
                fail
        )
    ),
    repeat,
    (
        print_colorednl("Saisir l'instance C2 :",36),
        read(C2),
        (
            concept(C2) ->
                true
            ;
                print_colorednl('%w n\'est pas un concept déclaré', [C2],31),
                print_colorednl('Veuillez recommencer',31),
                fail
        )
    ),
    nl,print_colorednl("Proposition à vérifier : [%w \u2293 %w \u22d0 \u22a5]",[C1,C2],33).



/*
╭───────────────────────────────╮
│          PARTIE 3             │
├───────────────────────────────┤
│                               │
│       ╭───────────────────╮   │
│       │                   │   │
│       │     ┏━━━━━━━━━┓   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┗━━━━━━━━━┛   │   │
│       │                   │   │
│       ╰───────────────────╯   │
│                               │
╰───────────────────────────────╯
*/
/*
╭──────────────────────────────────────────────╮
│               Fonction tri_Abox              │
╰──────────────────────────────────────────────╯
    Génère 5 listes à partir de la liste des assertions de concepts de la Abox étendue.
    On traite les assertions une par une en les insérant dans leur liste correspondante.
*/
% Fin du tri
tri_Abox([], [], [], [], [], []).

% Traitement des assertions du type (I,some(R,C))
tri_Abox([(I,some(R,C))| Abi], [(I, some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).

% Traitement des assertions du type (I,all(R,C))
tri_Abox([(I, all(R,C)) | Abi], Lie, [(I, all(R,C)) | Lpt], Li, Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).

% Traitement des assertions du type (I,all(R,C))
tri_Abox([(I, and(C1,C2)) | Abi], Lie, Lpt, [(I, and(C1,C2)) | Li], Lu, Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).

% Traitement des assertions du type (I,all(R,C))
tri_Abox([(I, or(C1,C2)) | Abi], Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls) :-
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).

% Traitement des assertions du type (I,all(R,C))
tri_Abox([(I,C)|Abi], Lie, Lpt, Li, Lu, [(I,C)|Ls]) :-
    cname(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).

% Traitement des assertions du type (I,C) et du type (I,not(C))
tri_Abox([(I,not(C))|Abi], Lie, Lpt, Li, Lu, [(I,not(C))|Ls]) :-
    cname(C),
    tri_Abox(Abi, Lie, Lpt, Li, Lu, Ls).


/*
╭──────────────────────────────────────────────╮
│              Fonction Resolution             │
╰──────────────────────────────────────────────╯
    Implémente l'algorithme de résolution.
*/
% Traitement du cas d'une Abox vide
resolution([], [], [], [], [], _).

% Traitement des assertions du type (I,some(R,C))
resolution(Lie, Lpt, Li, Lu, Ls, Abr) :-
    not_clash(Ls),
    complete_some(Lie, Lpt, Li, Lu, Ls, Abr).

% Traitement des assertions du type (I,and(C1,C2))
resolution([], Lpt, Li, Lu, Ls, Abr) :-
    not_clash(Ls),
    transformation_and([], Lpt, Li, Lu, Ls, Abr).

% Traitement des assertions du type (I,all(R,C))
resolution([], Lpt, [], Lu, Ls, Abr) :-
    not_clash(Ls),
    deduction_all([], Lpt, [], Lu, Ls, Abr).

% Traitement assertions du type (I,or(C1,C2))
resolution([], [], [], Lu, Ls, Abr) :-
	not_clash(Ls),
	transformation_or([], [], [], Lu, Ls, Abr).

% Traitement des assertions du type (I,C) ou du type (I,not(C)) (Fin de l'algorithme)
resolution([], [], [], [], Ls, Abr) :-
    write("Traitement d'une feuille"),nl,nl,
    (not_clash(Ls)->print_colored("Pas de clash dans ce noeud",'32')
    ;
    print_colored("Clash dans ce noeud",'31'),nl),
    nl,
    nl, write('============================='), nl, nl,
    not_clash(Ls).


/* 
    Tester si une liste d'assertions du type (I,C) ou du type (I,not(C)) continent un clash 
*/

not_clash([]).

not_clash([(I, D) | Ls])  :-        % Vérification si il n'ya pas de clash dans la liste dans le cas D et not(D) dans L
        cname(D),                   % Oblige D à etre un élement atomique ou non atomique  simple : ( sans not(X) )
        \+ member((I,not(D)), Ls),  % Si not(D) est dans la liste Ls, alors il y'a clash ( retourne faux )
        not_clash(Ls). 
                          
not_clash([(I, not(D))| Ls]) :-     % Vérification si il n'ya pas de clash dans la liste dans le cas not(D) et D dans L
        cname(D),                   % Oblige D à etre un élement atomique ou non atomique simple : ( sans not(X) )
        \+ member((I, D), Ls),      % Si D est dans la liste Ls, alors il y'a clash ( retourne faux )
        not_clash(Ls). 

/*
╭─────────────────────────────────────────────╮
│   Application de la règle du il existe ∃    │
╰─────────────────────────────────────────────╯
*/
complete_some([(I, some(R, C)) | Lie], Lpt, Li, Lu, Ls, Abr) :-
    writef("Application de la regle \u2203 pour %w : %w", [I,R]),
    write(" \u2203 "),
    write_concept(C),nl,

    % Creation d'une nouvelle instanciation b
    genere(B),
    
    % Application de la règle du il existe 
    % (Création d'un nouveau noeud avec les assertions <a, b> : R et (b : C))
    
    % Ajout de l'insertion (b : C)
    evolue((B,C), Lie, Lpt, Li, Lu, Ls, NewLie, NewLpt, NewLi, NewLu, NewLs),

    % Affichage du résultat (avec l'ajout de l'insertion <a, b> : R et (b : C))
    affiche_evolution_Abox([(I, some(R, C)) | Lie], Lpt, Li, Lu, Ls, Abr, NewLs, NewLie, NewLpt, NewLi, NewLu, [(I,B,R) | Abr]),

    % Continuer la résolution (continuer l'algorithme sur le nouveau noeud crée) (avec l'ajout de l'insertion <a, b> : R)
    resolution(NewLie, NewLpt, NewLi, NewLu, NewLs, [(I,B,R) | Abr]).

/*
╭───────────────────────────────────────────────────╮
│   Application de la règle de l'intersection  ⊓    |
╰───────────────────────────────────────────────────╯
*/
transformation_and(Lie, Lpt, [(I,and(C1,C2)) | Li], Lu, Ls, Abr) :-

    % Affichage :
    writef("Application de la regle \u2293 pour %w :", [I]),
    write_concept(C1),
    write(" \u2293 "),
    write_concept(C2),nl,nl,

    evolue((I,C1), Lie, Lpt, Li, Lu, Ls, NewLie1, NewLpt1, NewLi1, NewLu1, NewLs1),

    % Application de la règle de l'intersection avec C2 (création d'un nouveau noeud avec C1 et C2)
    evolue((I,C2), NewLie1, NewLpt1, NewLi1, NewLu1, NewLs1, NewLie2, NewLpt2, NewLi2, NewLu2, NewLs2),

    % Affichage du résultat 
    affiche_evolution_Abox(Lie, Lpt, [(I,and(C1,C2)) | Li], Lu, Ls, Abr, NewLs2, NewLie2, NewLpt2, NewLi2, NewLu2, Abr),

    % Continuer la résolution (continuer l'algorithme sur le nouveau noeud crée qui contient maintenant C1 et C2)
    resolution(NewLie2, NewLpt2, NewLi2, NewLu2, NewLs2, Abr).

/*
╭────────────────────────────────────────────────╮
│   Application de la règle du quelque soit ∀    │
╰────────────────────────────────────────────────╯
*/
deduction_all(Lie, [(I, all(R,C)) | Lpt], Li, Lu, Ls, Abr) :-

    % Affichage :
    writef("Application de la regle \u2200 pour %w : %w", [I,R]),
    write(" \u2200 "),
    write_concept(C),nl,nl,

    % Récuperation des des instances "b" tel qu'il existe dans la liste Abr : ( I, b, R)
    (setof((B, C),  member((I, B, R), Abr), NomsInstances) -> 
    writef("Application de la regle \u2200 trouvée"),
    nl ;
    writef("pas de b tel que : "),
    write_Abi([(I1, all(R,C))])
    ),

    % Developpement pour toutes les instances b trouvée ( si jamais aucun b n'existe, alors evolue ne fait rien)
    evolue_liste(NomsInstances, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),

    % Affichage du résultat 
    affiche_evolution_Abox(Lie, [(I, all(R,C)) | Lpt], Li, Lu, Ls, Abr,Ls1, Lie1, Lpt1, Li1, Lu1, Abr),

    % Continuer la résolution (continuer l'algorithme sur le nouveau noeud crée)
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).


/*
╭─────────────────────────────────────────────╮
│   Application de la règle de l'union ⊔      │
╰─────────────────────────────────────────────╯
*/

transformation_or(Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls, Abr) :- 

    % Affichage de la régle a transformer
    writef("Application de la regle \u2294 pour %w :", [I]),
    write_concept(C1),
    write(" \u2294 "),
    write_concept(C2),nl,
    writef("Avec création du noeud contenant %w :", [I]),
    write_concept(C1),nl,nl,

    % Création d'un nouveau noeud
    % Ajout de la nouvelle insertion
    evolue((I, C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),

    % Affichage de la modification
    affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),

    % Continuer l'algorithme sur le noeud crée
    resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr)

    ;

    % On refait les même étapes pour le deuxieme noeud

    % Affichage de la régle a transformer
    writef("Application de la regle \u2294 pour %w :", [I]),
    write_concept(C1),
    write(" \u2294 "),
    write_concept(C2),nl,
    writef("Avec création du noeud contenant %w :", [I]),
    write_concept(C2),nl,nl,

    % Création du deuxieme noeud
    % Ajout de la nouvelle insertion
    evolue((I, C2),Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),

    % Affichage de la modification
    affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),

    % Continuer l'algorithme sur le noeud crée
    resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr).

/*
╭───────────────────────────────────╮
│   Insertion d'une assertion       │
╰───────────────────────────────────╯.
    On écrit la fonction : evolue(A, Lie, Lpt, Li, Lu, Ls, NewLie, NewLpt, NewLi, NewLu, NewLs)
    A représente une nouvelle assertion de concepts à intégrer dans l’une des listes 
    Lie, Lpt, Li, Lu ou Ls qui décrivent les assertions de concepts de la Abox étendue 
    et NewLie, NewLpt, NewLi, NewLu et NewLs représentent les nouvelles listes mies à jour. 

    Traitment du cas d'une nouvelle assertion : A n'existe pas dans la liste correspondante : 
    Si A n'existe pas déjà dans la liste qui lui correspond, on l'insére dans celle-ci.
*/
% Traitement des assertions du type (I,some(R,C))
evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, [(I,some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-
    \+ member((I, some(R,C)), Lie).

% Traitement des assertions du type (I,and(C1,C2))
evolue((I,and(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I,and(R,C)) | Li], Lu, Ls) :-
    \+ member((I, and(R,C)), Li).

% Traitement des assertions du type (I,all(R,C))
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I,all(R,C)) | Lpt], Li, Lu, Ls) :-
    \+ member((I, all(R,C)), Lpt).

% Traitement des assertions du type (I,or(C1,C2))
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I,or(C1,C2)) | Lu], Ls) :-
    \+ member((I, or(C1,C2)), Lu).


% Traitement des assertions du type (I,C) et du type (I,not(C))
evolue((I,C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I,C) | Ls]) :-
    cnamea(C),
    \+ member((I, (I,C)), Ls).

evolue((I,not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I,not(C)) | Ls]) :-
    cnamea(C),
    \+ member((I, (I,not(C))), Ls).

/*
    Traitment du cas d'une assertion existante: A n'existe déjà dans la liste correspondante : 
    Si A existe déjà dans la liste qui lui correspond, on ne l'ajoute pas pour éviter la redondance d'assertion.
*/

% Traitement des assertions du type (I,some(R,C))
evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, some(R,C)), Lie).

% Traitement des assertions du type (I,and(C1,C2))
evolue((I,and(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, and(R,C)), Li).

% Traitement des assertions du type (I,all(R,C))
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, all(R,C)), Lpt).

% Traitement des assertions du type (I,or(C1,C2))
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, or(C1,C2)), Lu).

% Traitement des assertions du type (I,C) et du type (I,not(C))
evolue((I,C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I, (I,C)), Ls).

evolue((I,not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I, (I,not(C))), Ls).

% Fonction de simplification pour faire evolue sur une liste ( utile pour for all, et aussi pour and )
evolue_liste([], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls).
evolue_liste([A|L], Lie, Lpt, Li, Lu, Ls, NewLie, NewLpt, NewLi, NewLu, NewLs) :-
    evolue(A, Lie, Lpt, Li, Lu, Ls, NewLie1, NewLpt1, NewLi1, NewLu1, NewLs1),
    evolue_liste(L, NewLie1, NewLpt1, NewLi1, NewLu1, NewLs1, NewLie, NewLpt, NewLi, NewLu, NewLs).


/*
╭──────────────────────────────────────────────────────────────╮
│   Affichage de l’évolution d’un état de la Abox étendue      │
╰──────────────────────────────────────────────────────────────╯
    Affiche l’évolution d’un état de la Abox étendue vers un état suivant
*/

affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2):-
	write("\033[4;36mEtat de départ :\033[0m"),nl,nl,
	write_Abi(Ls1),
	write_Abi(Lie1),
	write_Abi(Lpt1),
	write_Abi(Li1),
	write_Abi(Lu1),
	write_Abr(Abr1),
	nl,
	write("\033[4;36mEtat d'arrivée :\033[0m"),nl,nl,
	write_Abi(Ls2),
	write_Abi(Lie2),
	write_Abi(Lpt2),
	write_Abi(Li2),
	write_Abi(Lu2),
	write_Abr(Abr2),
	nl,
	(not_clash(Ls2)->print_colored("Pas de clash dans ce noeud",'32');
		print_colored("Clash dans ce noeud",'31')),nl,
	nl, write('============================='), nl, nl.



/*
╭───────────────────────────────╮
│          UTILS                │
├───────────────────────────────┤
│                               │
│       ╭───────────────────╮   │
│       │                   │   │
│       │     ┏━━━━━━━━━┓   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┃         ┃   │   │
│       │     ┗━━━━━━━━━┛   │   │
│       │                   │   │
│       ╰───────────────────╯   │
│                               │
╰───────────────────────────────╯
*/
% Prédicats pour transformation en NNF

nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1),
nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1),
nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)):- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),Y):- nnf(X,Y),!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

%----------------------------
%----------------------------
concatene([],L1,L1).
concatene([X|Y],L1,[X|L2]) :- concatene(Y,L1,L2).

% Prédicat pour afficher du texte en couleur
print_colored(Text, Color) :-
    atomic_list_concat(['\e[', Color, 'm', Text, '\e[0m'], ColoredText),
    write(ColoredText).

print_colorednl(Text, Color) :-
	atomic_list_concat(['\e[', Color, 'm', Text, '\e[0m'], ColoredText),
	writenl(ColoredText).

print_colorednl(Text, Liste, Color) :-
	atomic_list_concat(['\e[', Color, 'm', Text, '\e[0m'], ColoredText),
	writenl(ColoredText, Liste).

% transforme l'expression conceptuelle E en une expression EA où ne figurent plus que des identificateurs de concepts atomiques

% Si l'element est atomique on arrête
transAtom(E,E) :- cnamea(E). 

% On remplace E par son expression conceptuelle 
transAtom(E,EA) :- cnamena(E), equiv(E,DE), transAtom(DE,EA). % ici on vérifie si E est non atomique pour pouvoir récupérer son expression conceptuelle si c'est le cas.

% On developpe chaque membre en utilisant la récurssion pour arriver au élements atomiques ou non atomiques
transAtom(not(E),not(EA)) :- transAtom(E,EA). 
transAtom(and(E1,E2),and(EA1,EA2)) :- transAtom(E1,EA1), transAtom(E2,EA2).
transAtom(or(E1,E2),or(EA1,EA2)) :- transAtom(E1,EA1), transAtom(E2,EA2).
transAtom(some(R,E),some(R,EA)) :- transAtom(E,EA).
transAtom(any(R,E),any(R,EA)) :- transAtom(E,EA).

%----------------------------
%----------------------------
% génère un nouvel identificateur
compteur(1).
genere(Nom) :- 
    compteur(V),
    nombre(V,L1),
    concatene([105,110,115,116],L1,L2),
    V1 is V+1,
    dynamic(compteur/1),
    retract(compteur(V)),
    dynamic(compteur/1),
    assert(compteur(V1)),nl,nl,nl,
    name(Nom,L2).
nombre(0,[]).
nombre(X,L1) :-
    R is (X mod 10),
    Q is ((X-R)//10),
    chiffre_car(R,R1),
    char_code(R1,R2),
    nombre(Q,L),
    concatene(L,[R2],L1).
chiffre_car(0,'0').
chiffre_car(1,'1').
chiffre_car(2,'2').
chiffre_car(3,'3').
chiffre_car(4,'4').
chiffre_car(5,'5').
chiffre_car(6,'6').
chiffre_car(7,'7').
chiffre_car(8,'8').
chiffre_car(9,'9').

%----------------------------
%----------------------------
/*
╭───────────────────────────────╮
│ Fonctions d'affichage         │        
╰───────────────────────────────╯
*/
% Fonction write avec saut de ligne
writenl(Chaine) :- write(Chaine),nl.
writenl(Chaine,ListVar) :- writef(Chaine, ListVar),nl.

% Prédicats affichage des Boxs : 
writeBoxs(Tbox,Abc,Abr) :- 
	print_colorednl('Tbox :',34),
    writeBox(Tbox),nl,
    print_colorednl('Abox d\'insertion de concept :',34),
    writeBox(Abc),nl,
    print_colorednl('Abox d\'insertion de rôle :',34),
    writeBox(Abr),nl.

writeBox([]).

writeBox([(X, Y) | Rest]) :-
    write('('),
    write(X),
    write(', '),
    write(Y),
    writenl(')'),
    writeBox(Rest).

% Fonction affichage de concept
write_concept(C) :-
	cnamea(C),
	write(C).

write_concept(not(C)) :-
	write(' \u00AC '),
	write_concept(C).

write_concept(some(R, C)) :-
	write(' \u2203 '),
	write(R),
	write('.'),
	write_concept(C).

write_concept(all(R, C)) :-
	write(' \u2200 '),
	write(R),
	write('.'),
	write_concept(C).

write_concept(and(C1, C2)) :-
	write_concept(C1),
	write(' \u2A05 '),
	write_concept(C2).

write_concept(or(C1, C2)) :-
	write_concept(C1),
	write(' \u2A06 '),
	write_concept(C2).

% Fonction affichage de la Abox d'insertions
write_Abi([]).
write_Abi([(I, C) | L]):-
	write(I), write(' : '), write_concept(C),nl,
	write_Abi(L).

% Fonction affichage de la Abox de rôles
write_Abr([]).
write_Abr([(I1, I2, R) | L]) :-
	write('<'), write(I1), write(', '), write(I2), write('> : '),
	write(R),nl,
	write_Abr(L).