Rapport de la partie 1 du projet de compilation, Mathis Bouverot-Dupuis

Pour examiner les arbres de syntaxe intermédiaires produits par le compilateur : utiliser l'option --verbose qui affiche très joliment les arbres (si --parse-only ou --type-only est fourni).

Explication rapide de la structure du code :
- pjuliac.ml : point d'entrée du compilateur, parse les arguments de la ligne de commande et appelle le lexer, le parser puis le type-checker
- lexer.mll : analyseur lexical, écrit avec ocamllex
- parser.ml : analyseur syntaxique, écrit à la main (analyseur descendant)
- pos_ast.ml : arbres de syntaxe produits par le parser et consommés par le type-checker
- type_checking : typage statique et allocation des variables (sur le tas/globales etc.)
- type_ast.ml : arbres de syntaxe produits par le typage
- context.ml : environnement contenant les variables/fonctions/structures du programme, utilisé pendant le typage

Difficultés et éléments manquants :
- écrire un parser avec menhir était très compliqué : meme si le code est plus long, j'ai l'impression d'avoir beaucoup plus de controle avec un analyseur écrit à la main qu'avec menhir, avec lequel certains aspects de la syntaxe étaient très embetants à implémenter
- les appels de fonctions ambigus (plusieurs surcharges d'une meme fonction sont compatibles avec les arguments fournis, mais on est sur de ne pas pouvoir résoudre la bonne fonction à l'exécution) ne sont pas traités. Je n'ai pas (encore) trouvé comment détecter ces cas.
- les constantes entières ne sont pas (encore) parfaitement implémentées : par exemple le compilateur ne sais pas faire la différence entre "1 -9223372036854775808" (à refuser) et "if 1 -9223372036854775808 end" (à accepter) 
