%---------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - Exercicio 1

% GRUPO 23
% Alexandre Martins  - A77523
% João Bernardo - A74814
% João Amorim  - A7408
% Tiago Gomes - Ae4560


%---------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).

%-----------------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op( 900, xfy,'::' ).
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic instituicao/3.
:- dynamic excecao/1.
:- dynamic nulo/1.
:- dynamic incerto/1.
:- dynamic impreciso/1.
:- dynamic imprecisoValues/1.
:- dynamic '-'/1.



%----------------------- Conhecimento Positivo ------------------------
% Extensao do predicado utente: #IdUt, Nome, Idade, Morada -> {V,F}

utente( 0, 'Elias',     28, 'Rua Jose Joaquim').
utente( 1, 'Sebastiao', 18, 'Rua Pinto Salgado').
utente( 2, 'Martim',    32, 'Rua da Luz').
utente( 3, 'Lucia',     55, 'Rua Vasconcelos').
utente( 4, 'Alexandre', 76, 'Rua da Feira').
utente( 5, 'Juliana',   22, 'Rua da Nazare').
utente( 6, 'Joao',      8,  'Rua Albertino Sousa').
utente( 7, 'Antonio',   16, 'Rua Estreita').
utente( 8, 'Isabela',   36, 'Rua Larga').
utente( 9, 'Artur',     51, 'Rua Comprida').
utente(10, 'Alice',     86, 'Rua Curta').


% Extensao do predicado prestador: #IdPrest, Nome, Especialidade, Instituicao -> {V, F}	

prestador( 0, 'Andre', 'Dermatologia', 2).
prestador( 1, 'Maria', 'Psiquiatria',  1).
prestador( 2, 'Josefina', 'Neurologia',   0).
prestador( 3, 'Jose', 'Ortopedia',    3).
prestador( 4, 'Helena', 'Podologia',    4).
prestador( 5, 'Bernardo', 'Cardiologia',  5).
prestador( 6, 'Tiago', 'Oftalmologia', 6).
prestador( 7, 'Nuno', 'Pediatria',    7).
prestador( 8, 'Vasco', 'Psicologia',   7).


% Extensao do predicado cuidado: Data, #IdUt, #IdPrest, Descricao, IdMed, Custo -'Consulta de' {V, F}	

cuidado(data(20, 2, 2019), 0,  4, 'Consulta de Podologia',    20).
cuidado(data(20, 2, 2019), 1,  0, 'Consulta de Dermatologia', 70).
cuidado(data(21, 3, 2019), 1,  7, 'Consulta de Pediatria',    35).
cuidado(data(21, 3, 2019), 2,  7, 'Consulta de Pediatria',    50).
cuidado(data(07, 4, 2019), 3,  5, 'Consulta de Cardiologia',  95).
cuidado(data(07, 4, 2019), 4,  3, 'Consulta de Ortopedia',    45).
cuidado(data(10, 4, 2019), 4,  6, 'Consulta de Oftalmologia', 25).
cuidado(data(10, 4, 2019), 5,  1, 'Consulta de Psiquiatria',  40).
cuidado(data(10, 4, 2019), 6,  2, 'Consulta de Neurologia',   50).
cuidado(data(13, 4, 2019), 6,  1, 'Consulta de Psiquiatria',  40).
cuidado(data(13, 4, 2019), 7,  6, 'Consulta de Oftalmologia', 25).
cuidado(data(19, 4, 2019), 8,  7, 'Consulta de Pediatria',    35).
cuidado(data(01, 5, 2019), 8,  0, 'Consulta de Dermatologia', 70).
cuidado(data(01, 5, 2019), 9,  3, 'Consulta de Ortopedia',    45).
cuidado(data(06, 6, 2019), 10, 2, 'Consulta de Neurologia',   50).
cuidado(data(12, 5, 2019), 10, 0, 'Consulta de Dermatologia', 70).
cuidado(data(12, 5, 2019), 10, 4, 'Consulta de Podologia',    20).
cuidado(data(13, 5, 2019), 2,  8, 'Consulta de Psicologia',   50).


% Extensao do predicado instituicao: #Id, Nome, Cidade -> {V, F}	

instituicao(0, 'Hospital de Braga',          'Braga').
instituicao(1, 'Hospital Sao Joao',          'Porto').
instituicao(2, 'Hospital de Santa Maria',    'Porto').
instituicao(3, 'Hospital de Santa Maria',    'Lisboa').
instituicao(4, 'Hospital Santa Maria Maior', 'Barcelos').
instituicao(5, 'Hospital Escala',            'Braga').
instituicao(6, 'Hospital de Faro',           'Faro').
instituicao(7, 'Hospital de Famalicao',      'Famalicao').


%----------------------- Conhecimento Negativo ------------------------

% Pressuposto do Mundo Fechado para utente.

-utente(Id, Nome, Idade, Morada) :-
    nao(utente(Id, Nome, Idade, Morada)),
    nao(excecao(utente(Id, Nome, Idade, Morada))).

% Negacoes fortes para utente.

-utente( 21, 'Diogo', 12, 'Rua Benjamim Salgado').
-utente( 22, 'Helena', 46, 'Rua Humberto Salgado').
-utente( 23, 'Jose', 45, 'Rua Antonio Salgado').

% Pressuposto do Mundo Fechado para prestador.

-prestador(Id, Nome, Esp, IdInst) :-
    nao(prestador(Id, Nome, Esp, IdInst)),
    nao(excecao(prestador(Id, Nome, Esp, IdInst))).

% Negacoes fortes para prestador.

-prestador( 21, 'Antonio', 'Dermatologia', 1).
-prestador( 22, 'Joao', 'Psiquiatria',  3).
-prestador( 23, 'Joaquim', 'Neurologia', 4).

% Pressuposto do Mundo Fechado para cuidado.

-cuidado(Data, IdUt, IdPrest, Descricao, Custo) :-
    nao(cuidado(Data, IdUt, IdPrest, Descricao, Custo)),
    nao(excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo))).

% Negacoes fortes para cuidado.

-cuidado(data(25, 12, 2019), 3,  5, 'Consulta de Podologia',    20).
-cuidado(data(26, 12, 2019), 6,  2, 'Consulta de Dermatologia', 70).
-cuidado(data(27, 12, 2019), 7,  1, 'Consulta de Pediatria',    35).

% Pressuposto do Mundo Fechado para instituicao.

-instituicao(Id, Nome, Cidade) :-
	nao(instituicao(Id, Nome, Cidade)),
	nao(excecao(instituicao(Id, Nome, Cidade))).

% Negacoes fortes para instituicao.

-instituicao(20, 'Hospital de Braga',          'Braga').
-instituicao(21, 'Hospital Sao Joao',          'Porto').
-instituicao(22, 'Hospital de Santa Maria',    'Porto').

%|------------------------ PREDICADOS AUXILIARES -------------------------|
%|------------------------------------------------------------------------|


% Extensão do Meta-Predicado demo: Questão, Resposta -> {V,F}

demo(Questao, verdadeiro) :- Questao.
demo(Questao, falso) :- -Questao.
demo(Questao, desconhecido) :- nao(Questao), nao(-Questao).


% Extensao do Meta-Predicado demo: Questao1, Questao2, Operacao, Resposta -> {V, F, D}
% e - conjuncao; ou - disjuncao; imp - implicacao; equiv - equivalencia; 

demo(Q1, Q2, OP, R) :- 
	OP == e,
	demo(Q1, R1), 
	demo(Q2, R2), 
	conj(R1, R2, R).

demo(Q1, Q2, OP, R) :- 
	OP == ou,
	demo(Q1, R1), 
	demo(Q2, R2), 
	disj(R1, R2, R).

demo(Q1, Q2, OP, R) :- 
	OP == imp,
	demo(Q1, R1), 
	demo(Q2, R2), 
	implic(R1, R2, R).

demo(Q1, Q2, OP, R) :- 
	OP == equiv,
	demo(Q1, R1), 
	demo(Q2, R2), 
	equiv(R1, R2, R).



% Extensao do Meta-Predicado demoLista: [Questao], [Resposta] -> {V,F,D}

demoLista( [],[] ).
demoLista( [A|H],[B|T] ) :- demo( A,B ),
                            demoLista( H,T ).  


% Extensao do predicado implic: Q1, Q2, Resultado -> {V, F, D}

implic(desconhecido, _, desconhecido).
implic(falso, verdadeiro, verdadeiro).
implic(falso, falso, verdadeiro).
implic(falso, desconhecido, verdadeiro).
implic(verdadeiro, desconhecido, desconhecido).
implic(verdadeiro, falso, falso).
implic(verdadeiro, verdadeiro, verdadeiro).


% Extensao do predicado equiv: Q1, Q2, Resultado -> {V, F, D}                

equiv(desconhecido, _, desconhecido).
equiv(_, desconhecido, desconhecido).
equiv(verdadeiro, verdadeiro, verdadeiro).
equiv(verdadeiro, falso, falso).
equiv(falso, verdadeiro, falso).
equiv(falso, falso, verdadeiro).


% Extensao do predicado conj: Q1, Q2, Resultado -> {V, F, D}

conj( verdadeiro, verdadeiro, verdadeiro ).
conj( verdadeiro, desconhecido, desconhecido ).
conj( desconhecido, verdadeiro, desconhecido ).
conj( desconhecido, desconhecido, desconhecido ).
conj( falso, _, falso ).
conj( _, falso, falso ).


% Extensao do predicado disjuncao: Q1, Q2, Resultado -> {V, F, D}

disj( verdadeiro, _, verdadeiro ).
disj( _, verdadeiro, verdadeiro ).
disj( falso, falso, falso ).
disj( falso, desconhecido, desconhecido ).
disj( desconhecido, falso, desconhecido ).
disj( desconhecido, desconhecido, desconhecido ).


% Extensao do predicado nao: Questao -> {V, F}	

nao(Questao) :- Questao, !, fail.
nao(Questao).


% Extensao do predicado solucoes: F, Q, S -> {V, F}	

solucoes(F, Q, S) :- findall(F, Q, S).


% Extensao do predicado comprimento: S, N -> {V, F}	

comprimento(S, N) :- length(S, N).


% Extensao do predicado teste: Lista -> {V, F}	

teste([]).
teste([X | L]) :- X, teste(L).

% Extensao do predicado testeImperfeito: Termo -> {V, F}
testConhecimento(T) :- demo(T, desconhecido).


% Extensao do predicado isImperf: T -> {V, F}	
% Indica se o conhecimento sobre o termo e imperfeito.

isImperf(utente(Id, _, _, _)) :- incerto(utente(Id)).
isImperf(utente(Id, _, _, _)) :- impreciso(utente(Id)).
isImperf(utente(Id, _, _, _)) :- imprecisoValues(utente(Id)).
isImperf(prestador(Id, _, _, _)) :- incerto(prestador(Id)).
isImperf(prestador(Id, _, _, _)) :- impreciso(prestador(Id)).
isImperf(prestador(Id, _, _, _)) :- imprecisoValues(prestador(Id)).
isImperf(cuidado(Data, IdUt, IdPrest, _, _)) :- incerto(cuidado(Data, IdUt, IdPrest)).
isImperf(cuidado(Data, IdUt, IdPrest, _, _)) :- impreciso(cuidado(Data, IdUt, IdPrest)).
isImperf(cuidado(Data, IdUt, IdPrest, _, _)) :- imprecisoValues(cuidado(Data, IdUt, IdPrest)).
isImperf(instituicao(Id, _, _)) :- incerto(instituicao(Id)).
isImperf(instituicao(Id, _, _)) :- impreciso(instituicao(Id)).
isImperf(instituicao(Id, _, _)) :- imprecisoValues(instituicao(Id)).


% Extensao do predicado isImperf: T -> {V, F}	
% Indica se o conhecimento sobre o termo e impreciso.

isImprec(utente(Id, _, _, _)) :- impreciso(utente(Id)).
isImprec(prestador(Id, _, _, _)) :- impreciso(prestador(Id)).
isImprec(instituicao(Id, _, _)) :- impreciso(instituicao(Id)).
isImprec(cuidado(Data, IdUt, IdPrest, _, _)) :- impreciso(cuidado(Data, IdUt, IdPrest)).



% ----------------- Remover Conhecimento Impreciso ------------------------

% Extensao do predicado delImprec: Utente -> {V, F}

delImprec(utente(Id, Nome, Idade, Morada), L) :-
	retract(excecao(utente(Id, Nome, Idade, Morada))),
	solucoes((utente(Id, N, I, M)), excecao(utente(Id, N, I, M)), L).
delImprec(utente(Id, Nome, Idade, Morada), L) :-
	solucoes((utente(Id, N, I, M)), excecao(utente(Id, N, I, M)), L).
	
	
% Extensao do predicado delImprec: Prestador -> {V, F}

delImprec(prestador(Id, Nome, Esp, IdInst), L) :-
	retract(excecao(prestador(Id, Nome, Esp, IdInst))),
	solucoes((prestador(Id, N, E, I)), excecao(prestador(Id, N, E, I)), L).
delImprec(prestador(Id, Nome, Esp, IdInst), L) :-
	solucoes((prestador(Id, N, E, I)), excecao(prestador(Id, N, E, I)), L).
	

% Extensao do predicado delImprec: Cuidado -> {V, F}

delImprec(cuidado(Data, IdUt, IdPrest, Desc, C), L) :-
	retract(excecao(cuidado(Data, IdUt, IdPrest, Desc, C))),
	solucoes((cuidado(Data, IdUt, IdPrest, D, C)), excecao(cuidado(Data, IdUt, IdPrest, D, C)), L).
delImprec(cuidado(Data, IdUt, IdPrest, Desc, C), L) :-
	solucoes((cuidado(Data, IdUt, IdPrest, D, C)), excecao(cuidado(Data, IdUt, IdPrest, D, C)), L).


% Extensao do predicado delImprec: Instituicao -> {V, F}

delImprec(instituicao(Id, Nome, Cidade), L) :-
	retract(excecao(instituicao(Id, Nome, Cidade))),
	solucoes((instituicao(Id, N, C)), excecao(instituicao(Id, N, C)), L).
delImprec(instituicao(Id, Nome, Cidade), L) :-
	solucoes((instituicao(Id, N, C)), excecao(instituicao(Id, N, C)), L).
	


% ----------------- Remover Conhecimento Incerto ------------------------	

% Extensao do predicado delIncerto: Utente -> {V, F}

delIncerto(utente(Id, Nome, Idade, Morada)) :-
	incerto(utente(Id)),
	retract(utente(Id, inc001, Idade, Morada)).
delIncerto(utente(Id, _, _, _)) :-
	impreciso(utente(Id)).
delIncerto(utente(Id, _, _, _)) :-
	imprecisoValues(utente(Id)).
	
	
% Extensao do predicado delIncerto: Prestador -> {V, F}

delIncerto(prestador(Id, Nome, Esp, IdInst)) :-
	incerto(prestador(Id)),
	retract(prestador(Id, Nome, inc001, IdInst)).
delIncerto(prestador(Id, _, _, _)) :-
	impreciso(prestador(Id)).
delIncerto(prestador(Id, _, _, _)) :-
	imprecisoValues(prestador(Id)).

	
% Extensao do predicado delIncerto: Cuidado -> {V, F}

delIncerto(cuidado(Data, IdUt, IdPrest, Desc, C)) :-
	incerto(cuidado(Data, IdUt, IdPrest)).
delIncerto(cuidado(Data, IdUt, IdPrest, _, _)) :-
	impreciso(cuidado(Data, IdUt, IdPrest)).
delIncerto(cuidado(Data, IdUt, IdPrest, _, _)) :-
	imprecisoValues(cuidado(Data, IdUt, IdPrest)).


% Extensao do predicado delIncerto: Instituicao -> {V, F}

delIncerto(instituicao(Id, Nome, Cidade)) :-
	incerto(instituicao(Id)),
	retract(instituicao(Id, inc01, Cidade)).
delIncerto(instituicao(Id, _, _)) :-
	impreciso(instituicao(Id)).
delIncerto(instituicao(Id, _, _)) :-
	imprecisoValues(instituicao(Id)).
	
	

% ----------------- Remover Conhecimento Imperfeito ------------------------

% Extensao do predicado delImperf: Utente -> {V, F}

delImperf(utente(Id, Nome, Idade, Morada)) :-
	incerto(utente(Id)),
	retract(incerto(utente(Id))).
delImperf(utente(Id, _, _, _)) :-
	imprecisoValues(utente(Id)),
	retract(imprecisoValues(utente(Id))),
	retract(excecao(utente(Id, _, _, _))).
delImperf(utente(Id, Nome, Idade, Morada)) :-
	impreciso(utente(Id)),
	retract(excecao(utente(Id, N, Idade, Morada))),
	delImperf(utente(Id, Nome, Idade, Morada)).
delImperf(utente(Id, _, _, _)) :-
	impreciso(utente(Id)),
	retract(impreciso(utente(Id))).
	
	
% Extensao do predicado delImperf: Prestador -> {V, F}
% Remove conhecimento imperfeito relativamente a um prestador
delImperf(prestador(Id, _, _, _)) :-
	incerto(prestador(Id)),
	retract(incerto(prestador(Id))).
delImperf(prestador(Id, _, _, _)) :-
	imprecisoValues(prestador(Id)),
	retract(imprecisoValues(prestador(Id))).
delImperf(prestador(Id, Nome, Esp, Ins)) :-
	impreciso(prestador(Id)),
	retract(excecao(prestador(Id, Nome, E, Ins))),
	delImperf(prestador(Id, Nome, Esp, Ins)).
delImperf(prestador(Id, _, _, _)) :-
	impreciso(prestador(Id)),
	retract(impreciso(prestador(Id))).
	

% Extensao do predicado delImperf: Cuidado -> {V, F}

delImperf(cuidado(Data, IdUt, IdPrest, _, _)) :-
	incerto(cuidado(Data, IdUt, IdPrest)),
	retract(incerto(cuidado(Data, IdUt, IdPrest))).
delImperf(cuidado(Data, IdUt, IdPrest, _, _)) :-
	imprecisoValues(cuidado(Data, IdUt, IdPrest)),
	retract(imprecisoValues(cuidado(Data, IdUt, IdPrest))).
delImperf(cuidado(Data, IdUt, IdPrest, Desc, Custo)) :-
	impreciso(cuidado(Data, IdUt, IdPrest)),
	retract(excecao(cuidado(Data, IdUt, IdPrest, Desc, C))),
	delImperf(cuidado(Data, IdUt, IdPrest)).
delImperf(cuidado(Data, IdUt, IdPrest, _, _)) :-
	impreciso(cuidado(Data, IdUt, IdPrest)),
	retract(impreciso(cuidado(Data, IdUt, IdPrest))).


% Extensao do predicado delImperf: Instituicao -> {V, F}

delImperf(instituicao(Id, Nome, Cidade)) :-
	incerto(instituicao(Id)),
	retract(incerto(instituicao(Id))).
delImperf(instituicao(Id, Nome, Cidade)) :-
	imprecisoValues(instituicao(Id)),
	retract(imprecisoValues(instituicao(Id))).
delImperf(instituicao(Id, Nome, Cidade)) :-
	impreciso(instituicao(Id)),
	retract(excecao(instituicao(Id, N, Cidade))),
	delImperf(instituicao(Id, Nome, Cidade)).
delImperf(instituicao(Id, Nome, Cidade)) :-
	impreciso(instituicao(Id)),
	retract(impreciso(instituicao(Id))).



%|------------------------ INVARIANTES ---------------------------|
%|----------------------------------------------------------------|


%---------------------------- Utente -----------------------------

% Invariante Estrutural que não permite a insercao de utentes repetidos (com o mesmo ID).
% Id e Idade inteiros.
% Idade entre 0 e 120.

+utente(Id, _, Idade, _) :: (
	integer(Id), integer(Idade), Idade >= 0, Idade =< 120,
	solucoes(Id, utente(Id, _, _, _), S),
    comprimento(S, N),
    N == 0
).


% Invariante que impede a insercao de conhecimento positivo contraditorio.
% Invariante que impede a insercao de conhecimento positivo que altere conhecimento interdito sobre
% o nome, a idade ou a morada de utente.

+utente(Id, Nome, Idade, Morada) :: (
	solucoes(Id, -utente(Id, Nome, Idade, Morada), S1),
	solucoes(N, (utente(Id, N, Idade, Morada), nulo(N)), S2),
	solucoes(I, (utente(Id, Nome, I, Morada), nulo(I)), S3),
	solucoes(M, (utente(Id, Nome, Idade, M), nulo(M)), S4),
	comprimento(S1,	N1),
	comprimento(S2, N2),
	comprimento(S3, N3),
	comprimento(S4, N4),
    N1 >= 0,
    N1 =< 1,
    N2 == 0,
	N3 == 0,
	N4 == 0
).


% Invariante que impede a insercao de conhecimento negativo contraditorio
% Invariante que impede a insercao de conhecimento negativo que ja exista
% Invariante que impede a insercao de conhecimento negativo que altere conhecimento interdito sobre
% o nome, a idade ou a morada de utente.

+(-utente(Id, Nome, Idade, Morada)) :: (
	solucoes(Id, utente(Id, Nome, Idade, Morada), S1),
	solucoes(Id, -utente(Id, Nome, Idade, Morada), S2),
	solucoes(N, (utente(Id, N, Idade, Morada), nulo(N)), S3),
	solucoes(I, (utente(Id, Nome, I, Morada), nulo(I)), S4),
	solucoes(M, (utente(Id, Nome, Idade, M), nulo(M)), S5),
	comprimento(S1, N1),
	comprimento(S2, N2),
	comprimento(S3, N3),
	comprimento(S4, N4),
	comprimento(S5, N5),
	N1 == 0,
	N2 >= 0,
	N2 =< 1,
	N3 == 0,
	N4 == 0,
	N5 == 0
).


% Invariante Referencial que nao permite a remocao de um utente enquanto existirem cuidados
% associadas ao mesmo
-utente(IdUt, _, _, _) :: (
	solucoes(IdUt, cuidado(_,IdUt, _, _, _), S1),
	comprimento(S1, N1),
	N1 == 0
).



%---------------------------- Prestador -----------------------------

% Invariante Estrutural que nao permite a insercao de prestadores que ja existam (com o mesmo ID).
% Nao permitir a insercao de prestadores cuja instituicao nao exista.
% Id deve ser inteiro

+prestador(Id, Nome, Esp, IdInst) :: (
	integer(Id), integer(IdInst), 
    solucoes(Id, prestador(Id, _, _, _), S1),
    solucoes(IdInst, instituicao(IdInst, _, _), S2),
    comprimento(S1, N1),
	comprimento(S2, N2),
    N1 == 0,
	N2 == 1
).


% Invariante que impede a insercao de conhecimento positivo contraditorio.
% Invariante que impede a insercao de conhecimento positivo que altere conhecimento interdito sobre
% o nome, a especialidade ou a instituicao de prestador.

+prestador(Id, Nome, Esp, IdInst) :: (
	solucoes(Id, -prestador(Id, Nome, Esp, IdInst), S1),
	solucoes(N, (prestador(Id, N, Esp, IdInst), nulo(N)), S2),
	solucoes(E, (prestador(Id, Nome, E, IdInst), nulo(E)), S3),
	solucoes(I, (prestador(Id, Nome, Esp, I), nulo(I)), S4),
	comprimento(S1, N1),
	comprimento(S2, N2),
	comprimento(S3, N3),
	comprimento(S4, N4),
	N1 >= 0, 
	N1 =< 1,
	N2 == 0,
	N3 == 0,
	N4 == 0

).


% Invariante que impede a insercao de conhecimento negativo contraditorio.
% Invariante que impede a insercao de conhecimento negativo que ja exista.
% Invariante que impede a insercao de conhecimento negativo que altere conhecimento interdito sobre
% o nome, a especialidade ou a instituicao de prestador.

+(-prestador(Id, Nome, Esp, IdIns)) :: (
	solucoes(Id, prestador(Id, Nome, Esp, IdIns), S1),
	solucoes(Id, -prestador(Id, Nome, Esp, IdIns), S2),
	solucoes(N, (prestador(Id, N, Esp, IdInst), nulo(N)), S3),
	solucoes(E, (prestador(Id, Nome, E, IdInst), nulo(E)), S4),
	solucoes(I, (prestador(Id, Nome, Esp, I), nulo(I)), S5),
	comprimento(S1, N1),
	comprimento(S2, N2),
	comprimento(S3, N3),
	comprimento(S4, N4),
	comprimento(S5, N5),
	N1 == 0,
	N2 >= 0, 
	N2 =< 1,
	N3 == 0,
	N4 == 0,
	N5 == 0
).


% Invariante Referencial que nao permite a remocao de servico enquanto existirem cuidados
% associados ao mesmo

-prestador(Id, _, _, _) :: (
	solucoes(Id, cuidado(_, _, Id, _, _), S1),
	comprimento(S1, N1),
	N1 == 0
).


%---------------------------- Cuidado -----------------------------


% Invariante que impede a insercao de um cuidado ja existente ou cujo 
% utente ou prestador nao existam.
% IdUt e IdPrest inteiros; Custo number e maior que 0.

+cuidado(Data, IdUt, IdPrest, Desc, Custo) :: (
	integer(IdUt), integer(IdPrest),
	number(Custo), Custo >= 0, 
	solucoes(IdUt, utente(IdUt, _, _, _), S1),
	solucoes(IdPrest, prestador(IdPrest, _, _, _), S2),
	solucoes((Data, IdUt, IdPrest, Desc, Custo),
             cuidado(Data, IdUt, IdPrest, Desc, Custo), S3),
	comprimento(S1, N1),
	comprimento(S2, N2),
    comprimento(S3, N3),
	N1 == 1, 
	N2 == 1,
	N3 == 0

).


% Invariante que impede a insercao de conhecimento positivo contraditorio.
% Invariante que impede a insercao de conhecimento positivo que altere conhecimento interdito sobre
% o custo do cuidado

+cuidado(Data, IdUt, IdPrest, Desc, Custo) :: (
	solucoes((Data, IdUt, IdPrest, Desc, Custo),
		     -cuidado(Data, IdUt, IdPrest, Desc, Custo), S1),
	solucoes(C, (cuidado(Data, IdUt, IdPrest, Desc, C), nulo(C)), S2),
	comprimento(S1, N1),
	comprimento(S2, N2),
	N1 >= 0,
	N1 =< 1,
	N2 == 0
).


% Invariante que impede a insercao de conhecimento negativo contraditorio.
% Invariante que impede a insercao de conhecimento negativo que ja exista.
% Invariante que impede a insercao de conhecimento negativo que altere conhecimento interdito sobre
% o custo de um cuidado.

+(-cuidado(Data, IdUt, IdPrest, Desc, Custo)) :: (
	solucoes((Data, IdUt, IdPrest, Desc, Custo),
             cuidado(Data, IdUt, IdPrest, Desc, Custo), S1),
	solucoes((Data, IdUt, IdPrest, Desc, Custo),
              -cuidado(Data, IdUt, IdPrest, Desc, Custo), S2),
	solucoes(C, (cuidado(Data, IdUt, IdPrest, Desc, C), nulo(C)), S3),
	comprimento(S1, N1),
    comprimento(S2, N2),
    comprimento(S3, N3),
	N1 == 1,
	N2 >= 0,
	N2 =< 1,
	N3 == 0
).


% Invariante que remove um cuidado se este existir.

-cuidado(Data, IdUt, IdPrest, Desc, Custo) :: (
	solucoes((Data, IdUt, IdPrest, Desc, Custo), 
	consulta(Data, IdServ, IdPrest, Desc, Custo), S1),
	comprimento(S1, N1),
	N1 = 1
).


%---------------------------- Instituicao -----------------------------


% Invariante Estrutural que nao permite a insercao de instituicoes repetidas (com o mesmo ID)
% Id inteiro

+instituicao(IdInst, _, _) :: (
	integer(IdInst), 
	solucoes(IdInst, instituicao(IdInst, _, _), S),
    comprimento(S, N),
    N == 0
).


% Invariante que impede a insercao de conhecimento positivo contraditorio.
% Invariante que impede a insercao de conhecimento positivo que altere conhecimento interdito sobre
% o nome ou a ci

+instituicao(IdInst, Nome, Cidade) :: (
	solucoes(Id, -instituicao(Id, Nome, Cidade), S1),
	solucoes(N, (instituicao(IdInst, N, Cidade), nulo(N)), S2),
	solucoes(Cidade, (instituicao(IdInst, Nome, C), nulo(C)), S3),
	comprimento(S1, N1),
	comprimento(S2, N2),
	comprimento(S3, N3),
	N1 >= 0,
	N1 =< 1,
	N2 == 0,
	N3 == 0
).


% Invariante que impede a insercao de conhecimento negativo contraditorio.
% Invariante que impede a insercao de conhecimento negativo que ja exista.
% Invariante que impede a insercao de conhecimento negativo que altere conhecimento interdito sobre
% o nome ou a cidade de uma instituicao.

+(-instituicao(IdInst, Nome, Cidade)) :: (
	solucoes(Id, instituicao(Id, Nome, Cidade), S1),
	solucoes(Id, -instituicao(Id, Nome, Cidade), S2),
	solucoes(N, (instituicao(IdInst, N, Cidade), nulo(N)), S3),
	solucoes(Cidade, (instituicao(IdInst, Nome, C), nulo(C)), S4),
	comprimento(S1, N1),
	comprimento(S2, N2),
	comprimento(S3, N3),
	comprimento(S4, N4),
	N1 == 0,
	N2 >= 0,
	N2 =< 1,
	N3 == 0,
	N4 == 0
).


% Invariante Referencial que nao permite a remocao de uma instituicao se existirem 
% prestadores associados a mesma

-instituicao(IdInst, _, _) :: (
	solucoes(IdInst, prestador(_, _, _, IdInst), S1),
	comprimento(S1, N1),
	N1 == 0
).



%|-------------------- EVOLUÇÃO E INVOLUÇÃO DO SISTEMA -----------------------|
%|----------------------------------------------------------------------------|

% ---------------------------- Evolucao -------------------------------

% Extensao do predicado evolucao: Termo, Flag -> {V, F}
% Insercao de conhecimento positivo sem existencia de conhecimento imperfeito.

evolucao(T) :- 
	nao(isImperf(T)),
	solucoes(I, +T::I, Li),
    teste(Li),
	assert(T).
					

% Extensao do predicado evolucao: Termo, Flag -> {V, F}
% Insercao de conhecimento positivo existindo conhecimento imperfeito.

evolucao(T) :- 
	isImperf(T),
	testConhecimento(T),
	delIncerto(T),
	solucoes(I, +T::I, Li),
	teste(Li),
	assert(T),
	delImperf(T).


% Extensao do predicado evolucao: Termo, Flag -> {V, F}
% Insercao de conhecimento negativo sem existencia de conhecimento imperfeito.

evolucao(-Te) :- 
	nao(impreciso(Te)),
	solucoes(I, +(-Te)::I, Li),
	teste(Li),
    assert(-Te).


% Extensao do predicado evolucao: Termo, Flag -> {V, F}
% Insercao de conhecimento negativo com existencia de conhecimento imperfeito.

evolucao(-Te) :- 
	isImprec(Te),
	delImprec(Te, Lista),
	comprimento(Lista, S),
	S > 1,
	solucoes(I, +(-Te)::I, Li),
	teste(Li),
    assert(-Te).



% Extensao do predicado evolucao: Termo, Flag -> {V, F}
% Insercao de conhecimento negativo com existencia de conhecimento imperfeito.	

evolucao(-Te) :- 
	isImprec(Te),
	delImprec(Te, [H|[]]),
	delImperf(Te),
    assert(H).



% ---------------------------- Involucao -------------------------------

% Extensao do predicado involucao: Termo -> {V, F}		
			   
involucao(Te) :- 
	Te,
	solucoes(I, -Te::I, Li),
	teste(Li),
    retract(Te).
					
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado involucao: -Termo -> {V, F}
% -Termo representa a negacao forte de um termo		

involucao(-Te) :-
    -Te,
	solucoes(I, -(-Te::Inv), Li),
	teste(Li),
    retract(-Te).
		

%|------------------ TRATAMENTO DO CONHECIMENTO IMPERFEITO -------------------|
%|----------------------------------------------------------------------------|

%------------------ Conhecimento Imperfeito Interdito ------------------

% Nao e possivel saber qual a idade do utente com id 14

utente(14, 'Azevedo', int001, 'Rua dos Padres').

excecao(utente(Id, Nome, Idade, Morada)) :-
	utente(Id, Nome, int001, Morada).
nulo(int001).

% Nao e possivel saber qual a descricao do cuidado realizado em 30 de 
% Junho de 2019

cuidado(data(30, 06, 2019), 5, 7, int002, 70).   

excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo)) :-
	cuidado(Data, IdUt, IdPrest, int002, Custo).                        
nulo(int002).             



%------------------ Conhecimento Imperfeito Impreciso ------------------

% Nao se sabe se o utente 12 se chama Albertino ou Alberto.

excecao(utente(12, 'Albertino', 10, 'Rua do Ceu')).
excecao(utente(12, 'Alberto', 10, 'Rua do Ceu')).	

impreciso(utente(12)).


% Nao se sabe se o utente 13 se chama Pedro ou Telmo, desconhecendo-se
% tambem a idade, sabendo-se apenas que esta entre os 45 e os 50 inclusive.

excecao(utente(13, 'Telmo', Idade, 'Rua dos Padres')) :-
	Idade >= 45,
	Idade =< 50.
excecao(utente(13, 'Telmo', Idade, 'Rua dos Padres')) :-
	Idade >= 45,
	Idade =< 50.

imprecisoValues(utente(13)).


% Nao se sabe se o prestador com id 12 tem como especialidade

excecao(prestador(10, 'Luis', 'Podologia', 5)).
excecao(prestador(10, 'Luis', 'Ortopedia', 5)).
excecao(prestador(10, 'Luis', 'Dermatologia', 5)).

impreciso(prestador(10)).

% Não se sabe o custo da consulta que ocorreu no dia 25 de Junho de 2019,
% sabendo-se apenas que está entre os 25 e os 50 euros.

excecao(cuidado(data(25,06,2019), 2, 3, 'Consulta de Ortopedia', C)) :- C>=25, C=<50.

imprecisoValues(cuidado(data(25, 06, 2019), 2, 3)).

% Nao se sabe se a instituicao com id 9 se chama Hospital da Trofa ou 
% Hospital da Treta

excecao(instituicao(9, 'Hospital da Trofa', 'Trofa')).
excecao(instituicao(9, 'Hospital da Treta', 'Trofa')).

impreciso(instituicao(9)).



%------------------ Conhecimento Imperfeito Incerto ------------------

% Não se sabe o nome do utente com id 11.

utente(11, inc001, 24, 'Rua Curta').

excecao(utente(Id, Nome, Idade, Morada)) :- 
	utente(Id, inc001, Idade, Morada).

incerto(utente(11)).


% Não se sabe a especialidade do prestador com id 9.

prestador( 9, 'Joaquina', inc001, 5).

excecao(prestador(Id, Nome, Especialidade, IdInst)) :-
	prestador(Id, Nome, inc001, IdInst).

incerto(prestador(9)).


% Não se sabe a cidade da instituicao com id 9.

instituicao(8, 'Hospital da Amadora', inc001).

excecao(instituicao(Id, Nome, Cidade)) :-
	instituicao(Id, Nome, inc001).

incerto(instituicao(8)).















