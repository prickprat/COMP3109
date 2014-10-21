
% Prologs House of Windsor
%
% Male members of House of Windsor
male(andrew).
male(charles).
male(edward).
male(harry).
male(mark).
male(peter).
male(philip).
male(severn).
male(timothy).
male(william).

% Female members of House of Windsor
female(anne).
female(beatrice).
female(diana).
female(elizabeth_II).
female(eugenie).
female(louise).
female(sophie).
female(sarah).
female(zara).

% Children of Elizabeth II & Philip
child_of(charles,elizabeth_II).
child_of(charles,philip).
child_of(anne,elizabeth_II).
child_of(anne,philip).
child_of(andrew,elizabeth_II).
child_of(andrew,philip).
child_of(edward,elizabeth_II).
child_of(edward,philip).

% Children of Diana & Charles
child_of(william,charles).
child_of(william,diana).
child_of(harry,charles).
child_of(harry,diana).

% Children of Anne & Mark
child_of(zara,anne).
child_of(zara,mark).
child_of(peter,anne).
child_of(peter,mark).

% Children of Sarah & Andrew
child_of(beatrice,sarah).
child_of(beatrice,andrew).
child_of(eugenie,sarah).
child_of(eugenie,andrew).

% Children of Sophie & Edward
child_of(louise,sophie).
child_of(louise,edward).
child_of(severn,sophie).
child_of(severn,edward).

% Married
married(philip,elizabeth_II).
married(charles,diana).
married(charles,camilla).
married(mark,anne).
married(timothy,anne).
married(andrew,sarah).
married(edward,sophie).


% Exe 1 )
mother_of(Person, Child) :- female(Person), child_of(Child, Person).	

% Exe 2
mother(Person) :- female(Person), child_of(_, Person).

% Exe 3
sibling(Person1, Person2) :- child_of(Person1, X), child_of(Person2, X).

% Exe 4 - doesnt capture divorced women
divorced(Person) :- (married(Person, X)), (married(Person, Y)), X \= Y.
divorced(Person) :- (married(X, Person)), married(Y, Person), X \= Y.