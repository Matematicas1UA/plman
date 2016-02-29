%
% This file is part of Pl-man
% Pl-man is a puzzle game inspired in the popular game pacman, and it is mainly aimed
% to teach programming in PROLOG and introductory courses of Artifial Intelligence.
%
% Copyright (C) 2007-2014 Francisco Gallego <ronaldo@cheesetea.com>
% Departamento de Ciencia de la Computación e Inteligencia Artificial
% Universidad de Alicante
% Campus de San Vicente
% Ap.Correos, 99 
% 03080, San Vicente del Raspeig (Alicante)
%
%   This program is free software: you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation, either version 3 of the License, or
%   (at your option) any later version.
%
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
%
%   You should have received a copy of the GNU General Public License
%   along with this program.  If not, see <http://www.gnu.org/licenses/>.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cauldron
%
% Implements a cauldron which lets users make potions.
% Potions are combinations of ingredients in a given
% order, called recipes. Recipes and their effects are
% customizable.
%
% Initialization and Recipe Customization
%------------------------------------------
%  cauldron(init, OID, L_PARAMS):-
%    OID:  Identifier of the cauldron object
%    L_PARAMS: Params to control the way the cauldron behaves.
%   
%  cauldron(newRecipe(OID_C, SHA1, REPEAT, ACTIONLIST))
%    OID_C: Identifier of the cauldron where the recipe will work.
%           Use a free variable to make the recipe work on all cauldrons.
%    SHA1:  Encripted version of the recipe. Use cauldron(hashRecipe/2) to
%           get this value for the recipes you want.
%    REPEAT:Number of times the actions will be triggered when the recipe
%           is complete (ingredients have been added). -1 for infinity.
%    ACTIONLIST: Ordered list of actions that will be performed when the 
%           recipe is complete.
%
% Example 1
%--------------------
%   createGameEntity(OID_C, 'U', object, 1, 1, active, cauldron, 
%               [name(cauldron), solid(true), static(true), 
%               appearance(attribs(normal, white, default)),
%               description('Magic Cauldron')]),
%   cauldron(init, OID_C, []), ...
%
% Example 2
%--------------------
%   cauldron(create(OID_C, 1, 1, [])),
%
%   cauldron(newRecipe(OID_C, '5de244f7178eba27da6149d6e6d4e7e5d1d64360', -1, 
%                      [ doRecipeActionsYouWant(OID_C) ])),
%   cauldron(newRecipe(OID_C, '2d3cde6bc5fcfe6f05393fa5b13751fe7dc994ce', 1, 
%                      [ sendGreetings, destroyWorld ])).
%
% Creates a cauldron that will accept 2 recipes. The first recipe
% will be triggered when ingredient list [ing2] is added to the cauldron
% (the SHA1 for this list is the first of the two SHAs). It will
% trigger doRecipeActionsYouWant(OID_C) forever, until cauldron state 
% gets changed. The 2nd recipe will be triggered by the ingredient list 
% [ing2, ing1]; it will be triggered only once, and will sendGreetings
% and destroyWorld.
%
% In order to know the SHA1 of a ingredient list, you should call
% cauldron(hashRecipe(RECIPE, SHA1)), where RECIPE is the ordered
% list of names of ingredients.
%
% To add ingredients to the cauldron, you should drop them on top
% of the cauldron.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(cauldron, [cauldron/1]).
:- dynamic d_cauldronStatus/2.
:- dynamic d_recipe/5.

%%%
%%% Creation and initialization of new cauldrons
%%%
cauldron(create(OID, X, Y, L_PARAMS)):-
    'pl-man':createGameEntity(OID, 'U', object, X, Y, active, cauldron, 
        [name(cauldron), solid(true), static(true), appearance(attribs(normal, white, default)),
         description('Cauldron')]),
    cauldron(init(OID, L_PARAMS)), !.
cauldron(create(OID, X, Y, L_PARAMS)):-
    append([ 'OID: ', OID, ' XY:', X, ',', Y, ' PARAMS:' ], L_PARAMS, PRINT),
    'pl-man':showLangMessage(cauldron, bad_creation, PRINT), !.

%%%
%%% Initialization
%%%
% init a cauldron (if it already exists, it is overwritten)
cauldron(init(OID, L_PARAMS)):-
    integer(OID),
    'pl-man':dynamicProperties(checkPropertiesList(L_PARAMS)),
    cauldron(destroyAll(OID)),
    'pl-man':dynamicProperties(create(PID, L_PARAMS)),
    assert(d_cauldronStatus(OID, PID)), 
    'pl-man':dynamicProperties(add(PID, theRecipe([]))), !.
cauldron(init(OID, L_PARAMS)):-
    append([ 'OID: ', OID, ' PARAMS:' ], L_PARAMS, PRINT),
    'pl-man':showLangMessage(cauldron, bad_parameters, PRINT), !.

%%%
%%% Destroy
%%%
% Destroy all cauldrons of the same OID
cauldron(destroyAll(OID)):-
    forall( d_cauldronStatus(OID, _), 
            cauldron(destroy(OID)) ).
cauldron(destroy(OID)):-
    retract(d_cauldronStatus(OID, PID)),
    'pl-man':dynamicProperties(destroy(PID)).
cauldron(destroy(OID)):-
    'pl-man':showLangMessage(cauldron, bad_destroy, [ 'OID: ', OID ]), !.

%%%
%%% Create a Recipe
%%% 
cauldron(newRecipe(OID, HASH, REPEAT, ACTIONLIST)):-
    % Check parameters
    (integer(OID) ; var(OID)),
    integer(HASH), % atom(HASH), atom_length(HASH, 40), %%%%% FIX FOR BAD VARIANT_SHA1/2
    integer(REPEAT), is_list(ACTIONLIST), 
    forall(member(M, ACTIONLIST), callable(M)),
    % Create recipe
    assert(d_recipe(wait, OID, HASH, REPEAT, ACTIONLIST)), !.
cauldron(newRecipe(OID, HASH, REPEAT, ACTIONLIST)):-
    append([ 'OID:', OID, ' HASH:', HASH, ' REPEAT:', REPEAT, ' ACTIONLIST:' ], ACTIONLIST, PRINT),
    'pl-man':showLangMessage(cauldron, bad_recipe_params, PRINT), !.

%%%
%%% Add new ingredients to the cauldron
%%% 

% If it finds new ingredients in the location of the cauldron, it adds them
cauldron(addNewIngredients(OID, X, Y)) :-
    'pl-man':entityType(PacID, pacman),
    'pl-man':getAllEntitiesFrom(L_ENT, X, Y, [OID, PacID]),
    forall( member(M, L_ENT), 
            cauldron(addNewIngredient(OID, M)) ).

% Adds a new ingredient (A entity that drops inside the cauldron)
cauldron(addNewIngredient(OID, ING_ID)):-
    'pl-man':entityName(ING_ID, NAME),
    d_cauldronStatus(OID, PLID),
    'pl-man':dynamicProperties(get(PLID, theRecipe(RECIPE))),
    append(RECIPE, [ NAME ], NEWRECIPE),
    'pl-man':dynamicProperties(set(PLID, theRecipe(NEWRECIPE))),
    'pl-man':showLangMessage(cauldron, ingredient_added, [ '(P', PLID, ':', NAME, ')' ]),
    'pl-man':destroyGameEntity(ING_ID), !, 
    cauldron(triggerValidRecipes(OID)).
cauldron(addNewIngredient(OID, ING_ID)):-
    'pl-man':showLangMessage(cauldron, problem_adding_ingredient, [ 'OID: ', OID, ' ING: ', ING_ID ]), !.

%%%
%%% Validate and trigger recipes
%%% 
% Succeds with a valid recipe for a concrete Cauldron (by menas of its Properties List ID)
cauldron(validRecipe(PLID, SHA1)):-
    'pl-man':dynamicProperties(get(PLID, theRecipe(RECIPE))),
    cauldron(hashRecipe(RECIPE, SHA1)).

% If any recipe is valid recipe, and trigger actions
cauldron(triggerValidRecipes(OID)):-
    d_cauldronStatus(OID, PLID),
    cauldron(validRecipe(PLID, SHA1)),
    d_recipe(wait, OID, SHA1, REPEAT, ACTIONS), !,
    'pl-man':showLangMessage(cauldron, potion_complete, [ '(P', PLID, ')' ]),
    cauldron(triggerRecipe(OID, SHA1, REPEAT, ACTIONS)).
cauldron(triggerValidRecipes(_)) :- !.

% Trigger a concrete recipe
cauldron(triggerRecipe(_, _, 0, _)):- !.
cauldron(triggerRecipe(OID, SHA1, REPEAT, ACTIONS)):-
    retract(d_recipe(_, OID, SHA1, REPEAT, ACTIONS)), !,
    'pl-man':showLangMessage(cauldron, potion_working, [ '(', REPEAT, ')' ]),
    forall( member(ACT, ACTIONS), ignore(ACT) ),
    R is REPEAT - 1,
    assert(d_recipe(triggered, OID, SHA1, R, ACTIONS)), !.

% Trigger again a previously trigged recipe, or finish it if no longer valid
cauldron(retriggerRecipes(OID)):-
    d_recipe(triggered, OID, HASH, REPEAT, ACTIONLIST),
    d_cauldronStatus(OID, PLID),
    cauldron(retriggerRecipe(OID, PLID, HASH, REPEAT, ACTIONLIST)).
cauldron(retriggerRecipes(_)):-!.
cauldron(retriggerRecipe(OID, PLID, SHA1, REPEAT, ACTIONLIST)) :-
    cauldron(validRecipe(PLID, SHA1)), !,
    cauldron(triggerRecipe(OID, SHA1, REPEAT, ACTIONLIST)).
cauldron(retriggerRecipe(OID, _, SHA1, REPEAT, ACTIONLIST)) :-
    retract(d_recipe(_, OID, SHA1, REPEAT, ACTIONLIST)), !,
    assert(d_recipe(wait, OID, SHA1, REPEAT, ACTIONLIST)).

%%%
%%% Hash a list of ingredients into SHA1
%%% 
% Hash the list of ingredients into a sha1 identifier for the recipe
cauldron(hashRecipe(RECIPE, SHA1)):-
    length(RECIPE, LEN), LENM is LEN + 1,
    term_hash(RECIPE, LENM, 2147483647, SHA1), !.
%%%%% FIX FOR BAD VARIANT_SHA1/2
%    term_hash(RECIPE, LENM, 2147483647, TH),
%    variant_sha1(TH, SHA1), !.          

%%%
%%% Control Rule
%%%

% Check for triggered potions, to continue triggering
cauldron(OID):-
    integer(OID), 
    cauldron(retriggerRecipes(OID)),
    d_cauldronStatus(OID, _), !,
    'pl-man':entityLocation(OID, X, Y, _),
    cauldron(addNewIngredients(OID, X, Y)).
cauldron(OID):-
    integer(OID),
    'pl-man':showLangMessage(cauldron, not_initialized, [ 'OID: ', OID ]), !.

