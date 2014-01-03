-module(ejst).
-export([getById/1, getById/2, getValueById/1, getValueById/2]).
-export([setHTMLById/2, setHTMLById/3]).
-export([setValueById/2, setValueById/3]).
-export([getHTMLById/1, getHTMLById/2]).
-export([getDisplayById/1, setDisplayById/2]).

-spec getValueById(op:ejs()) -> op:ejs().
-spec getValueById(op:ejs(), op:ejs()) -> op:ejs().
getValueById(Name) ->
	getValueById(Name, "document").
getValueById(Name, Domain) ->
	{[getById(Name, Domain), "value"]}.

-spec setValueById(op:ejs(), op:ejs()) -> op:ejs().
-spec setValueById(op:ejs(), op:ejs(), op:ejs()) -> op:ejs().
setValueById(Name, Value) -> setValueById(Name, "document", Value).
setValueById(Name, Domain, Value) ->
	{'=', {[{[Domain, "getElementById"], [Name]}, "value"]}, Value}.

-spec getById(op:ejs()) -> op:ejs().
-spec getById(op:ejs(), op:ejs()) -> op:ejs().
getById(Name) -> getById(Name, "document").
getById(Name, Domain) ->
	{[Domain, "getElementById"], [Name]}.

-spec getHTMLById(op:ejs()) -> op:ejs().
-spec getHTMLById(op:ejs(), op:ejs()) -> op:ejs().
getHTMLById(Name) -> getHTMLById(Name, "document").
getHTMLById(Name, Domain) ->
	{[{[Domain, "getElementById"], [Name]}, "innerHTML"]}.

-spec setHTMLById(op:ejs(), op:ejs()) -> op:ejs().
-spec setHTMLById(op:ejs(), op:ejs(), op:ejs()) -> op:ejs().
setHTMLById(Name, Value) -> setHTMLById(Name, "document", Value).
setHTMLById(Name, Domain, Value) ->
	{'=', {[{[Domain, "getElementById"], [Name]}, "innerHTML"]}, Value}.

-spec getDisplayById(op:ejs()) -> op:ejs().
getDisplayById(ID) ->
	{[ejst:getById({quotes, ID}), style, display]}.

-spec setDisplayById(op:ejs(), op:ejs()) -> op:ejs().
setDisplayById(ID, Style) ->
	{'=', {[ejst:getById({quotes, ID}), style, display]}, {quotes, Style}}.
