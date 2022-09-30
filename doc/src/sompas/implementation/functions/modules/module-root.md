---
description: Basic module of the lisp. Loaded in all scheme environments.
---

# Module root

### Functions to get informations on the environment

* **env::get\_keys**: returns the list of all elements defined in the environment
* **env::get\_macros :** returns the list of macros
* **env::get\_macro :** return the expression of a given macro

### Get and set functions

* **get**: function to get an element from a list of a map (also exists get-list and get-map)
```lisp
>> (get (list 1 2 3) 1)
LI>> 2
>> (get (map '((ten 10) (twenty 20))) twenty)
LI>> 20
```

* **set**: function to set an element in a list or a map
```lisp
>> (set (list 1 2 3) 1 1)
LI>> 2
>> (set (map '((ten 10) (twenty 20))) '(twenty twenty))
LI>> twenty: twenty
ten: 10
```
### Functions on list

* **list**: constructs a list.
```lisp
>>(list 1 2 3)
LI>>(1 2 3)
```
* **car**: returns the first element of a list, nil if the list is empty.

```lisp
>>(car '(3 4 5))
LI>> 3
```

* **cdr**: returns list without the first element, nil if the list has one element.

```lisp
>>(cdr '(3 4 5))
LI>>(4 5)
```

* **first:** returns the first element of a list (same as **car**)

```lisp
>>(first (list 1 2 3))
LI>> 1
```

* **second:** returns the second element of a list
```lisp
>>(second (list 1 2 3))
LI>> 2
```
* **third:** returns the third element of a list
```lisp
>>(second (list 1 2 3))
LI>> 3
```
* **rest**: returns the rest of a list (same as cdr)
```lisp
>> (rest '(1 2 3))
LI>> (2 3)
```
* **last**: returns the last element of a list.
```lisp
>>(last '(3 4 5))
LI>>5
```
* **cons**: creates a cons of two elements

```lisp
>> (cons 3 nil)
LI>> (3)
>> (cons nil 3)
LI>> (nil 3)
>> (cons 4 '(3))
LI>> (4 3)
>> (cons '(4) '(3))
LI>> ((4) 3)
```
* **len**: returns the length of a list of a map
```lisp
>>(len '(1 2 3))
LI>> 3
```
* **empty**: returns true is a list or a map is empty
```lisp
>> (empty '())
LI>> true
```
* **get-list** : get the element of the list at the given index (starts at 0). The first argument is a list, and the second the index
```lisp
>> (get-list (list 1 2 3) 1)
LI>> 2
```
* **set-list** : set the element of the list at the given index (starts at 0).
The first argument is a list, the second the value, and the third the index.
```lisp
>> (set (list 1 2 3) 1 1)
LI>> 2
```
* **append** : append two lists together
```lisp
>> (append (list 1 2 3) '(4 5))
LI>> (1 2 3 4 5)
```
* **member** : It takes two arguments of which the second must be a list,
if the first argument is a member of the second argument, and then it returns the remainder of the list beginning with the first argument.
```lisp
>> (member 3 '(1 2 3 4 5))
LI>> (3 4 5)
```
* **reverse** : reverse a list
```lisp
>> (reverse '(1 2 3 4 5))
LI>> (5 4 3 2 1)
```
### Functions on map

* **map**: constructs a map.
```lisp
>> (map '((ten 10) (twenty 20)))
LI>> twenty: 20
ten: 10
```
* **get-map**: get the value inside a map

```lisp
>> (define m1 (map '((test . 10))))
>> (get m1)
LI>> test: 10
>> (get-map m1 test)
LI>> 10
```
* **set-map**: returns a map with the new value inserted.
```lisp
>> (define m1 (set-map m1 '(twenty . 20)))
>> (get m1)
LI>> twenty: 20
test: 10
```

* **union-map** : unifies two maps
```lisp
>> (define map1 (map '((ten 10) (twenty 20))))
LI>> nil
>> map1
LI>> ten: 10
twenty: 20

>> (define map2 (map '((thirty 30) (fourty 40))))
LI>> nil
>> map2
LI>> thirty: 30
fourty: 40

>> (define map3 (union-map map1 map2))
LI>> nil
>> map3
LI>> thirty: 30
twenty: 20
fourty: 40
ten: 10
```
* **remove-map** : remove from the map an entry. The first argument is a map, and the second the key.
```lisp
>> (define map1 (map '((ten 10) (twenty 20))))
LI>> nil
>> (define map2 (map '((thirty 30) (fourty 40))))
LI>> nil
>> (define map3 (union-map map1 map2))
LI>> nil
>> (define map4 (remove-map map3 twenty))
LI>> nil
>> map4
LI>> thirty: 30
fourty: 40
ten: 10
```
* **remove-key-value-map** : remove a key-value binding if it exists.
```lisp
>> (define map1 (map '((ten 10) (twenty 20))))
LI>> nil
>> (define map2 (remove-key-value-map map1 '(twenty 20)))
LI>> nil
>> map2
LI>> ten: 10
>> (define map2 (remove-key-value-map map1 '(ten 1)))
LI>> error: In remove-key-value-map, map does not have key value (ten:1)
```

### Mathematical functions

* basic math functions:
  * not (!)
  * add (+)
  * sub (-)
  * div (/)
  * mul (\*)
  * eq (=)
  * gt (>)
  * ge (>=)
  * lt (<)
  * le (<=)

### Type verification
  * **lambda?** : returns true if the LValue is a lambda.
  * **integer?**: returns true if the LValue is an integer.
  * **float?**: returns true if the LValue is a float.
  * **nil?**: returns true if the LValue is nil.
  * **bool?**: returns true if the LValue is a boolean.
  * **symbol?**: returns true if the LValue is a symbol.
  * **fn?**: returns true if the LValue is a function.
  * **mut-fn?**: returns true if the LValue is a mutable function.
  * **quote?**: returns true if the LValue is a quote.
  * **map?**: returns true if the LValue is a map.
  * **list?**: returns true if the LValue is a list.
  * **pair?**: returns true if the LValue is a list with at least one element.

All aboves functions are defined in the _root_ environment.
