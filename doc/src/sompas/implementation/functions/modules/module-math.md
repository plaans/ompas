---
description: Module that provide advanced mathematical functions.
---

# Module math

### **Functions**


* **sin** : computes the sin of a number
```lisp
>> (sin 0)
LI>> 0
>> (sin pi)
LI>> 0.00000000000000012246467991473532
>> (sin (/ pi 2))
LI>> 1
```
* **cos** : computes the cos of a number
```lisp
>> (cos 0)
LI>> 1
>> (cos pi)
LI>> -1
>> (cos (/ pi 2))
LI>> 0.00000000000000006123233995736766
```
* **pow** : computes a number to a certain power
```lisp
>> (pow 2 3)
LI>> 8
>> (pow 2 3.5)
LI>> 11.313708498984761
>> (pow 2.3 3.5)
LI>> 18.45216910555504
```
* **square** : computes a number to the square.
```lisp
>> (square 2)
LI>> 4
>> (square 2.3)
LI>> 5.289999999999999
```
* **abs** : computes the absolute of a number
```lisp
>> (abs 2)
LI>> 2
>> (abs -2)
LI>> 2
```
* **rand-int-in-range** : generates a random int in a range.
```lisp
>> (rand-int-in-range 0 10)
LI>> 6
```
* **rand-float-in-range**: generates a random float in a range.

```lisp
>> (rand-float-in-range 0 10)
LI>> 1.875527591323538
```

### **Constants**
* **pi**: value of constant _pi_ in _f64_