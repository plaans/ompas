---
description: Simpler enum of LValue for data manipulation
---

# LValueS

Sometimes, we only need to manipulate pure datas, without pointers or other structures. To do so, we can use LValueS, an other enum that can be built from an LValue (dropping some information on pointers).&#x20;

We have the following transformation:

* LValue::Symbol => LValueS::Symbol
* LValue::Number(LNumber::Int) => LValueS::Int
* LValue::Number(LNumber::Float) => LValueS::Float
* LValue::Number(LNumber::Usize) => LValueS::Int
* LValue::Fn => LValueS::Symbol
* LValue::MutFn => LValueS::Symbol
* LValue::Lambda => _<mark style="color:red;">error</mark>_
* LValue::CoreOperator => LValueS::Symbol
* LValue::Map => LValueS::Map
* LValue::List => LValueS::List
* LValue::True => LValue::Bool
* LValue::Nil => LValue::Bool
* LValue::String => LValue::Symbol
* LValue::Character => LValue::Symbol
* LValue::AsyncFn => LValue::Symbol
* LValue::AsyncMutFn => LValue::Symbol
* LValue::Future => _<mark style="color:red;">error</mark>_

The reverse transformation is without data dropping though.
