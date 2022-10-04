# Resource management

To handle concurrency in the evaluation of tasks, the system provides a resource system. 
We define a resource as an object with an initial capacity $C_{init}$.
A resource $r$ can be acquired at time $t$ with an amount $c$ that is lower than or equal to the current capacity $C_t$.
Upon acquisition, the acting engine ensures that no race-condition occurs that would result in an over allocation and the capacity is immediately decreased by the amount $c$.

We distinguish *unary* and *divisible* resources.
A *unary* resource can be acquired by only one task at a time, where initial capacity and requested amount are always one.
A *divisible* resource with an initial capacity $C_{init}$ can be acquired with any $c_t \in[0, C_{init}]$.
At the difference of real-time systems and mutexes, there is no guarantee on the order of access to resources, as it defers this decision to the acting engine and reasoning systems.
When a resource is released, its capacity is increased by corresponding amount.





- `new-resource` declares a resource *r* of the resource, with an initial capacity *c* for a *divisible* resource.


- `acquire` acquires a resource *r* with the quantity $c$ needed if $r$ is *divisible*.
Once the acquisition has been validated by the system, the function returns a *resource-handle* $h$. If $h$ goes out of scope, the resource is automatically released.
The acquisition of a resource can be interrupted to avoid blocking a program waiting too long on a resource.


- `release` explicitly releases the resource using the *resource-handle* $h$.