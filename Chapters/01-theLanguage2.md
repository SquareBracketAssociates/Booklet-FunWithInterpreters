## Interpreting Pharo

We are going to write a programming language interpreter. This raises the question of the language we are going to interpret.
A language that is too complex would take ages to implement, the book would be too long.
Think of implementing any programming language you know: how many features does it have?
Complex languages have a lot of accidental complexity (think C, C++, Java).

In this book, we have made a choice: Pharo itself. 
Pharo is small enough to be implementable in a couple of afternoons.
And it is large enough to be interesting, as we will show you next.

After an introduction to abstract syntax trees and visitors (Chapters *@cha:ast@*, *@cha:visitorReminder@*, and *@cha:visitorast@*) this book 
define interpreters of increasing level of power:

- Chapter *@cha:interpreterLiteral@* defines an interpreter of basic literal objects such as integers, floats, booleans, and literal arrays. It sets a little infrastructure to support test definition.
- Chapter *@cha:self@* extends the previous interpreter with support for variables self and super.
- Chapter *@cha:scope@* extends the previous interpreter to support instance variables accesses. It introduces the concept of scopes. This concept will be further extended to support method execution. 
- Chapter *@cha:callingInfra@* introduces the infrastructure to support sending messages: a stack as well as a method scope. 
- Chapter *@cha:messageArgs@* extends the previous one to introduce support for parameters and temporaries.


### Conventions

In this book we follow the convention that results are prefixed with `>>>`.
Hence the execution of `1 + 2` yields the value `3`.

```
1 + 2 
>>> 3
```

### Todo

Write in chapter  *@cha:callingInfra@* why we use methodScope as a stack frame element
Why we unify scope and frame