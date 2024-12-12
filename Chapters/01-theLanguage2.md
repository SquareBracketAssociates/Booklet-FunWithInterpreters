## The Language: ObjVlisp

We are going to write a programming language interpreter. And this raises the questions: what language are we going to interpret?
A language that is too complex would take ages to implement, the book would be too long.
Think of implementing any programming language you know: how many features does it have?
Complex languages have a lot of accidental complexity (think C, C++, Java).

In this book we have made a choice: ObjVlisp.
ObjVLisp is small enough to be implementable in a couple of afternoons.
And it is large enough to be interesting, as we will show you next.


### ObjVlisp: a language kernel

ObjVlisp is an object-oriented programming language originally published in 1986 when the foundation of object-oriented programming was still emerging.
Actually, ObjVlisp is not a typical programming language, but a language kernel.
ObjVlisp is not a programming language because it does not have an associated syntax: we will attach one to it later, don't worry.
ObjVlisp is a language kernel because it defines the core concepts of the programming language, and their associated semantics.


#### A bit of History:
ObjVlisp was inspired from the kernel of Smalltalk-78.
The IBM SOM-DSOM kernel is similar to ObjVLisp while implemented in C++.
ObjVlisp is a subset of the reflective kernel of CLOS \(Common Lisp Object System\) since CLOS reifies instance variables, generic functions, and method combination.

#### Why ObjVlisp?

ObjVlisp has the following properties that make it interesting to study and learn:
- It unifies the concepts of class and instance \(there is only one data structure to represent all objects, classes included\),
- It is composed of only two classes `Class` and `Object` \(it relies on existing elements such as booleans, arrays, and string of the underlying implementation language\),
- It raises the question of meta-circular infinite regressions \(a class is an instance of another class that is an instance of yet another class, etc.\) and how to resolve it,
- It requires consideration of allocation, class and object initialization, message passing as well as the bootstrap process,
- It can be implemented in less than 30 methods in Pharo.

Another important point: this kernel is self-described. This does not mean that we are going to *write* ObjVlisp in itself (we cannot because ObjVLisp does not have a syntax). What this means is that some of its concepts are recursive: for example, classes are objects in ObjVlisp, which means that classes are instances of classes. See? We will start by explaining some aspects, but since everything is linked, you may have to read the chapter twice to fully get it.

### ObjVLisp's six postulates

The original ObjVlisp kernel is defined by six postulates. Some of them look a bit dated by modern standards, and the 6th postulate is simply wrong as we will explain later \(a solution is simple to design and implement\).
Here are the six postulates as stated in the original paper for the sake of historical perspective.

1. An object represents a piece of knowledge and a set of capabilities.
2. The only protocol to activate an object is message passing: a message specifies which procedure to apply \(denoted by its name, the selector\) and its arguments.
3. Every object belongs to a class that specifies its data \(attributes called fields\) and its behavior \(procedures called methods\). Objects will be dynamically generated from this model; they are called instances of the class. Following Plato, all instances of a class have same structure and shape, but differ through the values of their common instance variables.
4. A class is also an object, instantiated by another class, called its metaclass. Consequently \(P3\), to each class is associated a metaclass which describes its behavior as an object. The initial primitive metaclass is the class Class, built as its own instance.
5. A class can be defined as a subclass of one \(or many\) other class\(es\). This subclassing mechanism allows sharing of instance variables and methods, and is called inheritance. The class Object represents the most common behavior shared by all objects.
6. If the instance variables owned by an object define a local environment, there are also class variables defining a global environment shared by all the instances of a same class. These class variables are defined at the metaclass level according to the following equation: class variable \[an-object\] = instance variable \[an-object’s class\].


### ObjVLisp Model Overview

Contrary to a real uniform language kernel, ObjVlisp does not consider arrays, booleans, strings, numbers or any other elementary objects as part of the kernel as this is the case in a real bootstrap such as the one of Pharo. ObjVLisp's kernel focuses on understanding the core relationships between classes and objects.

Figure *@fig:ObjVlisp@* shows the two core classes of the kernel:
- `Object` is the root of the inheritance graph and is an instance of `Class`.
- `Class` is the first class and root of the instantiation tree and instance of itself as we will see later.


![The ObjVlisp kernel: a minimal class-based kernel.](figures/ObjVlispMore.pdf width=60&label=fig:ObjVlisp)

Now imagine we wanted to implement a class `Workstation` (which means *a really old computer*, you can look it up online ;)).
Figure *@withSing@* shows how that could take place in this abstract model.
The class `Workstation` is an instance of the class `Class` since it is a class.
It inherits from `Object`, meaning that it has the default behavior that objects exhibit.

Moreover, we can do something really cool in ObjVlisp: we can change how classes work using *metaclasses*.
The class `WithSingleton` is an instance of the class `Class` (as any other class) but differently from our `Workstation` it inherits from `Class`.
This means that `WithSingleton` will *behave* like a class!
This means that this class we just created is not just a normal but a metaclass: its instances are classes.
Metaclasses change the behavior of classes. Could you guess by the name how the instances of `WithSingleton` are supposed to behave?

Finally, suppose we implement a class named `SpecialWorkstation` as an instance of the class `WithSingleton` and inheriting from `Workstation`.
This structure illustrates the different roles of instantiation and inheritance.
Our `SpecialWorkstation` will behave as a class `WithSingleton`, and it's instances will behave as defined by `SpecialWorkstation` and its superclass `Workstation`.

 ![The kernel with specialized metaclasses.](figures/ObjVlispSingleton.pdf width=70&label=withSing)

The two diagrams *@fig:ObjVlisp@* and *@withSing@* will be explained step by step throughout this chapter.

![Understanding metaclasses using message passing.](figures/ObjVlispSingleton2.pdf width=90&label=fig:kernel2)

**Note:** We will see later that understanding such an architecture is important to understand message passing and how methods get executed.
Message passing always looks up methods in the class of the receiver of the message and then follows the inheritance chain \(See Figure *@fig:kernel2@*\) thus following first the instantiation link, then the inheritance link!
Figure *@fig:kernel2@* illustrates two main cases:
- When we send a message to `BigMac` or `Minna`, the corresponding method is looked up in their corresponding classes `Workstation` or `SpecialWorkstation` and follows the inheritance link up to `Object`.
- When we send a messsage to the classes `Workstation` or `SpecialWorkstation`, the corresponding method is looked up in their class, the class `Class` and up to `Object`.


### Modelling Instances and Classes

In this kernel, classes and objects are linked by the instantiation link, as shown by Figure  *@fig:Instantiation@*:
% +Simple instances.>file://figures/Ref-Instances.png|width=50|label=fig:Instances+
- Terminal instances are obviously objects: a workstation named `mac1` is an instance of the class `Workstation`, a point `10@20` is instance of the class `Point`.
- Classes are also objects \(instances\) of other classes: the class `Workstation` is an instance of the class `Class`, the class `Point` is an instance of the class `Class`.

**Things to think about:** What could be the class of the class `Class` and why?


![Chain of instantiation: classes are objects, too.](figures/Ref-InstantiationLink.pdf width=72&label=fig:Instantiation)

A class defines an ordered sequence of instance variables definitions.
Each variable definition is just the name of the variable.
All instances of a class share the same variable definitions.
However, each instance will have its own specific value for each variable definition.
For example, Figure *@fig:Ref-Instances@* shows that instances of `Workstation` have two values: a name and a next node.

In our diagrams, we represent terminal instances as rounded rectangles.
Inside each rectangle, there is a list of the values of its instance variables.
Since classes are objects, _when we want to stress that classes are objects_ we will later use a different graphical convention.


![Instances of `Workstation` have two values: their names and their next node.](figures/Ref-Instances.pdf width=60&label=fig:Ref-Instances)

Notice also that an object has a reference to its class.
As we will see when we discuss inheritance later on, every object possesses an instance variable class \(inherited from `Object`\) that references to its class.
We will not add that extra instance variable in the diagrams, because it is redundant with the arrow.

**Things to think about:**

- How is this model different so far from languages such as Pharo and Java? And what about Python and Javascript? Do these languages have classes too?
- What about the instance variables? How are they declared? Are they fixed or can be dynamically added? What about types?
- A bit more complex: Are classes objects too in those languages? Do they have metaclasses?


### A Brief Introduction to Messages and Methods

So far we have defined the structural part of our language kernel: the entities that make part of it and the relationships between themselves.
But let's remember that we define a programming language so we can automate computations with it: we want to execute some code!

This kernel presents a single operation to perform computation: message passing.
Message passing is the act of one object (the sender) to send a message to another object (the receiver).
When an object receives a message, it must search for a method to execute, a mechanism we will call *method lookup*.
In ObjVlisp, the *method lookup* will be as in most traditional object-oriented programming languages: when an object receives a message, it will search for the method in the class hierarchy starting from its class.

The ObjVlisp kernel represents how methods are stored and looked up as follows.
Methods belong to a class and are stored into a dictionary that associates method names \(the selectors\) with the method bodies containing the code to execute.
Since methods are stored in a class, the method dictionary should be described in the metaclass. Therefore, the method dictionary of a class is the _value_ of the instance variable `methodDict` defined on the metaclass `Class`. Each class will have its own method dictionary.

ObjVlisp does not specify how methods are represented, we will choose and attach a representation in the following chapters.

### Conclusion

We presented a small kernel composed of two classes: `Object`, the root of the inheritance tree and `Class`, the first metaclass root of the instantiation tree. 
We briefly revisited the ideas behind message passing.
In the next chapter we propose to you how to implement such a kernel.

#### Further readings


The kernel presented in this chapter is a kernel with explicit metaclasses and as such it is not a panacea. Indeed it results in problems with metaclass composition as explained in Bouraqadi et al.'s excellent article or  .
