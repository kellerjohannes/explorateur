# Conceptual thoughts

## The 'essence' of a 'composition': staff notation or sound (design)?

What is the essence of a composition involving dynamic wind? In the
context of a practical application, we can also ask: What is the
center piece of a software setup? There are two extreme cases. First,
the essence could be the notation. Musical thoughts are conceived,
developed and expressed within the realm of a specific
notation. Practically, notation software (in our case most probably
Dorico) would be the starting point, from which the main sequencing is
controlled. In the other extreme case, the sounding reality of the
organ (or any body of sound) is considered the essence. It is
developed while interacting with the real instrument, or with a
realistic simulation of it (samplers or physical modelling). Any form
of notation is therefore a secondary problem that is solved after the
core of the composition has been established.


## Concept

At the heart of the software setup there is a program written in
Common Lisp. It receives data from various controllers (MIDI keyboards
and other MIDI devices, OSC messages from custom OSC devices and
interfaces) and can be interacted with by the organist via a graphical
user interface (mouse or touch) or a purely text-based terminal. The
organist can create various layers of abstractions in order to control
the wind pressure of each smARTvalve: virtual _instruments_ that offer
structured access to specific constellations of parameters, such as
_organ stops_, spatial arrangements, multiphonics, finely controlled
detuning, etc. The software can also be used to sequence or automate
any parameter in a fully deterministic way (the state of the entire
_machine_ is known at any point in time, and fully reproducible).

The program can output control data that is directly sent to the Decap
modules (for now, this is MIDI, in the future there might be options
for OSC or custom data formats). On top of that, the program can also
send MIDI, OSC or serial data to other software or hardware. It can
therefore be used to develop musical material off-site using software
that simulates organ pipes or other instruments, as well as
controlling physical instruments such as Yves Rechsteiner's
Explorateur, or any other digitally controllable musical instrument.

## Implementation

We plan to develop the program in Common Lisp. For the musical
functionality we use INCUDINE, mainly for it's implementation of a
highly precise scheduling mechanism and the implementation of MIDI and
OSC. For the graphical user interface, we use CLOG, mainly because it
allows a strict separation between the performance-sensitive
real-time-computing and the rendering of the interface. The GUI is
essentially a web page that is rendered in a web browser and updated
through WebSockets in real time. Therefore, the GUI is entirely
optional. Since the software offers the GUI through a web server, it
can be accessed remotely, with any number of web browsers. In a local
network, this offers the option to interact with the program from
various points: a main computer, a tablet (exposing certain parameters
on a touch sensitive interface) and a mobile phone (used to debug or
tune individual pipes).


# Collaboration Johannes / Mauricio

## The language / the notation

Any kind of meaningful interaction with our system is defined by
_commands_. All these commands form the _domain specific language_
that we define. The language grows along the musical and practical
challenges. It can easily be extended or modified, potentially even
temporarily during a musical piece.

Our DSL is seamlessly integrated into Common Lisp, therefore it uses
the syntax of Common Lisp. Any functionality of Common Lisp is also
available in our DSL.

A fundamental understanding of the Common Lisp syntax is therefore
required in order to interact with our system.

### Common Lisp Syntax in a nutshell

Our system is a bit like a pocket calculator: you type in an
_expression_ and the system will display a _result_. Behind the
scenes, the system _evaluates_ the expression, which is essentially a
transformation process. The outcome of the transformation can be a
simplification (example: you type in `5+4+2` and the calculator
replies `11`) or an expansion (example: you ask our system to display
all currently playing organ pipes, and you will get a potentially long
list of pipe numbers), or any kind of action (example: you ask the
system to switch on a specific organ pipe. The system might not reply
anything, but it will _do_ something by sending out a MIDI message).

All the _expressions_ you type in have to be well structured. They
need to follow very strict rules, otherwise the system wouldn't know
how to process (to _evaluate_) it. Our language consists of atomic
things (you might comapare them to letters or sillables of actual
languages) and combination of these atomic things into more complex
structures (maybe comparable to word, sentences, paragraphs, chapters,
books).

Atoms are unchangeable. If you type in just one atom, the system will
_evaluate_ it without applying any transformation. The response will
consist of just the same atom. Numbers are atoms. If you type in `3`,
the system will respond with `3`. There are various different kinds of
numbers the system recognizes: integers (`3`, `-5`, `0`), ratios
(`3/2`, `81/80`, `-4/3`; ratios always need to be typed with a slash
(`/`), never with a colon (`3:2` is not a ratio in the eyes of our
system. The system will simplify a ratio though, `3/1` will be treated
as `3`, that's not considered an actual transformation, since `3/1` is
considered _equal_ to `3`), and decimals (`3.141`, `1.4975`). There
are many more types of numbers and notations of numbers, which we are
going to ignore for now.

Strings are also atoms: they are sequences of any Unicode symbol,
always enclosed in double quotes ("Hi there", "E♭ is sharper than
D♯."). A string is considered unchangeable, the system will not
transform it in any way unless you explicitely operate on it.

Now to the combinations of atoms. It's illegal to just type in two
atoms in succession (`5 3` is considered a syntactic error and will
result in either an error message or in undefined behaviour). To group
atoms into a sequence of atoms, you have to enclose them in
paretheses. Typing in such a sequence triggeres a specific behaviour:
the first atom is assumed to be the name of an operation that will be
applied to all the following atoms within the sequence. There are many
predefined operations, one of the is `+`. Typing in `(+ 5 3 2)` will
be read by the system like this: look up the definition of `+` and
send the atoms `5`, `3` and `2` to the _function_ that was found under
the name `+`. The result of the application of this function will be
the response to the user.

There are hundreds of these _functions_ available, `+` is a very
obvious and conceptually simple one. Of course there are much more
complex functions, such as sorting data alphabetically, or lookup data
in multidimensional nested structures. On top of that, it's very easy
to define your own functions.

It's possible to created nested sequences of atoms. For example `(+ 5
(* 3 2))`. When evaluating this expression, the system will try to
apply the function `+` to the atom `5` and the expression `(* 3 2)`.
In order to provide `+` with a usable atom (`+` needs to be fed
numbers, otherwise it wouldn't know what to do), the system needs to
evaluate `(* 3 2)` first. So it applies `*` (multiplication) to `3`
and `2`, and sends the result of this evaluation (`6`) to `+`, along
with the atom `5`. The result of the evaluation of the entire original
expression is therefore `11`.

This is essentially all there is to know about the Lisp
syntax. Everything is structured with parentheses only. A Common Lisp
program is a collection of _expressions_, some of them with just a
couple of nested sequences of atoms, some of them hilariously complex
and funny looking.

Of course there is much more to learn about the language before you
can become productive in any way, but this fundamental syntax will
always remain the same.








## Questions Mauricio

These questions were asked in the early stage of conception of the
custom software tools.


### How can the software interact with hardware?

INCUDINE offers implementations of various interaction protocols, such
as MIDI, OSC, Websockets. Therefore, connecting existing hardware such
as the _Osmose_ controller, the MIDI ring, or TouchOSC interfaces is a
straight forward possibility. How the software handles incoming data
is entirely up to us, a possible approach would be to define _callback
functions_, which are commands that are triggered for specific
incoming MIDI events. These commands can interact with any part of the
software, triggering processes of any degree of complexity.


### How could the software interact with notation?

The main commercial notation applications are out of reach, except
thought musicXML export. Lilypond could be used to transform
automatically generated code into staff notation. Both approaches
(generating musicXML and Lilypond code) are technically easily
possible, but conceptually equally challenging. It would probably be
productive to solve the conceptual challenge within the software,
using a custom abstract representation of the _score_, and then
implement various _backends_ that convert the internal representation
of the score into various output formats, such as XML or Lilypond.

At this point, it seems impossible to predict whether the
transformation of complex algorithmic sequencing data into linear
staff notation is possible in an automated manner, or needs to be done
by hand. In the latter case, it would probably be more efficient to
_transcribe_ the acoustic reality of a piece in an entirely detached
step, after the piece has been developed.
