# Haskell introduction

In this assignment, you will pick up the basics of Haskell. This will help you
to understand and write the harder assignments later in the course.

## Getting started

### GHCup

Install GHCup by following the [installation instructions](https://www.haskell.org/ghcup/) 
for your operating system. Do make sure to allow GHCup to hook into stack,
as we will be using stack to build the assignments. To check if the 
installation went succesful, the following command should give a version 
number.

```sh
$ ghcup --version
```

Make sure this is the latest version of GHCup. If this is not the case, you
can upgrade to the latest version via

```sh
$ ghcup upgrade
```

GHCup manages all your Haskell related software. For this course, we will be
working with the package manager Stack. To install this, you can simply run:

```sh
$ ghcup install stack
```

We also recommend that you install the Haskell Language Server (HLS). If your
code editor supports language servers (which most of them do), this will 
display errors or useful tips to improve your code! It also allows you to run
some additional commands when editing, for example, jumping to a definition of
a piece of code. You can install it via the following command.

```sh
$ ghcup install hls
```

For an overview of all the software installed on your system by GHCup, run the
following command. With this, you can make sure that both Stack and HLS are on
their latest versions.

```sh
$ ghcup tui
```

It should be the case that stack will automatically install the GHC version 
specified by the project when trying to compile it. If this doesn't work you 
could also manually install specific GHC versions, either through this menu, 
or through the command line.

### Git

Install Git by following the [installation instructions](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
for your operating system.

You may clone this assignment through git. Make sure to run this command
in the directory where you would like this project to exist. You can find
the `<directory-path>` in the top right of this repository, under `Code`.

```sh
$ git clone <directory-path>
```

Git is also the recommended way to cooperate with your teammates during the
later assignments. Do make sure to keep your repositories private in this case!
Note that this is an individual assignment.

## Running and testing

You can build and test the code through stack. Make sure to be within the
project folder before you run stack. One of the most useful commands throughout
the course will be:

```sh
$ stack test
```

This will run the entire test infrastructure on your code. If all went well up
to this point, this will give you a list of functions that failed their tests.

There are many other ways to build and/or run your code. One recommended way 
would be to use GHCi. This is a Read-Eval-Print-Loop (REPL) that allows you
to test smaller parts of your program. You could for example run a single 
function in your implementation via this command.

```sh
$ stack ghci
```

Here is a [user-guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html)
for GHCi. It is not all that difficult to use, but it is useful to know some
of the commands like `:r` or `:q`. Note that running GHCi through stack already
imports the current codebase into scope. This is practically always desireable,
so we recommend running GHCi in this fashion instead of as a standalone program.

## Assignment structure

Implement all function stubs found within the `src` folder of this project.
A stub will have the following shape:

```haskell
-- | Returns true if the passed list was empty
-- e.g. null [1, 2] = False
null :: [a] -> Bool
null = undefined
```

You will have to provide an implementation for this, e.g.

```haskell
-- | Returns true if the passed list was empty
-- e.g. null [1, 2] = False
null :: [a] -> Bool
null [] = True
null _  = False
```

Notice how the comment above the stub provides you with an explanation of
what the code is supposed to do, as well as an example case. Do make sure to 
read all the comments in a file. They provide useful insights, as well as 
occasionally ask you to implement functions in a specific way. Do not change
the function signature of the stubs. This will generally make your program
unable to compile!

The automatic tests that are ran by `stack test` will check whether your
implementation is correct. These tests will also tell you in which order we 
think the assignment works best. Some tests may even depend on earlier pieces 
of your implementation in order to function. Hence, we strongly suggest to work
in this order!

## Grading

Your final grade corresponds directly to the one awarded to you by the test
infrastructure. Do make sure your submission correctly executes on our online
environment.

If there are issues with the submission system, don't panic! We will handle this
on a case-by-case basis.

If your uploaded submission somehow fail tests that work locally, ping
us and we will have a look!

If the online environment suddenly fails to work moments before the deadline,
don't hesitate to send us your submission through different means (e.g. email).

## Plagiarism

We have a strict zero tolerance policy against plagiarism. Sadly, we find cases
every year... This is not fun for you, nor us. Please, refrain from copying 
and/or sharing your code with other students.

While we generally recommend our students to work via Git, do make sure to make
your repository **private**! Sharing your code in this manner is sadly still
plagiarism, even if unintentional.
