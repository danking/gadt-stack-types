Using GADTs to Prevent Stack Underflow
======================================

If you compose these state transitions in reverse order (the last shift/reduction should appear first), then the type of the resulting function represents how the stack will change.

I'll use the following grammar as recurring example. I've written it in `bison` style, so if you'd like to take a look at the resulting PDA as a graph send it to `bison` with the argument `-r all`.

    %token PLUS STAR INT
    %%

    exp : exp PLUS term
        | term

    term : term STAR INT
        | INT

Now that you understand the state machine, imagine sending it through a series of shift transitions and reductions. Let's take a closer look at the result of the sequence:

    s1-->s8--r4-->s1-->s5-->s6-->s7--r3-->s1-->s2-->s3-->s8--r4-->s3-->s4--r1-->s1

Any input of the form `# * # + #` would follow exactly this route through the PDA's graph. This is what a function composition representing this entire computation looks like:

    r1 . s3tos4 . r4 . s3tos8 . s2tos3 . s1tos2 . r3 . s6tos7 . s5tos6 . s1tos5 . r4 . s1tos8

It's reversed because the syntactically last procedure is applied first. If we ask GHCi for the type of this composition by prepending the previous line with `:t` we'll find that it is

    :: S1 -> Stack ()

But S1 is really just `Stack ()`, the empty stack. As you can see the type system has figured out exactly what the stack should look like at the end of this computation. If any state shifted to a state with a stack that the destination cannot possibly handle, we get a static type error. Furthermore, no reduction can be applied to an invalid stack because the type checker would similarly deduce the error. This is how we statically prevent stack underflow.

Another, simpler, example that I should write more about.

    :t r3 . s6tos7 . s5tos6 . s1tos5 . r4 . s1tos8