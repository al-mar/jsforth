# jsforth

A programming language inspired by FORTH.

The engine has tight integration with JavaScript. Its virtual memory holds JavaScript objects instead of bytes. When you type an unknown word, the interperter evaluates it with function "eval", so you can type 1+2 and get 3 on the stack. Also, the engine simulates preemptive multitasking. So you can run several virtual machines simultaneously without blocking the user interface.
