
# blackfish

**Video**: 
https://user-images.githubusercontent.com/3711047/113652026-0ed89080-9661-11eb-8f6c-5f488897b5df.mp4

# Description

Blackfish is an experimental lazy pure functional visual music programming langauge designed for children. (That's a lot of adjectives!)

1) Experimental: New features are constantly being added, and it is not currently capable of doing anything useful

1) Lazy: Blackfish is lazily evaluated, because I think lazy evaluation is more intuitive for people who have never programmed before

1) Pure: All functions in blackfish are pure. This means they only return a value, rather than directly causing "side effects" from their invokation. How is that useful? 
The way Blackfish programs effect the world is by returning a special value from the `main` function, which is automatically invoked by the Blackfish runtime. 
Depending on the value yoyu return, a different side effect may be triggered.

1) Functional: Blackfish has a focus on functions and functional ways of doing things, although most things you would expect from a functional langauge these days (higher
order functions, strong type systems, the ability to actually write your own functions besides the main function, etc.) are not yet present. But one day they will be!

1) Visual: Blackfish is a visual language! As you can see in the image above, blackfish programs don't look like anything you've likely seen before. Instead, you write programs
by arranging blocks. Blocks input and output red lasers (and sometimes do other things). By connecting the inputs and the outputs just right, you can set it up so the laser
that leaves the blackfish world (at the bottom right) has the value you'd like it to have.

1) Music: Every blackfish program is a song! Not a very good song, maybe, but a song. Blackfish programs take an input representing the current time, in the bottom left. They 
do some manipulation of that input and output a value on the bottom right. That value tells Blackfish what piano key to press. The program pictured above is a program that just
plays every key starting from 0 (the lowest key) to 87 (the highest key). The blue block intercepting the laser is the `id` function, which does nothing except output its input. 
The blue block above it isn't connected to any input and so does nothing.

1) Designed for children: Every feature in blackfish has been chosen to have minimal complexity and minimal time-to-first-smile. The goal is to create a system that encourages 
children to learn to produce music by exploiting their natural desire to make music. Since learning more programming concepts will allow them to create more intricate songs,
they'll be intrinsically motivate to do so. At least, that's the plan!

