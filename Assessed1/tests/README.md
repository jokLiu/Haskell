To run the marking script:

* Copy your solution to `./inputs/Assessed1Part2.hs`
* Ensure that your solution's module name is `Assessed1Part2`
* `$> cd inputs`
* `$> make`

This will produce output that says which tests you passed, and which you
failed, along with a (usually unhelpful) explanation of why it failed.

If it fails to run, one of several things could have gone wrong:
 1. Your solution fails to compile. Ensure your solution compiles by itself
 2. You don't have the correct module name. (See above)
 3. Your file is named incorrectly. (See above)
 4. Your solution doesn't export one of the functions you were asked to complete;
   usually, this is because you commented out a partial solution that doesn't work.
   Replace the function definition in your file with

       exercise_name = undefined

 5. You changed the type of a function you were asked to define. This will require
   more direct intervention. Or you can just comment that out, and replace it with

       exercise_name :: --The type we gave you goes here!
       exercise_name = undefined

When marking, I fixed errors of types 2,3, and 4 to ensure your code could be tested.

-----

The output for each test will look something like:

      Test 4: tabulate (7 points)
        [testing] test_tabulate... *** Failed!
      Input: ()
      Output: False
      Expected output: True

      Points given: 0

The unhelpful input/output combo is the result of some messy hacking I had to do
to get the tests running. So, you'll have to check what the test is actually
doing. The word after "[testing]" is the name of a function in `TestHelpers.hs`.
In the example above, this is `test_tabulate`.

Each test in `TestHelpers.hs` is a function of type `() -> Bool` which is
probably an equality check.

You can manually check each test in ghci, by opening ghci with `TestHelpers.hs`
If you also open `TestHelpers.hs` in your terminal, you can see what each test
is checking, and print out inputs to see where you lost points (if you did).

Note that your "expected mark" is out of 100, not out of 25. So divide by 4 for
your actual mark.
