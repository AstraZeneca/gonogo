# How to contribute

This package is part of an ongoing research project. You are encouraged to
contact the 
maintainer first before raising issues or submitting patches.

## Coding conventions

We aim to follow the [Tidyverse style guide][tidy], with some modifications.

### Data representation 

We use S3 objects.

### Naming conventions

We follow the following naming

- The basic class constructor is named in `UpperCamelCase`.
  That is also the name of the class.
- Helper methods to build elements of the class start with "make_" 
  followed by the class name in `lower_case_separated_with_underscores`.
- Variable names are `lower.case.separated.with.periods`.

The rationale is:

- Names with periods look nice
- However, they can be confused with S3 methods when used in function names.
  Following the [Tidyverse style guide][tidy], we use underscores in function names.
- We want to distinguish between the constructor for class (e.g. `Protocol`) 
  and a variable containing an object of the class (`protocol`). Therefore,
  imitating the [Google R style guide][google], we name classes starting
  with a capital letter.
  
The last point is unconventional. However, because the package user 
will likely prefer to use the helper methods to build
objects, instead of the class constructors directly, their experience
will be affected very little by this choice.
