---
description: Error type used in scheme
---

# LError

Some errors can occur evaluating LValues:

*   **WrongType**: Error that his raised when the wrong kind of LValue is received.

    ```
    >> (* 3 t)
    LI>> error: In LValue::mul, t: Got Symbol, expected Number
    ```
*   **NotInListOfExpectedTypes**: Same as wrong type, but several types could have been ok.

    ```
    >> (length 10)
    LI>> error: In length, 10: Got int, expected [List, Map]
    ```
*   **WrongNumberOfArgument**: The number of arguments is not in the expected range

    ```
    >> (- 10 25 3)
    LI>> error: In -, "(10 25 3)": Got 3 element(s), expected 2
    ```
* **UndefinedSymbol**: The symbol as no binding in the environment.
* **SpecialError**: Special error with formatting string in function of the context. Mainly used for internal errors. Special Errors should not occur and raise a bigger programming problem that needs to be adressed.
* **ConversionError**: Conversion Error when transforming a LValue to its inner type (Internal Error). Should not occur if functions are safe and type verifications are done.
