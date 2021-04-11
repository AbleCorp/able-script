pub enum TOKENS {
    LEFT_PARENTHESIS,                              // (
    RIGHT_PARENTHESIS,                             // )
    LEFT_BRACKET,                                  // [
    RIGHT_BRACKET,                                 // ]
    LEFT_BRACE,                                    // {
    RIGHT_BRACE,                                   // }
    COMMENT { value: String },                     // #
    SUBTRACT,                                      // -
    ADDITION,                                      // +
    MULTIPLY,                                      // *
    DIVIDE,                                        // /
    CHAR,                                          // Base52 based character
    FUNCTION,                                      // functio
    BF_FUNCTION { name: String, functio: String }, // Brain fuck FFI
    VARIABLE,                                      // Variable bro
    BOOLEAN { state: bool },                       // True, False
    ABOOLEAN { state: u8 },                        // Always, Sometimes, Never
    PRINT,                                         // Prints the preceding things
    MELO, // Ban the following variable from ever being used again
    T_DARK,
}
pub enum ABOOL {
    NEVER = -1,
    SOMETIMES = 0,
    ALWAYS = 1,
}
