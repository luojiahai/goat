{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9
@float = $digit+ (\. $digit+)? (E ($digit+ | (\+ | \-) $digit+) )?
$alpha = [a-zA-Z]
@ident = $alpha [$alpha $digit \_ \’]*

rules :-
    $white  ;
    begin   { \s -> KEY_BEGIN   }
    do      { \s -> KEY_DO      }
    else    { \s -> KEY_ELSE    }
    end     { \s -> KEY_END     }
    fi      { \s -> KEY_FI      }
    if      { \s -> KEY_IF      }
    od      { \s -> KEY_OD      }
    proc    { \s -> KEY_PROC    }
    read    { \s -> KEY_READ    }
    ref     { \s -> KEY_REF     }
    then    { \s -> KEY_THEN    }
    val     { \s -> KEY_VAL     }
    while   { \s -> KEY_WHILE   }
    write   { \s -> KEY_WRITE   }
    \,      { \s -> OP_COM      }
    \=      { \s -> OP_EQU      }
    \+      { \s -> OP_ADD      }
    \-      { \s -> OP_SUB      }
    \*      { \s -> OP_MUL      }
    \/      { \s -> OP_DIV      }
    \>      { \s -> OP_GRE      }
    \>\=    { \s -> OP_GRE_EQ   }
    \<      { \s -> OP_LES      }
    \<\=    { \s -> OP_LES_EQ   }
    \!      { \s -> OP_NOT      }
    \!=     { \s -> OP_NOT_EQ   }
    \:      { \s -> OP_COL      }
    \&\&    { \s -> OP_AND      }
    \|\|    { \s -> OP_IOR      }
    \(      { \s -> CTRL_LPA    }
    \)      { \s -> CTRL_RPA    }
    \[      { \s -> CTRL_LBR    }
    \]      { \s -> CTRL_RBR    }
    \;      { \s -> CTRL_SCO    }
    bool    { \s -> TYPE_BOOL   }
    float   { \s -> TYPE_FLOAT  }
    int     { \s -> TYPE_INT    }
    true    { \s -> BOOL True   }
    false   { \s -> BOOL False  }
    $digit  { \s -> INT (read s :: Int)     }
    @float  { \s -> FLOAT (read s :: Float) }
    @ident  { IDENT }

{
data Token
    = BEGIN | DO | ELSE | END | FI | IF | OD 
    | PROC | READ | REF | THEN | VAL | WHILE | WRITE 
    | COM | EQU | ADD | SUB | MUL | DIV | GRE | GRE_EQ | LES | LES_EQ | NOT | NOT_EQ
    | LPA | RPA | LBR | RBR | COL | SCO | AND | IOR
    | TYPE_BOOL | TYPE_FLOAT | TYPE_INT | BOOL Bool | INT Int | FLOAT Float
    | IDENT String
    deriving (Eq, Show)

main
    = do
        s <- getContents
        print (alexScanTokens s)
}