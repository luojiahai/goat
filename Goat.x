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
    begin   { \s -> BEGIN  }
    do      { \s -> DO     }
    else    { \s -> ELSE   }
    end     { \s -> END    }
    fi      { \s -> FI     }
    if      { \s -> IF     }
    od      { \s -> OD     }
    proc    { \s -> PROC   }
    read    { \s -> READ   }
    ref     { \s -> REF    }
    then    { \s -> THEN   }
    val     { \s -> VAL    }
    while   { \s -> WHILE  }
    write   { \s -> WRITE  }
    \,      { \s -> COM    }
    \=      { \s -> EQU    }
    \+      { \s -> ADD    }
    \-      { \s -> SUB    }
    \*      { \s -> MUL    }
    \/      { \s -> DIV    }
    \>      { \s -> GRE    }
    \>\=    { \s -> GRE_EQ }
    \<      { \s -> LES    }
    \<\=    { \s -> LES_EQ }
    \!      { \s -> NOT    }
    \!=     { \s -> NOT_EQ }
    \(      { \s -> LPA    }
    \)      { \s -> RPA    }
    \[      { \s -> LBR    }
    \]      { \s -> RBR    }
    \:      { \s -> COL    }
    \;      { \s -> SCO    }
    \&\&    { \s -> AND    }
    \|\|    { \s -> IOR    }
    bool    { \s -> TYPE_BOOL  }
    float   { \s -> TYPE_FLOAT }
    int     { \s -> TYPE_INT   }
    true    { \s -> BOOL True  }
    false   { \s -> BOOL False }
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