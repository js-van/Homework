signature SCHTEST = sig
    val test: string -> (LamAST.lamtype * LamAST.subst) option;
    val getProg: string -> SchAST.program;
end

