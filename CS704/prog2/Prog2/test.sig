signature SCHTEST = sig
    val test: SchAST.interpreter -> string -> SchAST.constant list -> SchAST.constant;
    val getProg: string -> SchAST.program;
end
