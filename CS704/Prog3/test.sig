signature SCHTEST = sig
    val test: string -> SchBTA.bindtime list -> SchAST.constant list -> 
	      (SchBTA.division * SchPE.programa * SchAST.program * SchAST.constant * SchAST.constant);
    val getProg: string -> SchAST.program;
end
