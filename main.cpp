#include <iostream>
#include "codegen.h"
#include "node.h"

using namespace std;

extern int yyparse();
extern NBlock* programBlock;

void createCoreFunctions(CodeGenContext& context);

int main(int argc, char **argv)
{
	yyparse();
	cout << programBlock << endl;
    // see http://comments.gmane.org/gmane.comp.compilers.llvm.devel/33877
	InitializeNativeTarget();
    cout<<"OK1"<<endl;
	InitializeNativeTargetAsmPrinter();
    cout<<"OK2"<<endl;
	InitializeNativeTargetAsmParser();
    cout<<"OK3"<<endl;
	CodeGenContext context;
	createCoreFunctions(context);
    cout<<"OK4"<<endl;
	context.generateCode(*programBlock);
    cout<<"OK5"<<endl;
	context.runCode();
    cout<<"OK6"<<endl;
	return 0;
}

