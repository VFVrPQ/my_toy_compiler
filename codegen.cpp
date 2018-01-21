#include "node.h"
#include "codegen.h"
#include "parser.hpp"
#include <set>

using namespace std;

#define ISTYPE(value, id) (value->getType()->getTypeID() == id)
static Type *typeOf(CodeGenContext& context,const NIdentifier& type) ;

int flag = 0;
int mainFunctionNum = 0;
Function* myMainFunction;
/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root)
{
	std::cout << "Generating code...\n";
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(llvmContext), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "bigmain", module);
	BasicBlock *bblock = BasicBlock::Create(llvmContext, "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */ //递归
	ReturnInst::Create(llvmContext, bblock); // 代码生成
	popBlock();
	
	/* Print the bytecode in a human-readable format 
	   to see if our program compiled properly
	 */
	std::cout << "Code is generated.\n";
	// module->dump();

	legacy::PassManager pm;
	pm.add(createPrintModulePass(outs()));
	pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	if (mainFunctionNum == 0){
		cout<< "ERROR: no main fun" <<endl;
		exit(0);
	}else 
	if (mainFunctionNum != 1){
		cout<< "ERROR: too many main fun" <<endl;
		exit(0);
	}

	std::cout << "Running code...\n";
	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	ee->finalizeObject();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(myMainFunction, noargs);
	std::cout << "Code was run.\n";
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(CodeGenContext& context,const NIdentifier& type) 
{
	if (type.name.compare("int") == 0) {
		return Type::getInt64Ty(context.llvmContext);
	}
	else if (type.name.compare("double") == 0) {
		return Type::getDoubleTy(context.llvmContext);
	}
	return Type::getVoidTy(context.llvmContext);
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
	std::cout << "Creating integer: " << value << endl;
	return ConstantInt::get(Type::getInt64Ty(context.llvmContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
	std::cout << "Creating double: " << value << endl;
	return ConstantFP::get(Type::getDoubleTy(context.llvmContext), value);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
	std::cout << "Creating identifier reference: " << name << endl;
	if (context.locals().find(name) == context.locals().end()) {
		std::cerr << "undeclared variable " << name << endl;
		return NULL;
	}
	return new LoadInst(context.locals()[name], "", false, context.currentBlock());
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cerr << "no such function " << id.name << endl;
	}
	std::vector<Value*> args;
	ExpressionList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		args.push_back((**it).codeGen(context));
	}
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
	std::cout << "Creating method call: " << id.name << endl;
	return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
	std::cout << "Creating binary operation " << op << endl;
	
	Instruction::BinaryOps instr;
	CmpInst::Predicate com;
	switch (op) {
		case TPLUS: 	instr = Instruction::Add; goto math;
		case TMINUS: 	instr = Instruction::Sub; goto math;
		case TMUL: 		instr = Instruction::Mul; goto math;
		case TDIV: 		instr = Instruction::SDiv; goto math;	
		/* TODO comparison */
		/*
		"=="				          	return TOKEN(TCEQ);
		"!="			          		return TOKEN(TCNE);
		"<"				          		return TOKEN(TCLT);
		"<="	          				return TOKEN(TCLE);
		">"				          		return TOKEN(TCGT);
		">="					        return TOKEN(TCGE);
		*/
		case TCEQ:		com = CmpInst::ICMP_EQ; goto compare;
		case TCNE:		com = CmpInst::ICMP_NE; goto compare;
		case TCLT:		com = CmpInst::ICMP_SLT;goto compare;
		case TCLE:		com = CmpInst::ICMP_SLE;goto compare;
		case TCGT:		com = CmpInst::ICMP_SGT;goto compare;
		case TCGE:		com = CmpInst::ICMP_SGE;goto compare;
	}
	return NULL;
math:
	return BinaryOperator::Create(instr, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
compare:
//Create (OtherOps Op, Predicate predicate, Value *S1, Value *S2, const Twine &Name, BasicBlock *InsertAtEnd)
//ICmpInst (BasicBlock &InsertAtEnd, Predicate pred, Value *LHS, Value *RHS, const Twine &NameStr="")
	return CmpInst::Create(*(new Instruction::OtherOps), com, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
	std::cout << "Creating assignment for " << lhs.name << endl;
	if (context.locals().find(lhs.name) == context.locals().end()) {
		std::cerr << endl << "ERROR: undeclared variable " << lhs.name << endl;
		exit(0);
		return NULL;
	}
	return new StoreInst(rhs.codeGen(context), context.locals()[lhs.name], false, context.currentBlock());
}

set<std::string> uniq;
Value* NBlock::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;

	uniq.clear() ; 

	for (it = statements.begin(); it != statements.end(); it++) {
		std::cout << "Generating code for " << typeid(**it).name() << endl;	
		last = (**it).codeGen(context);
	}
	std::cout << "Creating block" << endl;
	return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating code for " << typeid(expression).name() << endl;
	return expression.codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating return code for " << typeid(expression).name() << endl;
	Value *returnValue = expression.codeGen(context);
	context.setCurrentReturnValue(returnValue);
	return returnValue;
}

Value* NIterationStatement::codeGen(CodeGenContext& context)
{
	std::cout << "Generating while code for " << typeid(expression).name() << endl;

    return nullptr;
}

static Value* CastToBoolean(CodeGenContext& context, Value* condValue){

    if( ISTYPE(condValue, Type::IntegerTyID) ){
        condValue = context.builder.CreateIntCast(condValue, Type::getInt1Ty(context.llvmContext), true);
        return context.builder.CreateICmpNE(condValue, ConstantInt::get(Type::getInt1Ty(context.llvmContext), 0, true));
    }else if( ISTYPE(condValue, Type::DoubleTyID) ){
        return context.builder.CreateFCmpONE(condValue, ConstantFP::get(context.llvmContext, APFloat(0.0)));
    }else{
		std::cout<< "no castToBoolean" <<std::endl;
        return condValue;
    }
}

Value* NSelectionStatement::codeGen(CodeGenContext& context)
{	
	cout << "Generating if statement" << endl;

	Value* condValue = condition.codeGen(context);
	if (!condValue) {
		std::cout<< "No condValue" <<std::endl;
		return nullptr;
	}

	condValue = CastToBoolean(context, condValue);

	Function* theFunction = context.currentBlock()->getParent();
	BasicBlock *ifTrue = BasicBlock::Create(context.llvmContext, "then", theFunction, 0);
	BasicBlock *ifFalse = BasicBlock::Create(context.llvmContext, "else");
	BasicBlock *mergeBB = BasicBlock::Create(context.llvmContext, "ifcont");

	Value *last = NULL;
	if (falseBlock){
		 BranchInst::Create(ifTrue, ifFalse, condValue, context.currentBlock() );
	}else{
		 BranchInst::Create(ifTrue, mergeBB, condValue, context.currentBlock() );
	}

	//context.builder.SetInsertPoint(ifTrue);
	context.pushBlock(ifTrue);
	last = trueBlock.codeGen(context);
	last = BranchInst::Create(mergeBB, context.currentBlock());
	context.popBlock();
	//std::cout<< ifTrue->getParent() << std::endl;

	if (falseBlock){
   		theFunction->getBasicBlockList().push_back(ifFalse);        //
    	context.builder.SetInsertPoint(ifFalse);
		context.pushBlock(ifFalse);
		last = falseBlock->codeGen(context);
		last = BranchInst::Create(mergeBB, context.currentBlock());
		context.popBlock();
	}

    theFunction->getBasicBlockList().push_back(mergeBB);        //
    context.builder.SetInsertPoint(mergeBB);
	ReturnInst::Create(context.llvmContext, mergeBB); //for test
	//context.pushBlock(mergeBB);

	return last;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
	if (uniq.find(id.name) == uniq.end())
		std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
	else {
		std::cout << "!!Error:same variable declaration " << id.name << endl;
		exit(0);
	}
	uniq.insert(id.name);

	AllocaInst *alloc = new AllocaInst(typeOf(context,type), id.name.c_str(), context.currentBlock());
	context.locals()[id.name] = alloc;
	if (assignmentExpr != NULL) {
		NAssignment assn(id, *assignmentExpr);
		assn.codeGen(context);
	}
	return alloc;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf(context,(**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(context,type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);

    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
	vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf(context,(**it).type));
	}
	FunctionType *ftype = FunctionType::get(typeOf(context,type), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(context.llvmContext, "entry", function, 0);

	if (id.name == "main" && !flag){
		myMainFunction = function;
		mainFunctionNum ++;
	}
	context.pushBlock(bblock);

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	uniq.clear();
	for (it = arguments.begin(); it != arguments.end(); it++) {
		(**it).codeGen(context);
		
		argumentValue = &*argsValues++;
		argumentValue->setName((*it)->id.name.c_str());
		StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
	}
	
	block.codeGen(context);
	ReturnInst::Create(context.llvmContext, context.getCurrentReturnValue(), bblock);

	context.popBlock();
	std::cout << "Creating function: " << id.name << endl;
	return function;
}
