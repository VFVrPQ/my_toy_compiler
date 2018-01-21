## c-minus BNF

> **说明**
> 暂时没有数组定义
> type_specifier可能会有问题
> 可能要先去除左递归？
> stmt = expr ;
> expression ： 是表达式，有值的
> 匹配注释
```
"/*"(([^\*]*(\*[^\/])?)*)"*/" COMMENT
```

> **TOKEN**
> TSEMICOLON TCOMMA
> TINT TVOID
> TLPAREN TRPAREN TLBRACE TRBRACE
> TIF TELSE TWHILE
> TECQ TCNE TCLT TCLE TCGT TCGE
> TPLUS | TMINUS
----
> **type**
> 1. ident : ID type_specifier myvoid var
> 2. expr : NUM expression simple_expression additive_expression term factor
> 3. varvec : params param_list
> 4. exprvec : args
> 5. block : program stmts declaration_list  
> 6. stmt : stmt declaration var_declaration com_declaration fun_declaration statement expression_stmt selection_stmt iteration_stmt return_stmt param
> 7. token : comparion addop mulop

> **BNF**
```

program : declaration_list { programBlock = $1; }
;

declaration_list : declaration_list declaration { $1->statements.push_back($<stmt>2);} 
| declaration { $$ = new NBlock(); $$->statements.push_back($<stmt>1);}
;

declaration : var_declaration
    | fun_declaration 
    | com_declaration
;

var_declaration : type_specifier ID TSEMICOLON { $$ = new NVariableDeclaration($1, *$2); }
;

type_specifier : TINT { $$ = new NIdentifier($1); }
    | TVOID { $$ = new NIdentifier($1); }
    ;

fun_declaration : type_specifier ID TLPAREN params TRPAREN TLBRACE stmts TRBRACE { $$ = new NFunctionDeclaration($1, *$2, *$4, *$7); delete $4;}
    ;

params : param_list
    | myvoid { $$ = new VariableList(); $$->push_back($<param>1);}
    ;

myvoid : TVOID { $$ = new NIdentifier($1); }
    ;

param_list : param_list TCOMMA param { $$ = $1; $$->push_back($<param>3); }
    | param { $$ = new VariableList(); $$->push_back($<param>1;)}
    ;

param : type_specifier ID { $$ = new NVariableDeclaration(*$1,*$2); }
    ;

stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
    | stmts stmt { $1->statements.push_back($<stmt>2); }
    ;

stmt : var_declaration 
    | com_declaration
    | expression_stmt 
    | TLBRACE stmt TRBRACE { $$ = $2;}
    | selection_stmt
    | iteration_stmt
    | return_stmt
    | /*空*/ { $$ = new NStatement();}
    ;

expression_stmt : expression TSEMICOLON { $$ = new NExpressionStatement(*$1); }
    | TSEMICOLON { $$ = new NStatement(); }
    ;

selection_stmt : TIF TLPAREN expression TRPAREN statement { $$ = new NSelectionStatement(*$3,*$5); }
    | TIF TLPAREN expression TRPAREN statement TELSE statement { $$ = new NSelectionStatement(*$3,*$5,*$7); } 
    ;

iteration_stmt : TWHILE TLPAREN expression TRPAREN statement { $$ = new NIterationStatement(*$3,*$5); }
    ;

return_stmt : TRETURN TSEMICOLON { $$ = new NReturnStatement(); }
    | TRETURN expression TSEMICOLON {$$ = new NReturnStatement($<expression>2); }
    ;

expression : var TEQUAL expression { $$ = new NAssignment(*<ident>1,*$3); }
    | simple_expression
    ;

var : ID {$<ident>$ = $1; }
    ;

simple_expression : additive_expression comparision additive_expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
    | additive_expression
    ;

additive_expression : additive_expression addop term
{ $$ = new NBinaryOperator(*$1,$2,*$3); }
    | term
    ;

addop : TPLUS | TMINUS ; 

term : term mulop factor {$$ = NBinaryOperator(*$1,$2,*$3);}
    | factor
    ;

factor : TLPAREN expression TRPAREN { $$ = $2; }
    | var
    | ID TLPAREN args TRPAREN {  $$ = new NMethodCall(*$1, *$3); delete $3; }
    | NUM
    ;

args : args TCOMMA expression { $1->push_back($3); }
    | /*空*/ { $$ = new ExpressionList(); }
    | expression { $$ = new ExpressionList(); $$->push_back($1); }
    ;
comparion : TECQ | TCNE | TCLT | TCLE | TCGT | TCGE ;

com_declaration : COMMENT { $$ = new NBlock(); }
    ;

```



----
my
```
%%

program: declaration_list { programBlock = $1; }
	;
		
declaration_list: declaration_list declaration { $1->statements.push_back($<stmt>2); $$ = $1; } 
	| declaration { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	;

declaration: var_declaration { $$ = $1; } 
    | extern_declaration { $$ = $1; }
    | fun_declaration 
    | com_declaration
	;

var_declaration: type_specifier ident TSEMICOLON { $$ = new NVariableDeclaration(*$1, *$2); }
	;

type_specifier: TINT { $$ = new NIdentifier(*$1); }
    | TVOID { $$ = new NIdentifier(*$1); }
    ;

fun_declaration: type_specifier ident TLPAREN params TRPAREN TLBRACE stmts TRBRACE { $$ = new NFunctionDeclaration(*$1, *$2, *$4, *$7); }
    ;

extern_declaration: TEXTERN type_specifier ident TLPAREN params TRPAREN TSEMICOLON { $$ = new NExternDeclaration(*$2, *$3, *$5); }
    ;

params: param_list
    | myvoid { $$ = new VariableList(); $$->push_back($<var_decl>1);}
    ;

myvoid: TVOID { $$ = new NIdentifier(*$1); }
    ;

param_list: param_list TCOMMA param { $$ = $1; $$->push_back($<var_decl>3); }
    | param { $$ = new VariableList(); $$->push_back($<var_decl>1); }
    ;

param: type_specifier ident { $$ = new NVariableDeclaration(*$1,*$2); }
    ;

stmts: stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
    | stmts stmt { $1->statements.push_back($<stmt>2); }
	| stmts com_declaration { $$ = $1; } 
    ;

stmt: var_declaration 
    | expression_stmt 
    | TLBRACE stmt TRBRACE { $$ = $2; }
    | selection_stmt
    | iteration_stmt
    | return_stmt
    | /*空*/ { $$ = new NStatement();}
    ;

expression_stmt: expression TSEMICOLON { $$ = new NExpressionStatement(*$1); }
    | TSEMICOLON { $$ = new NStatement(); }
    ;

selection_stmt: TIF TLPAREN expression TRPAREN stmt { $$ = new NSelectionStatement(*$3,*$5); }
    | TIF TLPAREN expression TRPAREN stmt TELSE stmt { $$ = new NSelectionStatement(*$3,*$5,*$7); } 
    ;

iteration_stmt: TWHILE TLPAREN expression TRPAREN stmt { $$ = new NIterationStatement(*$3,*$5); }
    ;

return_stmt: TRETURN TSEMICOLON { $$ = new NReturnStatement(); }
    | TRETURN expression TSEMICOLON {$$ = new NReturnStatement(*$2); }
    ;

expression: ident TEQUAL expression { $$ = new NAssignment(*$1,*$3); }
    | ident TLPAREN args TRPAREN {  $$ = new NMethodCall(*$1, *$3); }
    | ident { $<ident>$ = $1; }
	| numeric
	| expression comparison expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| TLPAREN expression TRPAREN { $$ = $2; }
	;

args: args TCOMMA expression { $1->push_back($3); }
    | /*空*/ { $$ = new ExpressionList(); }
    | expression { $$ = new ExpressionList(); $$->push_back($1); }
    ;

comparison: TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE 
	| TPLUS | TMINUS | TMUL | TDIV
	;

ident: TIDENTIFIER { $$ = new NIdentifier(*$1); }
    ;

numeric: TINTEGER { $$ = new NInteger(atol($1->c_str())); }
	;

com_declaration: TCOMMENT { $$ = new NStatement(); }
    ;
%%
```


垃圾llvm，好点的文档都没有
http://www.codeweblog.com/用llvm来开发自己的编译器-一-变量和函数/

```
> LLVMContext: 线程上下文，通个getGlobalContext获得
> Module: 模块，可以看成是变量、函数和类型的集合，一般对应一个文件
> Function: 函数
> Type: 数据类型如i64、double、i64、double等
> Value: 可以看成是个变量如上面的%a、%1等
> BasicBlock: 基本块，如上面的entry,在IR里类型是label,可以看成是指令的集合，但必须以return、br等跳转类指令结束
> IRBuilder: 是一个辅助类，提供便捷的api插入指令
```

---

## 变量的处理

### 函数外声明的是全局变量，函数内声明的都是局部变量
```
全局的变量 : Value *var = new GlobalVariable(module,type,false,GlobalValue::ExternalLinkage,initial);
创建一个局部变量 : Value *var = irBuilder.CreateAlloca(type);

要用load和store来进行取值和赋值:
Value *val = irBuilder.CreateLoad(var)
irBuilder.CreateStore(val,var)

```

---

## 函数的处理
```
vector<Type*> argTypes;
argTypes.push_back(builder.getInt64Ty());
ArrayRef<Type*> argTypesRef(argTypes);
FunctionType *funcType = FunctionType::get(builder.getVoidTy(),argTypesRef,false);
Function *func = Function::Create(funcType,Function::ExternalLinkage,"funcA",&module);
```
BasicBlock *bb = BasicBlock::Create(context,"label1",func);
builder.SetInsertPoint(bb);

函数体就是BasicBlock的集合，可以用BasicBlock *bb = BasicBlock::Create(context,"label1",func)在函数体的最后创建一个BasicBlock。把IRBuilder的插入点设置成相应BasicBlock，后面用builder创建的指令都会追加到这个BasicBlock里了。



### **取出Integer的值**
```
ConstantInt::get(Type::getInt64Ty(context.llvmContext), value, true);
```
### **取出Double的值**
```
ConstantFP::get(Type::getDoubleTy(context.llvmContext), value)
```

### **取出Identifier的值:**
```
new LoadInst(context.locals()[name], "", false, context.currentBlock())
```

### **创建call**
```
CallInst::Create(function, makeArrayRef(args), "", context.currentBlock())
```

### **创建+-*/**
```
BinaryOperator::Create(Instruction::Add, lhs.codeGen(context), 
		rhs.codeGen(context), "", context.currentBlock());
```

### **定义**
```
AllocaInst *alloc = new AllocaInst(typeOf(context,type), id.name.c_str(), context.currentBlock());
```

### **创建fuzhi语句**
```
new StoreInst(rhs.codeGen(context), context.locals()[lhs.name], false, context.currentBlock())

argumentValue = &*argsValues++;
argumentValue->setName((*it)->id.name.c_str());
StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
```

### **创建return Statement**
```
Value *returnValue = expression.codeGen(context);
context.setCurrentReturnValue(returnValue);
```

### **创建externFunction**
```
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf(context,(**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(context,type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);

    return function;
```

### **创建function**
```
    vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf(context,(**it).type));
	}
	FunctionType *ftype = FunctionType::get(typeOf(context,type), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(context.llvmContext, "entry", function, 0);
    context.pushBlock(bblock);
    Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (it = arguments.begin(); it != arguments.end(); it++) {
		(**it).codeGen(context);
		
		argumentValue = &*argsValues++;
		argumentValue->setName((*it)->id.name.c_str());
		StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
	}
	
	block.codeGen(context);
	ReturnInst::Create(context.llvmContext, context.getCurrentReturnValue(), bblock);

	context.popBlock();
	
```